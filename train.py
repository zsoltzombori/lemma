#!/usr/bin/env python3

from glob import glob
import random

import numpy as np
import torch
from torch.nn import functional as F
from torch.optim import Adam, SGD, Adamax, RMSprop
from torch.utils.tensorboard.writer import SummaryWriter

from torch_geometric.loader import DataLoader

from features import TARGETS
from dataset import LemmaDataset
from model import get_model
import argparse

SEED=111
np.random.seed(SEED)
torch.manual_seed(SEED)
torch.cuda.manual_seed_all(SEED)

# training device
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'

# pred is bs * targets
# y is bs * targets
def eval_order(pred, y):
    bs = len(pred)
    predt = pred.transpose(0,1)
    pred1 = predt.unsqueeze(1).repeat_interleave(bs, dim=1)
    pred2 = predt.unsqueeze(2).repeat_interleave(bs, dim=2)
    pred_first_greater = torch.tril(pred1 > pred2).sum(dim=[1,2])

    yt = y.transpose(0,1)
    y1 = yt.unsqueeze(1).repeat_interleave(yt.shape[1], dim=1)
    y2 = yt.unsqueeze(2).repeat_interleave(yt.shape[1], dim=2)
    y_first_greater = torch.tril(y1 > y2).sum(dim=[1,2])

    correct_order = (pred1 > pred2) == (y1 > y2)
    correct_order = correct_order.sum(dim=[1,2])
    correct_order = (correct_order - bs) / 2
    result = correct_order / (bs * (bs-1) / 2.0) # normalise to [0,1]

    return result

def ranking_loss(pred, y):
    label = y.unsqueeze(0) > y.unsqueeze(1)
    loss = torch.maximum(torch.tensor(0.0), -1 * label * (pred.unsqueeze(0) - pred.unsqueeze(1))+0.1)
    loss = loss.sum(dim=(0,1))
    loss = loss.sum()
    return loss

def get_loss(pred, y, utility):
    if utility == "all":
        return F.mse_loss(pred, y)
    else:
        assert utility in TARGETS, "Utility" + utility + "must be in TARGETS"
        index = TARGETS.index(utility)
        pred_curr = pred[:, index]
        y_curr = y[:, index]
        if utility == "lf_is_in_proof":
            return F.binary_cross_entropy_with_logits(pred_curr, y_curr)
        else:
            return F.mse_loss(pred_curr, y_curr)

def validate(model, dataset, epoch, writer, batch_size, utility):
    with torch.no_grad():
        model.eval()
        losses = []
        targetwise_losses = []
        targetwise_correct_orders = []
        for batch in DataLoader(dataset, batch_size=batch_size, shuffle=True):
            batch = batch.to(DEVICE)
            y = model(batch)
            # loss = F.mse_loss(y, batch.y)
            # loss = ranking_loss(y, batch.y)
            loss = get_loss(y, batch.y, utility)
            targetwise_loss = torch.sqrt((torch.square(y - batch.y).mean(dim=0)))
            targetwise_correct_order = eval_order(y, batch.y)
            losses.append(loss)
            targetwise_losses.append(targetwise_loss)
            targetwise_correct_orders.append(targetwise_correct_order)

        loss = torch.mean(torch.stack(losses))
        targetwise_loss = torch.mean(torch.stack(targetwise_losses), dim=0)
        targetwise_correct_orders = torch.mean(torch.stack(targetwise_correct_orders), dim=0)

        loss = loss.detach()
        writer.add_scalar('loss/valid', loss, global_step=epoch)
        for l, o, t in zip(targetwise_loss, targetwise_correct_orders, TARGETS):
            writer.add_scalar('{}/valid'.format(t), l.detach(), global_step=epoch)
            writer.add_scalar('order_{}/valid'.format(t), o.detach(), global_step=epoch)

    return loss

def train(args):
    files = glob(args.data_dir + '/**/*.pl', recursive=True) + glob(args.data_dir + '/**/*.pl.gz', recursive=True)
    if args.data_dir_extra is not None:
        files_extra = glob(args.data_dir_extra + '/**/*.pl', recursive=True) + glob(args.data_dir_extra + '/**/*.pl.gz', recursive=True)
        files += files_extra
    num_validation = max(int(args.validation * len(files)), 1)
    random.shuffle(files)
    validation_files = files[:num_validation]
    train_files = files[num_validation:]

    train_dataset = LemmaDataset(train_files)
    valid_dataset = LemmaDataset(validation_files)
    print("Train size: ", len(train_dataset))
    print("Valid size: ", len(valid_dataset))

    model = get_model(args)
    if args.saved_model_path is not None:
        model.load_state_dict(torch.load(args.saved_model_path))
    model = model.to(DEVICE)
    model.train()
    # print(model)

    optimiser = Adam(model.parameters(), lr=args.lr)
    # optimiser = SGD(model.parameters(), lr=args.lr, momentum=0.9)
    # optimiser = Adamax(model.parameters(), lr=args.lr)
    # optimiser = RMSprop(model.parameters(), lr=args.lr)
    writer = SummaryWriter()

    # print(f"training using device '{DEVICE}'...")
    step = 0
    epoch = 0
    best_validation = float('inf')
    epochs_since_improvement = 0
    while epochs_since_improvement < args.tolerance:
        # print("EPOCH: ", epoch + 1)
        validation = validate(model, valid_dataset, epoch, writer, args.bs, args.utility)
        if validation < best_validation:
            best_validation = validation
            epochs_since_improvement = 0
            # save only the best model
            if args.new_model_path is not None:
                torch.save(model.state_dict(), args.new_model_path)
        else:
            epochs_since_improvement += 1

        model.train()
        for batch in DataLoader(train_dataset, batch_size=args.bs, shuffle=True):
            batch = batch.to(DEVICE)
            y = model(batch)
            # loss = F.mse_loss(y, batch.y)
            loss = get_loss(y, batch.y, args.utility)
            loss.backward()
            optimiser.step()
            optimiser.zero_grad()
            writer.add_scalar('loss/train', loss.detach(), global_step=step)

            # plot loss for each target separately
            targetwise_loss = torch.sqrt((torch.square(y - batch.y).mean(dim=0)))
            for l, t in zip(targetwise_loss, TARGETS):
                writer.add_scalar('{}/train'.format(t), l.detach(), global_step=step)

            step += 1
        epoch += 1

if __name__ == '__main__':
    # torch.manual_seed(0)
    # random.seed(0)
    parser = argparse.ArgumentParser()
    parser.add_argument('--data_dir', dest="data_dir", default=None, help="Directory that contains files for training")
    parser.add_argument('--data_dir_extra', dest="data_dir_extra", default=None, help="Directory that contains extra files for training")
    parser.add_argument('--validation', dest="validation", type=float, default=0.05, help="Proportion (between 0 and 1) of files to use for validation")
    parser.add_argument('--saved_model_path', dest="saved_model_path", default=None, help="Path that contains previously saved model.")
    parser.add_argument('--new_model_path', dest="new_model_path", default=None, help="Path where the trained model is to be saved.")
    parser.add_argument('--bs', dest="bs", default=128, type=int, help="Batch size")
    parser.add_argument('--lr', dest="lr", default=1e-3, type=float, help="Learning rate.")
    parser.add_argument('--tolerance', dest="tolerance", default=50, type=int, help="Number of epochs permitted without improvement")
    parser.add_argument('--convolutions', dest="convolutions", default=4, type=int, help="Number of graph convolutions")
    parser.add_argument('--channels', dest="channels", default=8, type=int, help="Dimension of graph node")
    parser.add_argument('--hidden', dest="hidden", default=1024, type=int, help="Size of hidden layer")
    parser.add_argument('--utility', dest="utility", default="all", help="Utility measure name (see features.py) or all")
    parser.add_argument('--model_type', dest='model_type', default="gnn", help="gnn/linear")
    parser.add_argument('--concat_linear', dest='concat_linear', type=int, default=1, help="1/0")

    args = parser.parse_args()
    # args.data_dir_extra = "/home/zombori/terms22/experiments/pretrain/lg_tsize_optim.pl/e752cd0018839b128b0746076bd50345/"
    # print(args)
    train(args)
