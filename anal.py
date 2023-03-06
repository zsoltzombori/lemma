import torch
from pathlib import Path
from torch_geometric.loader import DataLoader
import matplotlib.pyplot as plt
import argparse
import numpy as np
from glob import glob
import os

from train import LemmaDataset
from features import FEATURES, TARGETS
from model import get_model

# training device
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'


def get_histograms(dataset, num_bins=100):
    targets = dataset.data.y
    # bins = torch.tensor(range(0,num_bins+1)) / num_bins
    for i in range(len(TARGETS)):
        curr_targets = targets[:,i]
        histogram = torch.histc(curr_targets, bins=num_bins, min=0, max=1)
        # print(histogram)
        plt.bar(range(num_bins), histogram, align="center")
        plt.xlabel('Bins')
        plt.ylabel(TARGETS[i])
        plt.savefig("histograms/Histogram_" + TARGETS[i] + ".png")
        plt.clf()

def get_scatter(dataset, model, args):
    os.makedirs(args.outdir, exist_ok=True)
    utilities = []
    targets = []
    model = model.to(DEVICE)
    counter = 100
    for batch in DataLoader(dataset, batch_size=args.bs, shuffle=False):
        batch = batch.to(DEVICE)
        utilities.append(model(batch).numpy(force=True))
        targets.append(batch.y.numpy(force=True))
        counter -= 1
        if counter <= 0:
            break
    utilities = np.concatenate(utilities)
    targets = np.concatenate(targets)

    for i in range(len(TARGETS)):
        curr_targets = targets[:,i]
        curr_utilities = utilities[:,i]
        plt.scatter(curr_targets, curr_utilities)
        plt.title(TARGETS[i])
        plt.xlabel("target")
        plt.ylabel("model")
        plt.savefig(args.outdir + "/Scatter_" + TARGETS[i] + ".png")
        plt.clf()

    
def get_weights(model, args):
    if args.model_type == "linear":
        weights = model.fc.weight
        index = TARGETS.index(args.utility)
        weights = weights[index].numpy(force=True)
        perm = (-np.absolute(weights)).argsort()
        for w, f in zip(weights[perm], np.array(FEATURES)[perm]):
            print(f, ": ", w)
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # locations
    parser.add_argument('--data_dir', dest="data_dir", default="datasets", help="Directory that contains training files")
    parser.add_argument('--saved_model_path', dest="saved_model_path", default="models/model0", help="Path that contains previously saved model.")
    parser.add_argument('--convolutions', dest="convolutions", default=16, type=int, help="Number of graph convolutions")
    parser.add_argument('--channels', dest="channels", default=64, type=int, help="Dimension of graph node")
    parser.add_argument('--hidden', dest="hidden", default=1024, type=int, help="Size of hidden layer")
    parser.add_argument('--bs', dest="bs", default=128, type=int, help="Batch size")
    parser.add_argument('--outdir', dest="outdir", default="scatters", help="output directory")
    parser.add_argument('--utility', dest="utility", default="u_tsize_reduction", help="Utility measure name (see features.py)")
    parser.add_argument('--model_type', dest='model_type', default="gnn", help="gnn/linear")
    parser.add_argument('--concat_linear', dest='concat_linear', type=int, default=1, help="1/0")

    args = parser.parse_args()
    files = glob(args.data_dir + '/**/*.pl', recursive=True) + glob(args.data_dir + '/**/*.pl.gz', recursive=True)
    dataset = LemmaDataset(files)

    model = get_model(args)
    model.load_state_dict(torch.load(args.saved_model_path))
    model.eval()

    # get_histograms(dataset, num_bins=100)

    get_scatter(dataset, model, args)
    get_weights(model, args)


