import os
import torch
from pathlib import Path
from torch_geometric.loader import DataLoader
import argparse
import numpy as np
import re
import time

from features import TARGETS
from prepare_graph_input import NODE_TYPES, read_file, build_graphs
from train import LemmaDataset
from model import get_model

# training device
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'
# DEVICE= 'cpu'

def tofile_prolog(utilities, lemma_ids, args, data_dict):
    f = open(args.outfile, "w")    
    f.write("% utility measure: {}, lemma_count: {}\n".format(args.utility, args.lemma_count))
    
    # data_dict = read_file(args.lemma_file)
    for utility, lemma_id  in zip(utilities, lemma_ids):
        dterm = data_dict[lemma_id]["dterm"]
        formula = data_dict[lemma_id]["formula"]
        utility = int(utility * 100) / 100
        line = "lemma({},{},{},{}).\n".format(lemma_id, utility, formula, dterm)
        f.write(line)
    f.close()

def tofile_dterm(utilities, lemma_ids, args, data_dict):
    f = open(args.outfile, "w")    
    f.write("% utility measure: {}, lemma_count: {}\n".format(args.utility, args.lemma_count))
    
    # data_dict = read_file(args.lemma_file)
    for utility, lemma_id  in zip(utilities, lemma_ids):
        dcterm = data_dict[lemma_id]["dcterm"]
        line = "{}.\n".format(dcterm)
        f.write(line)
    f.close()


def tofile_tptp(utilities, lemma_ids, args, data_dict):
    f = open(args.outfile, "w")
    f.write("% utility measure: {}, lemma_count: {}\n".format(args.utility, args.lemma_count))
    
    # data_dict = read_file(args.lemma_file)
    for utility, lemma_id  in zip(utilities, lemma_ids):
        formula = data_dict[lemma_id]["formula"]
        match = re.search(r'lemma\((.*),\[(.*)\]\)', formula)
        head = match.group(1).strip()
        body = match.group(2).strip()

        variable_list = re.findall(r'[A-Z]\w*', formula)
        variable_list = ",".join(set(variable_list))

        axiom_name = "ax_{}".format(int(utility * 100) / 100)
        
        if body == "":
            tptp_formula = "thm({})".format(head)            
        else:
            body = body.split(",")
            body = ["~ thm({}) ".format(b) for b in body]
            body = " | ".join(body)
            tptp_formula = "{} | thm({})".format(body, head)

        axiom = "fof({}, axiom, ![{}]: ({})).".format(axiom_name, variable_list, tptp_formula)
        f.write(axiom + "\n")
    f.close()

def smaller_utility_better(utility):
    if utility in ["lf_is_in_proof", "u_reproof"]:
        return False
    else:
        return True

        
def evaluate(args):
    if not os.path.exists(args.lemma_file):
        f = open(args.outfile, "w")    
        f.write("% utility measure: {}, lemma_count: {}\n".format(args.utility, args.lemma_count))
        f.close()
        return


    if args.model_type == "linear":
        get_graph=False
    else:
        get_graph=True
        
    T0 = time.time()
    dataset = LemmaDataset([args.lemma_file], save_data_dict=True, get_graph=get_graph)
    T1 = time.time()

    model = get_model(args)
    if DEVICE == 'cpu':
        map_location=torch.device('cpu')
    else:
        map_location=torch.device('cuda:0')
        
    model.load_state_dict(torch.load(args.saved_model_path, map_location=map_location))
    model.eval()
    model = model.to(DEVICE)

    utilities = []
    lemma_ids = []
    for batch in DataLoader(dataset, batch_size=args.bs, shuffle=False):
        batch = batch.to(DEVICE)
        ut = model(batch).numpy(force=True)
        utilities.append(ut)
        lemma_ids.append(batch.lemma_id)

    utilities = np.concatenate(utilities)
    lemma_ids = np.concatenate(lemma_ids)
    utilities = utilities[:, TARGETS.index(args.utility)]
    T2 = time.time()

    if smaller_utility_better(args.utility):
        perm = np.argsort(utilities)
    else:
        perm = np.argsort(-utilities)
    utilities = utilities[perm][:args.lemma_count]
    lemma_ids = lemma_ids[perm][:args.lemma_count]
    T3 = time.time()

    if args.out_type == "prolog":
        tofile_prolog(utilities, lemma_ids, args, dataset.data_dicts[0])
    elif args.out_type == "tptp":
        tofile_tptp(utilities, lemma_ids, args, dataset.data_dicts[0])
    elif args.out_type == "dterm":
        tofile_dterm(utilities, lemma_ids, args, dataset.data_dicts[0])
    else:
        assert False, "Unknown out_type: " + args.out_type
    T4 = time.time()
    # print("Data loading: {} sec, model evaluation: {} sec, sorting: {} sec, data saving: {} sec".format(T1-T0, T2-T1, T3-T2, T4-T3))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # locations
    parser.add_argument('--saved_model_path', dest="saved_model_path", default=None, help="Path that contains previously saved model.")
    parser.add_argument('--bs', dest="bs", default=128, type=int, help="Batch size")
    parser.add_argument('--lemma_file', dest="lemma_file", default=None, help="File that holds candidate lemmas.")
    parser.add_argument('--utility', dest="utility", default="u_tsize_reduction", help="Utility measure name (see features.py)")
    parser.add_argument('--lemma_count', dest="lemma_count", default=100, type=int, help="Number of lemmas to keep")
    parser.add_argument('--outfile', dest="outfile", default=None, help="Output file.")
    parser.add_argument('--out_type', dest="out_type", default="prolog", help="prolog/tptp/dterm.")
    parser.add_argument('--convolutions', dest="convolutions", default=16, type=int, help="Number of graph convolutions")
    parser.add_argument('--channels', dest="channels", default=64, type=int, help="Dimension of graph node")
    parser.add_argument('--hidden', dest="hidden", default=1024, type=int, help="Size of hidden layer")
    parser.add_argument('--model_type', dest='model_type', default="gnn", help="gnn/linear")
    parser.add_argument('--concat_linear', dest='concat_linear', type=int, default=1, help="1/0")

    args = parser.parse_args()
    T0 = time.time()
    evaluate(args)
    T1 = time.time()
    # print("Elapsed time: {} sec".format(T1-T0))
