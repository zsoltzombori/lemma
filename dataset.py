from typing import List

import torch
from torch.utils.data import Dataset
from torch_geometric.utils import to_undirected
from torch_geometric.data import Data

from features import FEATURES, TARGETS
from prepare_graph_input import read_file, build_graphs

class LemmaDataset(Dataset):
    """
    A `Dataset` object representing our lemma dataset.
    In future this could be larger than CPU memory
    See https://pytorch-geometric.readthedocs.io/en/latest/notes/create_dataset.html
    """
    def __init__(self, files: List[str], max_lemmas=10000000, save_data_dict=False, get_graph=True):
        lemma_count = 0
        self.data = []
        self.data_dicts = []
        for filename in files:
            data_dict = read_file(filename)
            if save_data_dict:
                self.data_dicts.append(data_dict)
            graph_dict = build_graphs(data_dict, get_graph)
            for lemma in graph_dict:
                graph, features, targets = graph_dict[lemma]

                x = torch.tensor(graph.nodes)
                edge_index = torch.tensor([graph.from_list, graph.to_list])
                edge_index = to_undirected(edge_index)
                assert isinstance(edge_index, torch.Tensor)

                # TODO some kind of whitening/embedding?
                features = torch.tensor([features], dtype=torch.float)
                y = torch.tensor([targets], dtype=torch.float)
                self.data.append(Data(
                    x=x,
                    edge_index=edge_index,
                    features=features,
                    y=y,
                    lemma_id=lemma,
                ))
                lemma_count += 1
                if lemma_count >= max_lemmas:
                    return

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        return self.data[idx]

    @property
    def feature_vector_size(self):
        return len(FEATURES)

    @property
    def target_size(self):
        return len(TARGETS)
