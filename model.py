import torch
from torch.nn import Module, ModuleList, Embedding, Linear, functional as F
from torch_geometric.nn import BatchNorm, GCNConv, global_mean_pool

from features import FEATURES, TARGETS
from prepare_graph_input import NODE_TYPES

class Residual(Module):
    def __init__(self, channels):
        super().__init__()
        self.bn1 = BatchNorm(channels)
        self.bn2 = BatchNorm(channels)
        self.conv1 = GCNConv(channels, channels)
        self.conv2 = GCNConv(channels, channels)

    def forward(self, x, edge_index):
        before = x
        x = self.bn1(x)
        x = self.conv1(x, edge_index)
        x = F.relu(x)
        x = self.bn2(x)
        x = self.conv2(x, edge_index)
        x = F.relu(x)
        return x + before

# residual GCN model, global mean pooling
class Model(Module):
    def __init__(self, convolutions, channels, hidden, concat_linear):
        super().__init__()
        self.target_size = len(TARGETS)
        self.embedding = Embedding(len(NODE_TYPES), channels)
        self.residual = ModuleList([Residual(channels) for _ in range(convolutions)])
        self.fc = Linear(hidden, self.target_size)
        self.concat_linear=bool(concat_linear)
        if self.concat_linear:
            feature_vector_size = len(FEATURES)
            self.hidden = Linear(channels + feature_vector_size, hidden)
        else:
            self.hidden = Linear(channels, hidden)

    def forward(self, batch):
        x = batch.x
        edge_index = batch.edge_index
        features = batch.features
        batch = batch.batch
        x = self.embedding(x)
        for residual in self.residual:
            x = residual(x, edge_index)
        x = global_mean_pool(x, batch=batch)
        if self.concat_linear:
            x = torch.cat((x, features), dim=1)
        x = self.hidden(x)
        x = F.relu(x)
        x = self.fc(x)
        # x = torch.sigmoid(x) # the targets are all numbers in [0,1]
        return x

class LinearModel(Module):
    def __init__(self):
        super().__init__()
        self.feature_vector_size = len(FEATURES)
        self.target_size = len(TARGETS)
        self.fc = Linear(self.feature_vector_size, self.target_size)

    def forward(self, batch):
        features = batch.features
        x = self.fc(features)
        # x = torch.sigmoid(x) # the targets are all numbers in [0,1]
        return x

def get_model(args):
    if args.model_type == "gnn":
        return Model(convolutions=args.convolutions, channels=args.channels, hidden=args.hidden, concat_linear=args.concat_linear)
    elif args.model_type =="linear":
        return LinearModel()
    else:
        assert False, "Unknown model type:" + args.model_type
