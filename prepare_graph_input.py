#!/usr/bin/env python3

import re
import ast
import gzip

NODE_TYPES = ["i", "d", "n", "VAR", "LHS", "CONST","AXIOM_ROOT","FROM_ROOT","TO_ROOT","GOAL_ROOT", "DTERM_ROOT", "ROOT"]

from features import FEATURES, TARGETS

# read lemma file into a dictionary
def read_file(filename):
    result = {}
    open_fn = gzip.open if filename.endswith('.gz') else lambda p: open(p, 'rb')
    with open_fn(filename) as file:
        for line in file:
            line = line.strip().decode('ascii')
            match = re.search(r'spo\(([^,]*),([^,]*),(.*)\).', line)
            if match is not None:
                subj = match.group(1).strip()
                pred = match.group(2).strip()
                obj = match.group(3).strip()
                if subj not in result:
                    result[subj] = {}
                result[subj][pred] = obj
        return result

class TermGraph():
    def __init__(self):
        self.variables = {}
        self.meta_nodes = {}
        self.cons = {}
        self.nodes = []
        self.from_list = []
        self.to_list = []

    def display(self):
        print("nodes  :", self.nodes)
        print("Edges:")
        for fromnode, tonode in zip(self.from_list, self.to_list):
            fromtype = self.nodes[fromnode]
            totype =self.nodes[tonode]
            print("   {} ({}) -> {} ({})".format(fromnode, fromtype, tonode, totype))


# parse a dc term
# note that this is not yet general purpose term parser!
class TermParser():
    def __init__(self):
        self.termGraph = TermGraph()
        self.variables = self.termGraph.variables
        self.meta_nodes = self.termGraph.meta_nodes
        self.cons = self.termGraph.cons
        self.nodes = self.termGraph.nodes
        self.from_list = self.termGraph.from_list
        self.to_list = self.termGraph.to_list

    # create a single root that is connected to all meta_nodes
    def connect_to_root(self):
        root_type = NODE_TYPES.index("ROOT")
        assert root_type not in self.nodes, "You should only create a single root"
        root_id = self.insert_node("", root_type, lookup_dict=None)
        for meta_node in self.meta_nodes:
            meta_id = self.meta_nodes[meta_node]
            self.from_list.append(root_id)
            self.to_list.append(meta_id)
        return self.termGraph


    def extend(self, term, term_name, reset_variables):
        if reset_variables:
            self.variables.clear()

        # parse the given term
        tree = ast.parse(term).body[0].value
        root_id = self.parse(tree)

        # add a link from the corresponding meta node
        term_type = NODE_TYPES.index(term_name)
        meta_id = self.insert_node(term_name, term_type, lookup_dict=self.meta_nodes)
        self.from_list.append(meta_id)
        self.to_list.append(root_id)

        return self.termGraph
        

    def insert_node(self, node, node_type, lookup_dict=None):
        if lookup_dict is None:
            node_id = len(self.nodes)
            self.nodes.append(node_type)
        elif node in lookup_dict:
            node_id = lookup_dict[node]
        else:
            node_id = len(self.nodes)
            self.nodes.append(node_type)
            lookup_dict[node] = node_id
        return node_id
        
    def parse(self, tree):
        if "id" in tree._fields: # terminal node (constant or variable)
            c = ast.unparse(tree)
            if c == "_": # variable that occurs only once
                c_type = NODE_TYPES.index("VAR")
                c_id = self.insert_node(c, c_type, lookup_dict=None)
            elif c[0].isupper(): # variable
                c_type = NODE_TYPES.index("VAR")
                c_id = self.insert_node(c, c_type, lookup_dict=self.variables)
            else: # constant
                c_type = NODE_TYPES.index("CONST")
                c_id = self.insert_node(c, c_type, lookup_dict=self.cons)
            return c_id
        elif "value" in tree._fields: # constant number #TODO BETTER UNDERSTAND THIS
            c = ast.unparse(tree)
            c_type = NODE_TYPES.index("CONST")
            c_id = self.insert_node(c, c_type, lookup_dict=self.cons)
            return c_id
        elif "func" in tree._fields: # inner node
            func = tree.func
            func = ast.unparse(func)
            func_type = NODE_TYPES.index(func)
            func_id = self.insert_node(func, func_type, lookup_dict=None)
         
            args = tree.args
            args = [self.parse(a) for a in args]

            if len(args) == 1: # unary function
                child_id = args[0]
                self.from_list.append(func_id)
                self.to_list.append(child_id)
            elif len(args) == 2: # add three edges to the two children with an extra "LHS" node
                left_id = args[0]
                right_id = args[1]                 
                lhs_type = NODE_TYPES.index("LHS")
                lhs_id = self.insert_node("", lhs_type, lookup_dict=None)
                self.from_list += [func_id, func_id, lhs_id]
                self.to_list += [lhs_id, right_id, left_id]
            else:
                assert False, "I am expecting unary or binary functions in: " + ast.unparse(tree)
            return func_id
        
        else:
            print(tree)
            print(tree._fields)
            print(ast.unparse(tree))
            assert False, "Unknown node"
            
        


def term2list(term):
    tree = ast.parse(term).body[0].value
    if "id" in tree._fields: # atom
        return [term]
    if "elts" in tree._fields: # list
        return [ast.unparse(a) for a in tree.elts]
    else:
        head = ast.unparse(tree.func) # function
        args = [ast.unparse(a) for a in tree.args]
        return [head] + args

def build_graphs(data_dict, get_graph=True):
    graph_dict = {}
    for subject in data_dict:
        if data_dict[subject]["type"] == "lemma":
            termParser = TermParser()

            if get_graph:
                formula = data_dict[subject]["formula"]
                terms = term2list(formula)
                fromterm = terms[1]
                toterms = term2list(terms[2])
                termParser.extend(fromterm, "FROM_ROOT", reset_variables=False)
                for toterm in toterms:
                    termParser.extend(toterm, "TO_ROOT", reset_variables=False)


                if "proof" in data_dict[subject]:
                    proof = data_dict[subject]["proof"]    
                    problem = data_dict[proof]["problem"]
                else:
                    problem = data_dict[subject]["problem"]

                # add graphs for all axioms
                num_axioms = int(data_dict[problem]["number_of_axioms"])
                for i in range(1, num_axioms+1):
                    axiom = data_dict[problem]["axiom({})".format(i)]
                    termParser.extend(axiom, "AXIOM_ROOT", reset_variables=True)

                # add graph for goal
                goal = data_dict[problem]["goal"]
                termParser.extend(goal, "GOAL_ROOT", reset_variables=True)

                dterm = data_dict[subject]["dterm"]
                termParser.extend(dterm, "DTERM_ROOT", reset_variables=True)

            graph = termParser.connect_to_root()

            # extract features
            features = []
            for f in FEATURES:
                if f in data_dict[subject]:
                    features.append(float(data_dict[subject][f]))
                else:
                    features.append(0)
            targets = []
            for t in TARGETS:
                if t in data_dict[subject]:
                    targets.append(float(data_dict[subject][t]))
                else:
                    targets.append(-1)

            graph_dict[subject] = (graph, features, targets)
    return graph_dict


if __name__ == '__main__':
    # filename = "training_data_example_draft.pl"
    # filename = "training_data_01/training_data_pdb_short.pl"
    filename = "out/exp2/0/train/LCL006-1.pl"
    data_dict = read_file(filename)
    graph_dict = build_graphs(data_dict)

    for lemma in graph_dict:
        graph, features, targets = graph_dict[lemma]

        print("\n\n----------------")
        print("Lemma: ", lemma)
        print("Features:\n", features)
        print("Targets", targets)
        graph.display()
