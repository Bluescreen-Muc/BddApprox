
import random
import numpy as np
import pygraphviz as pgv
from weakref import WeakSet, WeakValueDictionary
import sys

import time

from evaluation import Stats
from collections import defaultdict, Counter

from functions import *




class Node:
    uid = 2
    MAX_NUMBER_NODES = 10000
    def __init__(self, var=None, low=None, high=None):
        self.var = var
        self.uid = Node.uid
        self.low = low
        self.high = high
        self.iter_index = -1

    def __iter__(self):
        return self

    def __next__(self):
        self.iter_index += 1
        if self.iter_index > 1:
            self.iter_index = -1
            raise StopIteration
        return (self.low, self.high)[self.iter_index]

    def __eq__(self, other):
        if not isinstance(other, Node):
            return False
        return self.var == other.var and self.low.uid == other.low.uid and self.high.uid == other.high.uid

    def __hash__(self):
        return self.var * Node.MAX_NUMBER_NODES ** 2 + self.low.uid * Node.MAX_NUMBER_NODES + self.high.uid

    def __str__(self):
        return '({}<-{}->{}) ' \
            .format(self.low.var if self.low.var != "TERMINAL" else self.low, self.var, self.high.var
        if self.high.var != "TERMINAL" else self.high)

    @staticmethod
    def copy_node(node):
        return Node(node.var, node.low, node.high)

class Terminal(Node):
    def __init__(self, uid):
        super().__init__()
        self.uid = uid

    def __eq__(self, other):
        if not isinstance(other, Terminal):
            return False
        return self.uid == other.uid

    def __hash__(self):
        return self.uid

    def __str__(self):
        return "TRUE" if self.uid else "FALSE"

    @staticmethod
    def random_terminal(truth=0.5):
        return Bdd.TRUE if np.random.binomial(1, truth) else Bdd.FALSE

    def check_children(self, node):
        return False

class Bdd:
    TRUE = Terminal(1)
    FALSE = Terminal(0)

    def __init__(self):
        self.max_var = 0
        self.hash_pool = WeakValueDictionary()
        self.var_pool = WeakValueDictionary()
        self.hash_pool[0]= Bdd.FALSE
        self.hash_pool[1]= Bdd.TRUE
        self.var_pool = defaultdict(list)
        self.root_node = None
        self.max_var = 0
        self.info = defaultdict(NodeInfo)

    def set_max_var(self, n):
        Bdd.max_var == n

    def node(self, var, low, high, node=None):
        if node:
            node = Node(node.var, node.low, node.high)
        else:
            node = Node(var, low, high)
        if var > self.max_var:
            self.max_var = var

        if node.low == node.high:
            return low

        else:
            node_hash = hash(node)
            if node_hash in self.hash_pool:

                return self.hash_pool[node_hash]

            else:
                self.hash_pool[node_hash]=node
                if not self.var_pool[node.var]:
                    self.var_pool[node.var] = WeakSet()
                    self.var_pool[node.var].add(node)
                else:
                    self.var_pool[node.var].add(node)
                Node.uid += 1
                return node


    def delete_orphans(self):
        while True:
            children = []
            parents = self.hash_pool
            for node in parents:
                if isinstance(node, Terminal):
                    continue
                children += [node.low, node.high]
            orphans = [x for x in parents if x not in children and x != self.root_node]
            if not orphans:
                break
            else:
                [self.hash_pool.remove(x) for x in orphans]

    # def delete_node(self, node):
    #     if node.uid not in self.hash_pool:
    #         raise Exception("delete_node: node not found")
    #     del (self.hash_pool[node.uid])

    # def get_vars(self, terminal=False, from_bottom=True, max=sys.maxsize, min=-1):
    #     output = []
    #     for var, nodes in self.hash_pool.items():
    #         if not terminal and var == 0:
    #             continue
    #         if nodes:
    #             output.append(var)
    #
    #     return list(filter(lambda x: x >= min and x <= max, sorted(output, reverse=from_bottom)))

    def print_nodes(self):
        output = []
        for uid, node in self.hash_pool.items():
            output.append([uid, node])
        print("Node list sorted by uid:")
        for uid, node in output[::-1]:
            print(uid, node)
        print("\n")

    def get_vars(self, lower=-1, upper=sys.maxsize, from_bottom=True):
        output = []
        for x in range(lower,min(self.max_var, upper+1)):
            if self.var_pool[x]:
                output.append(x)
        return output[::-1] if from_bottom else output

    def count_rec(self):
        vars = self.get_vars()
        for var in vars:
            for node in self.var_pool[var]:
                for child in node:
                    if child.uid == 0:
                        self.info[node.uid].false_paths += 2**(self.max_var-var)
                    elif child.uid == 1:
                        self.info[node.uid].true_paths += 2 ** (self.max_var - var)

                    else:
                        self.info[node.uid].false_paths += (2**(child.var - node.var - 1))*self.info[child.uid].false_paths
                        self.info[node.uid].true_paths += (2**(child.var - node.var - 1))*self.info[child.uid].true_paths
        if isinstance(self.root_node, Terminal):
            self.info[self.root_node.uid].false_paths = 2**self.max_var if self.root_node == Bdd.FALSE else 0
            self.info[self.root_node.uid].true_paths = 2 ** self.max_var if self.root_node == Bdd.TRUE else 0
        else:
            root_multiplier = 2**(self.root_node.var - 1)
            self.info[self.root_node.uid].false_paths *= root_multiplier
            self.info[self.root_node.uid].true_paths *= root_multiplier



    def approximation1(self, depth=2):
        self.count_rec()
        vars = self.get_vars(upper = self.max_var-depth)
        print(vars)
        # for var in vars_to_round:
        #     for node in list(self.hash_pool[var]):
        #         if node.low == Bdd.FALSE:
        #             nodes_to_change.add(node)
        #         if node.high == Bdd.FALSE:
        #             nodes_to_change.add(node)
        # print(nodes_to_change)
        # for node in nodes_to_change:
        #     self.set_node(node, Bdd.FALSE)

    def get_nodes(self):
        return self.hash_pool.values()

    def print_info(self):
        for node, info in self.info:
            print(info)

    def draw(self, file=None):
        if not file:
            file = 'bdd.png'
        g = pgv.AGraph(strict=False, directed=True)
        g.node_attr['shape'] = 'circle'
        g.node_attr['style'] = 'filled'
        g.node_attr['colorscheme'] = 'set312'

        for node in self.get_nodes():

            g.add_node('%d' % node.uid, rank=node.var)
            new_node = g.get_node(node.uid)
            new_node.attr['fillcolor'] =  0 if isinstance(node, Terminal) else (node.var % 12 + 1)
            if node.uid in [0, 1]:
                new_node.attr['fillcolor'] = 'White'
                new_node.attr['shape'] = 'doublecircle'
                new_node.attr['label'] = ['F', 'T'][node.uid]

            else:
                label = node.var
                g.get_node(node.uid).attr['label'] = label

        for node in self.get_nodes():
            if isinstance(node, Terminal):
                continue
            g.add_edge('%d' % node.uid, '%d' % node.low.uid, style='dotted')
            g.add_edge('%d' % node.uid, '%d' % node.high.uid)
        for var in self.var_pool.keys():
            g.add_subgraph([node.uid for node in self.var_pool[var]], rank="same")
        g.draw(file, g.layout(prog='dot'))

    @staticmethod
    def apply(f, bdd1, bdd2):
        def apply_nodes(f, node1, node2, bdd):
            if isinstance(node1, Terminal) or isinstance(node2, Terminal):
                return f(node1,node2)
            if bdd.get_var(node1) < bdd.get_var(node2):
                return bdd.node(node1.var, apply_nodes(f, node1.low, node2, bdd),
                                apply_nodes(f, node1.high, node2, bdd))
            elif bdd.get_var(node1) == bdd.get_var(node2):
                return bdd.node(node1.var, apply_nodes(f, node1.low, node2.low, bdd),
                                apply_nodes(f, node1.high, node2.high, bdd))
            if bdd.get_var(node1) > bdd.get_var(node2):
                return bdd.node(node2.var, apply_nodes(f, node1, node2.low, bdd),
                                apply_nodes(f, node1, node2.high, bdd))

        bdd = Bdd()
        bdd.root_node = apply_nodes(f, bdd1.root_node, bdd2.root_node, bdd)
        return bdd

    @staticmethod
    def create_bdd(depth=10, truth_rate = 0.5):
        depth = depth - 1
        bdd = Bdd()
        array = [[[] for _ in range(depth)] for _ in range(depth)]
        for i in range(0,depth)[::-1]:
            for j in range(0,depth)[::-1]:
                if i == depth-1:
                    array[i][j] = bdd.node(i+2, Terminal.random_terminal(truth=truth_rate), Terminal.random_terminal(truth=truth_rate))
                else:
                    array[i][j] = bdd.node(i+2 ,array[i+1][np.random.randint(depth)],array[i+1][np.random.randint(depth)])

        bdd.root_node = bdd.node(1, array[0][np.random.randint(depth)],array[0][np.random.randint(depth)])
        array = None
        bdd.check_weak_references()

        return bdd

    def check_weak_references(self):
        if self.hash_pool._pending_removals:
            print("FOUND WEAK REFERENCES")
            self.hash_pool._commit_removals()

    def set_node(self, old_node, new_node):
        vars = self.get_vars(from_bottom=True, max=old_node.var - 1)
        to_do_low = set()
        to_do_high = set()
        for var in vars:
            for node in list(self.hash_pool[var]):
                if node.low == old_node:
                    to_do_low.add(node.low)

                if node.high == old_node:
                    to_do_high.add(node.high)
        print(len(to_do_high), len(to_do_low))
        for node in to_do_low:
            node = self.get_node(node)
            node.low = new_node
            self.hash_pool[node.var].add(node)
        for node in to_do_high:
            node = self.get_node(node)
            node.high = new_node
            self.hash_pool[node.var].add(node)

    def get_var(self, node):
        if isinstance(node, Terminal):
            return self.max_var + 1
        else:
            return node.var



class NodeInfo:

    def __init__(self):
        self.true_paths = 0
        self.false_paths = 0

    def __str__(self):
        return "{} {}".format(self.false_paths, self.true_paths)

if __name__ == '__main__':
    start = time.time()
    bdd = Bdd.create_bdd(1000)
    print(time.time()-start)
    print([x for x in  bdd.get_nodes()])
    bdd.draw()
    bdd.approximation1(2)

