
import random
import numpy as np
import pygraphviz as pgv
from weakref import WeakSet
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
    def __init__(self, terminal):
        super().__init__()
        self.var = "TERMINAL"
        self.uid = 1 if terminal == True else 0

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
        return Terminal(1) if np.random.binomial(1, truth) else Terminal(0)

    def check_children(self, node):
        return False

class Bdd:
    TRUE = Terminal(1)
    FALSE = Terminal(0)

    def __init__(self):
        self.max_var = 0
        self.node_pool = defaultdict(WeakSet)
        self.node_pool[0].add(Bdd.FALSE)
        self.node_pool[0].add(Bdd.TRUE)
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
            if node in self.node_pool[node.var]:

                return {node}.intersection(self.node_pool[node.var]).pop()

            else:
                self.node_pool[node.var].add(node)
                Node.uid += 1
                return node

    def get_node(self, node):
        if node in self.node_pool[node.var]:
            return {node}.intersection(self.node_pool[node.var]).pop()
        else:
            return None

    def delete_orphans(self):
        while True:
            children = []
            parents = self.node_pool
            for node in parents:
                if isinstance(node, Terminal):
                    continue
                children += [node.low, node.high]
            orphans = [x for x in parents if x not in children and x != self.root_node]
            if not orphans:
                break
            else:
                [self.node_pool.remove(x) for x in orphans]

    # def delete_node(self, node):
    #     if node.uid not in self.node_pool:
    #         raise Exception("delete_node: node not found")
    #     del (self.node_pool[node.uid])

    def get_vars(self, terminal=False, from_bottom=True, max=sys.maxsize, min=-1):
        output = []
        for var, nodes in self.node_pool.items():
            if not terminal and var == 0:
                continue
            if nodes:
                output.append(var)

        return list(filter(lambda x: x >= min and x <= max, sorted(output, reverse=from_bottom)))

    def print_nodes(self):
        output = []
        for uid, node in self.node_pool.items():
            output.append([uid, node])
        print("Node list sorted by uid:")
        for uid, node in output[::-1]:
            print(uid, node)
        print("\n")



    def count_rec(self):
        var_order = self.get_vars()
        for var in var_order:
            for node in self.node_pool[var]:
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
        print(self.max_var)
        vars_to_round = self.get_vars(terminal=False, from_bottom=True, min=self.max_var-depth+1, max=self.max_var)
        print(vars_to_round)
        for var in vars_to_round:
            for node in self.node_pool[var]:
                if node.low == Bdd.FALSE or node.high == Bdd.FALSE:
                    self.set_node(node, Bdd.FALSE)

    def check_duplicates(self):
        for node in self.get_nodes():
            if isinstance(node, Terminal):
                continue
            if not isinstance(node.low, Terminal) and node.low not in self.node_pool[node.low.var]:
                print("feeffe")
                print(node)

            if not isinstance(node.high, Terminal) and node.high not in self.node_pool[node.high.var]:
                print("feeffe")

    def get_nodes(self):
        output = set()
        for x in self.node_pool.values():
            output.update(x)
        return output

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
        for var, nodes in self.node_pool.items():
            g.add_subgraph([node.uid for node in nodes], rank="same")
        g.draw(file, g.layout(prog='dot'))

    # @staticmethod
    # def apply(f, bdd1, bdd2):
    #     if bdd1.var_order != bdd2.var_order:
    #         raise Exception("Different Variable Orders")
    #
    #     def apply_nodes(f, node1, node2, bdd):
    #         for node in [node1, node2]:
    #             if node.uid in [0, 1]:
    #                 node.low = node.high = bdd.node_pool[node.uid]
    #         if isinstance(node1, Terminal) and isinstance(node2, Terminal):
    #             return bdd.node_pool[int(f(bool(node1.uid), bool(node2.uid)))]
    #         var_index_node1 = bdd1.get_var_index(node1.var)
    #         var_index_node2 = bdd2.get_var_index(node2.var)
    #         if var_index_node1 < var_index_node2:
    #             return bdd.node(node1.var, apply_nodes(f, node1.low, node2, bdd),
    #                             apply_nodes(f, node1.high, node2, bdd))
    #         elif var_index_node1 == var_index_node2:
    #             return bdd.node(node1.var, apply_nodes(f, node1.low, node2.low, bdd),
    #                             apply_nodes(f, node1.high, node2.high, bdd))
    #         if var_index_node1 > var_index_node2:
    #             return bdd.node(node2.var, apply_nodes(f, node1, node2.low, bdd),
    #                             apply_nodes(f, node1, node2.high, bdd))
    #
    #     bdd = Bdd()
    #     bdd.var_order = bdd1.var_order[::]
    #     bdd.root_node = apply_nodes(f, bdd1.root_node, bdd2.root_node, bdd)
    #     return bdd
    #
    #
    #
    # @staticmethod
    # def create_random_bdd_recursive(bdd=None, depth=10, concentration=0.8, truth_ratio=0.1):
    #     if not bdd:
    #         bdd = Bdd()
    #         bdd.var_order = [x + 1 for x in range(depth)] + ["TERMINAL"]
    #         bdd.root_node = Bdd.create_random_bdd_recursive(bdd, depth, concentration, truth_ratio)
    #         return bdd
    #     recursion = lambda: Bdd.create_random_bdd_recursive(bdd, depth - 1, concentration, truth_ratio)
    #     if depth == 0:
    #         return Terminal(1) if np.random.binomial(1, truth_ratio) else Terminal(0)
    #
    #
    #     else:
    #         if np.random.binomial(1, concentration):
    #             return bdd.node(len(bdd.var_order) - depth, recursion(), recursion())
    #
    #         else:
    #             return recursion()
    #
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
        for x in self.node_pool.values():
            if x._pending_removals:
                print("FOUND WEAK REFERENCES")
                x._commit_removals()
    #
    # def set_node_true(self, del_node):
    #     if del_node == self.root_node:
    #         del_node.low = Bdd.TRUE
    #         del_node.high = Bdd.TRUE
    #         return
    #     if isinstance(del_node, Terminal):
    #         return
    #     if del_node not in self.node_pool.values():
    #         return
    #
    #     del(self.node_pool[del_node.uid])
    #
    #     for node in self.get_nodes(lambda x: x.uid):
    #         if node.low.uid == del_node.uid:
    #             node.low = Bdd.TRUE
    #         if node.high.uid == del_node.uid:
    #             node.high = Bdd.TRUE
    #         if node.low == node.high:
    #             #print(node)
    #             self.set_node_true(node)
    #
    def set_node(self, old_node, new_node):
        vars = self.get_vars(terminal=False, from_bottom=True, max=old_node.var - 1)
        to_do = []
        for var in vars:
            for node in self.node_pool[var]:
                tmp_node = Node.copy_node(node)
                if node.low == old_node:
                    # tmp_node = self.get_node(Node(node.var, new_node, node.high))
                    # if tmp_node:
                    #     self.set_node(node, tmp_node)
                    # else:
                    node.low = new_node
                if node.high == old_node:
                    # tmp_node = self.get_node(Node(node.var, node.low, new_node))
                    # if tmp_node:
                    #     self.set_node(node, tmp_node)
                    # else:
                    node.high = new_node
                if node.low == node.high:
                    if node == self.root_node:
                        self.root_node = node.low
                    else:
                        to_do.append(node)
            # for x in to_do:
            #     self.set_node(x, x.low)
        return to_do


    # def check_duplicates(self):
    #     while True:
    #         change = False
    #         hashes = defaultdict(list)
    #         for node in self.get_nodes():
    #             hashes[hash(node)].append(node.uid)
    #
    #         for key in hashes.keys():
    #             if len(hashes[key]) > 1:
    #                 change = True
    #                 node = hashes[key][0]
    #                 for faulty_node in hashes[key][1::]:
    #                     if faulty_node in self.node_pool and node in self.node_pool:
    #                         self.set_node(self.node_pool[faulty_node], self.node_pool[node])
    #         if not change:
    #             break
    #
    # def apprrox(self):
    #     def score(low,high):
    #         if low == Bdd.FALSE and high == Bdd.FALSE:
    #             return 0
    #         if low == Bdd.FALSE and high == Bdd.TRUE:
    #             return 1
    #         if low == Bdd.TRUE and high == Bdd.FALSE:
    #             return -1
    #         return 2
    #
    #     info = defaultdict(list)
    #     results = dict()
    #     for node in self.get_nodes():
    #         info[node.var].append(node)
    #     for id, var in enumerate(self.get_vars(reverse=True)):
    #         for node in info[var]:
    #             low = node.low
    #             high = node.high
    #             if isinstance(low, Terminal):
    #                 if isinstance(high, Terminal):
    #                     node.score.append(score(low,high))
    #                 else:
    #                     node.score.append(0, high.score) / 2
    #             else:
    #                 node.score = (2,low.score)
    #
    #
    # @staticmethod
    # def test_rounding(depth, rounding_depth, truth_rate, n):
    #     for _ in range(n):
    #         bdd = Bdd.create_bdd(depth, truth_rate)
    #         nodes = len(list(bdd.node_pool.values()))
    #         counter = bdd.count_rec()
    #         stats = Stats(nodes, counter)
    #
    #         bdd.rounding(rounding_depth)
    #         print('frrr')
    #         nodes = len(list(bdd.node_pool.values()))
    #         counter = bdd.count_rec()
    #         print(stats.approximation(nodes, counter))
    #
    #
    #

class NodeInfo:

    def __init__(self):
        self.true_paths = 0
        self.false_paths = 0

    def __str__(self):
        return "{} {}".format(self.false_paths, self.true_paths)

if __name__ == '__main__':
    while True:
        start = time.time()
        bdd = Bdd.create_bdd(15)
        #bdd.check_duplicates()
        bdd.check_weak_references()
        bdd.draw()
        bdd.approximation1(1)
        bdd.check_duplicates()
        bdd.draw('bdd1.png')
        print(bdd.get_vars(terminal=False, from_bottom=True, min=2))
        # print(bdd.info[bdd.root_node.uid])
        #print(time.time() - start)
        #bdd.delete_orphans()
        break
        max = 100
        counter = 0
        l = []
        for node in bdd.get_nodes():
            if isinstance(node, Terminal):
                continue
            if node.var < max:
                l = [node]
                max = node.var
                counter = 1
            elif node.var == max:
                counter += 1
                l.append(node)
        if counter > 1:
            print ("fef")
            [print(x) for x in l]

            break
    # seed = 12
    # Bdd.test_rounding(30, 1,0.8,10)
