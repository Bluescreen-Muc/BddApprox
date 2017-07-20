
import random
import numpy as np
import pygraphviz as pgv
from weakref import WeakSet

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
        self.true_paths = 0
        self.false_paths = 0

    def check_children(self, node):
        return node.uid == self.low.uid or node.uid == self.high.uid

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
        return '({}<-{}->{}) True: {}   False: {}' \
            .format(self.low.var if self.low.var != "TERMINAL" else self.low, self.var, self.high.var
        if self.high.var != "TERMINAL" else self.high, self.true_paths, self.false_paths)

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
        self.var_order = ["TERMINAL"]
        self.node_pool = set()
        self.node_pool.add (Bdd.FALSE)
        self.node_pool.add (Bdd.TRUE)
        self.root_node = None
        self.max_var = 0

    def set_max_var(self, n):
        Bdd.max_var == n

    def node(self, var, low, high, node=None):
        if node:
            node = Node(node.var, node.low, node.high)
        else:
            node = Node(var, low, high)
        if var > self.max_var:
            self.max_var = var

        if node.low.uid == node.high.uid:
            return low

        else:
            self.node_pool.add(node)
            Node.uid += 1
            return node

    def delete_orphans(self):
        while True:
            children = []
            parents = self.node_pool
            print(parents)
            for node in parents:
                if isinstance(node, Terminal):
                    continue
                children += [node.low, node.high]
            orphans = [x for x in parents if x not in children and x != self.root_node]
            if not orphans:
                break
            else:
                [self.node_pool.remove(x) for x in orphans]

    def delete_node(self, node):
        if node.uid not in self.node_pool:
            raise Exception("delete_node: node not found")
        del (self.node_pool[node.uid])

    def get_node(self, get_node):
        for node in self.node_pool:
            if get_node == node:
                return node
        else:
            raise Exception("Node not found")

    def get_nodes(self, key=None, terminal=False, reverse=False):
        output = self.node_pool.values()
        if not terminal:
            output = [x for x in output if x and not isinstance(x, Terminal)]
        return sorted(output, key=key, reverse=reverse) if key else output

    def print_nodes(self):
        output = []
        for uid, node in self.node_pool.items():
            output.append([uid, node])
        print("Node list sorted by uid:")
        for uid, node in output[::-1]:
            print(uid, node)
        print("\n")

    def print_var_order(self):
        print("Variable Order")
        for id, var in enumerate(self.var_order, 1):
            print("{}. {}".format(id, var))
        print("\n")

    def print_info(self):
        print("Node count: {}".format(len(self.node_pool.values())))
        print("Outputs True: {}, False: {}".format(self.get_root_node().true_paths, self.get_root_node().false_paths))

    # def count_rec(self):
    #     info = {n.uid: [0,0] for n in self.get_nodes()}
    #     for id,level in enumerate(self.var_order[::-1]):
    #         if level == "TERMINAL":
    #             continue
    #         for node in filter(lambda x: x.var == level, self.get_nodes()):
    #             for child in node:
    #                 if child.uid == 0:
    #                     info[node.uid][0] += 2**(id-1)
    #                     continue
    #                 if child.uid == 1:
    #                     info[node.uid][1] += 2 ** (id - 1)
    #                     continue
    #
    #                 else:
    #                     info[node.uid][0] += (2**(self.get_var_index(child.var)- self.get_var_index(node.var)-1)) * info[child.uid][0]
    #                     info[node.uid][1] += (2 ** (self.get_var_index(child.var) - self.get_var_index(node.var) - 1)) * \
    #                                          info[child.uid][1]
    #
    #     info[self.root_node.uid][0] *= 2 ** (self.get_var_index(self.root_node.var))
    #     info[self.root_node.uid][1] *= 2 ** (self.get_var_index(self.root_node.var))
    #
    #     # output = [sum(info[x.uid][0] for x in self.root_nodes), sum(info[x.uid][1] for x in self.root_nodes)]
    #     return info[self.root_node.uid]
    #
    #
    # def rounding(self, depth=1):
    #     var_order = self.get_vars(reverse=True)
    #     info = self.count_rec()
    #     nodes = defaultdict(list)
    #     for node in self.get_nodes():
    #         nodes[node.var].append(node.uid)
    #
    #     for var in var_order[:depth]:
    #         for uid in nodes[var]:
    #             if uid in self.node_pool:
    #                 if self.node_pool[uid].low.uid == 0 or self.node_pool[uid].high.uid == 0:
    #                     self.set_node_true(self.node_pool[uid])
    #                 else:
    #                     self.set_node_true(info[self.node_pool[uid].low.uid][1]) if info[self.node_pool[uid].low.uid][1] < info[self.node_pool[uid].high.uid][1] else self.set_node_true(info[self.node_pool[uid].high.uid][1])
    #     self.check_duplicates()
    #
    #
    # def get_number_of_nodes(self):
    #     return len(list(self.node_pool.values()))
    #
    # def get_vars(self, reverse = False, Terminal=False):
    #     output = self.var_order[::]
    #     if not Terminal:
    #         output.remove("TERMINAL")
    #     return output[::-1] if reverse else output
    #
    def draw(self, file=None):
        if not file:
            file = 'bdd.png'
        g = pgv.AGraph(strict=False, directed=True)
        g.node_attr['shape'] = 'circle'
        g.node_attr['style'] = 'filled'
        g.node_attr['colorscheme'] = 'set312'

        for node in self.node_pool:

            g.add_node('%d' % node.uid)
            new_node = g.get_node(node.uid)
            new_node.attr['fillcolor'] =  1
            if node.uid in [0, 1]:
                new_node.attr['fillcolor'] = 'White'
                new_node.attr['shape'] = 'doublecircle'
                new_node.attr['label'] = ['F', 'T'][node.uid]

            else:
                label = node.var
                g.get_node(node.uid).attr['label'] = label

        for node in self.node_pool:
            if isinstance(node, Terminal):
                continue
            g.add_edge('%d' % node.uid, '%d' % node.low.uid, style='dotted')
            g.add_edge('%d' % node.uid, '%d' % node.high.uid)

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
    # def count(self):
    #     bits = len(self.var_order)-1
    #
    #     true_count = 0
    #     false_count = 0
    #     for x in range(2 ** bits):
    #         x = format(x, '0{}b'.format(bits))
    #         node = self.root_node
    #
    #         for level, path in enumerate(x):
    #             if self.var_order.index(node.var) > level:
    #                 continue
    #             if path == '0':
    #                 node = node.low
    #             if path == '1':
    #                 node = node.high
    #             if node.uid == 1:
    #                 true_count += 1
    #                 break
    #             elif node.uid == 0:
    #                 false_count += 1
    #                 break
    #     return (false_count, true_count)
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
        bdd = Bdd()
        array = [[[] for _ in range(depth)] for _ in range(depth)]
        for i in range(0,depth)[::-1]:
            for j in range(depth)[::-1]:
                if i == depth-1:
                    array[i][j] = bdd.node(i+2, Terminal.random_terminal(truth=truth_rate), Terminal.random_terminal())
                else:
                    array[i][j] = bdd.node(i+2 ,array[i+1][np.random.randint(depth)],array[i+1][np.random.randint(depth)])
        bdd.root_node = bdd.node(1, array[0][np.random.randint(depth)],array[0][np.random.randint(depth)])

        bdd.delete_orphans()
        return bdd
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
    # def set_node(self, old_node, new_node):
    #     uid_new = new_node.uid
    #     uid_old = old_node.uid
    #
    #     if isinstance(new_node, Terminal):
    #         return
    #     if new_node not in self.node_pool.values():
    #         return
    #
    #     self.node_pool.pop(old_node.uid,0)
    #
    #     for node in self.get_nodes(lambda x: x.uid):
    #         if node.low.uid == uid_old:
    #             node.low = self.node_pool[uid_new]
    #         if node.high.uid == uid_old:
    #             node.high = self.node_pool[uid_new]
    #         if node.low == node.high:
    #             #print(node)
    #             self.set_node(node, new_node)
    #
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


if __name__ == '__main__':
    pass
    bdd = Bdd.create_bdd(100)

    for x in bdd.node_pool:
        print(x.var)
    # seed = 12
    # Bdd.test_rounding(30, 1,0.8,10)
