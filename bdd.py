import random

import numpy as np
import pygraphviz as pgv
import time

from functions import *
class Node:

    uid = 2
    MAX_NUMBER_NODES = 10000

    def __init__(self, var=None, low=None, high=None):
        self.var = var
        self.parents = 0
        self.uid = Node.uid
        self.low = low
        self.high = high
        self.iter_index = -1
        self.true_paths = 0
        self.false_paths = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.iter_index += 1
        if self.iter_index > 1:
            raise StopIteration
        return (self.low, self.high)[self.iter_index]

    def __eq__(self, other):
        return self.var == other.var and self.low.uid == other.low.uid and self.high.uid == other.high.uid

    def __hash__(self):
        return self.var * Node.MAX_NUMBER_NODES**2 + self.low.uid * Node.MAX_NUMBER_NODES + self.high.uid

    def __str__(self):
        return '({}<-{}->{}) True: {}   False: {}   Parents: {}'\
            .format(self.low.var if self.low.var != "TERMINAL" else self.low, self.var, self.high.var
        if self.high.var != "TERMINAL" else self.high, self.true_paths, self.false_paths, self.parents)

    @staticmethod
    def copy_node(node):
        return Node(node.var, node.low, node.high)

class Terminal(Node):

    def __init__(self, terminal):
        self.var = "TERMINAL"
        self.uid = 1 if terminal == True else 0

    def __eq__(self, other):
        return self.uid == other.uid

    def __hash__(self):
        return self.uid

    def __str__(self):
        return "TRUE" if self.uid else "FALSE"


class Bdd:

    def __init__(self):
        self.var_order = ["TERMINAL"]
        self.node_pool = dict()
        self.node_pool[0] = Terminal(0)
        self.node_pool[1] = Terminal(1)


    def get_var_index(self,var):
        return self.var_order.index(var)

    def node(self, var,low,high):
        node = Node(var,low,high)
        if node.low.uid == node.high.uid:
            return self.node_pool[low.uid]
        else:
            if var not in self.var_order:
                self.var_order.insert(0, var)
            if node not in self.node_pool.values():
                node.parents = 1
                self.node_pool[node.uid]=node
                Node.uid += 1
            else:
                self.get_node(node).parents += 1

            node = [x for x in self.node_pool.values() if x == node][0]
        node.false_paths, node.true_paths = self.count_paths(node)
        return node

    def get_node(self, node):
        for k,v in self.node_pool.items():
            if v == node:
                return v
        else:
            raise Exception("Node not found")


    def get_root_node(self):
        max_uid = max(self.node_pool.keys())
        return self.node_pool[max_uid]

    def get_nodes(self, key=None, terminal=False, reverse=False):
        output = self.node_pool.values()
        if not terminal:
            output= [x for x in output if x and not isinstance(x, Terminal)]

        return sorted(output, key = key,reverse=reverse) if key else output


    def print_nodes(self):
        output = []
        for uid, node in self.node_pool.items():
            output.append([uid, node])
        print("Node list sorted by uid:")
        for uid, node in output[::-1]:
            print(uid,node)
        print("\n")

    def print_var_order(self):
        print("Variable Order")
        for id, var in enumerate(self.var_order,1):
            print("{}. {}".format(id,var))
        print("\n")

    def print_info(self):
        print("Node count: {}".format(len(self.node_pool.values())))
        print("Outputs True: {}, False: {}".format(self.get_root_node().true_paths, self.get_root_node().false_paths))

    def count_paths(self, node):
        true_count = 0
        false_count = 0
        for child in (node.low, node.high):
            multiplier = 2 ** (abs(self.var_order.index(node.var) - self.var_order.index(child.var)) -1)
            if child.uid == 0:
                false_count += multiplier
                continue
            if child.uid == 1:
                true_count += multiplier
                continue

            false_count += multiplier*child.false_paths
            true_count += multiplier*child.true_paths
        return false_count, true_count

    def draw(self, file=None):
        if not file:
            file = 'bdd.png'
        g = pgv.AGraph(strict=False, directed=True)
        g.node_attr['shape'] = 'circle'
        g.node_attr['style'] = 'filled'
        g.node_attr['colorscheme'] = 'set312'

        for node in self.get_nodes(key=lambda x: x.uid, terminal=True):
            g.add_node('%d' % node.uid)
            new_node = g.get_node(node.uid)
            new_node.attr['fillcolor'] = (self.get_var_index(node.var)%12) + 1
            if node.uid in [0,1]:
                new_node.attr['fillcolor'] = 'White'
                new_node.attr['shape'] = 'doublecircle'
                new_node.attr['label'] = ['F','T'][node.uid]

            else:
                g.get_node(node.uid).attr['label'] = node.var




        for node in self.get_nodes(key=lambda x: x.uid):
            if isinstance(node, Terminal):
                continue
            g.add_edge('%d' % node.uid, '%d' % node.low.uid,style = 'dotted')
            g.add_edge('%d' % node.uid, '%d' % node.high.uid)





        g.draw(file, g.layout(prog='dot'))
        positions = {x.uid : g.get_node(x.uid).attr['pos'] for x in self.get_nodes(terminal=True)}
        print(positions)

    @staticmethod
    def apply(f, bdd1, bdd2):
        if bdd1.var_order != bdd2.var_order:
            raise Exception ("Different Variable Orders")
        def apply_nodes(f, node1, node2, bdd):
            for node in [node1, node2]:
                if node.uid in [0,1]:
                    node.low = node.high = bdd.node_pool[node.uid]
            if isinstance(node1, Terminal) and isinstance(node2, Terminal):
                return bdd.node_pool[int(f(bool(node1.uid), bool(node2.uid)))]
            var_index_node1 = bdd1.get_var_index(node1.var)
            var_index_node2 = bdd2.get_var_index(node2.var)
            if var_index_node1 < var_index_node2:
                return bdd.node(node1.var, apply_nodes(f, node1.low,node2, bdd), apply_nodes(f, node1.high,node2,bdd))
            elif var_index_node1 == var_index_node2:
                return bdd.node(node1.var, apply_nodes(f, node1.low,node2.low,bdd),  apply_nodes(f,node1.high,node2.high, bdd))
            if var_index_node1 > var_index_node2:
                return bdd.node(node2.var, apply_nodes(f,node1,node2.low, bdd), apply_nodes(f,node1,node2.high,bdd))

        bdd = Bdd()
        node1 = bdd1.get_root_node()
        node2 = bdd2.get_root_node()
        apply_nodes(f, node1, node2, bdd)
        return bdd


    @staticmethod
    def check_bdd(depth=7, nodes=1000):
        for _ in range(nodes):
            bdd = Bdd.create_random_bdd_recursive(depth=depth)
            true_count = 0
            false_count = 0
            bits = depth
            for x in range(2**bits):
                node = bdd.get_root_node()
                x = format(x, '0{}b'.format(bits))
                for level, path in enumerate(x):
                    if bdd.var_order.index(node.var) > level:
                        continue
                    if path == '0':
                        node = node.low
                    if path == '1':
                        node = node.high
                    if node.uid == 1:
                        true_count += 1
                        break
                    elif node.uid == 0:
                        false_count += 1
                        break
            assert((true_count, false_count)==(bdd.get_root_node().true_paths, bdd.get_root_node().false_paths))
            return True


    @staticmethod
    def create_random_bdd_recursive(bdd=None, depth=3, concentration = 0.8, truth_ratio = 0.5):
        if not bdd:
            bdd = Bdd()
            bdd.var_order = [x+1 for x in range(depth)]+["TERMINAL"]
            Bdd.create_random_bdd_recursive(bdd, depth, concentration, truth_ratio)
            bdd.get_root_node().true_paths *= 2**bdd.var_order.index(bdd.get_root_node().var)
            bdd.get_root_node().false_paths *= 2 ** bdd.var_order.index(bdd.get_root_node().var)
            return bdd
        recursion = lambda: Bdd.create_random_bdd_recursive(bdd, depth-1, concentration, truth_ratio)
        if depth == 0:
            return Terminal(1) if np.random.binomial(1,truth_ratio) else Terminal(0)


        else:
            if np.random.binomial(1, concentration):
                return bdd.node(len(bdd.var_order)-depth, recursion(),recursion())

            else:
                return recursion()

    @staticmethod
    def create_bdd(depth=10, number_of_bdd=1):
        bdd = Bdd()
        bdd.var_order = [x + 1 for x in range(depth)] + ["TERMINAL"]
        info=[]
        info.append(list(np.random.randint(depth,size=number_of_bdd*2)))
        for level in range(depth-1):
            info.append([random.randint(0,depth-1) for _ in range(len(set(info[level])) * 2)])
        info.append([random.randint(0, 1) for _ in range(len(set(info[depth-1])) * 2)])
        print(info)
        for level in range(depth - 1):
            nodes = [Node(level, None, None) for _ in set(info[level])]
        for level in range(depth)[::-1]:
            tmp=[]
            counter = -2
            for node in info[level]:
                if node in tmp:
                    continue
                tmp.append(node)
                counter += 2
                if level == depth -1:
                    bdd.node(level+1, bdd.node_pool[info[level+1][counter]], bdd.node_pool[info[level+1][counter+1]])
                    print(Node(level+1, bdd.node_pool[info[level+1][counter]], bdd.node_pool[info[level+1][counter+1]]))
                    continue


if __name__ == '__main__':
    bdd = Bdd()
    #print(Bdd.check_bdd(depth=10, nodes=10000))
    bdd = Bdd.create_random_bdd_recursive(depth=5)
    bdd.draw("bdd1.png")
    bdd2 = Bdd.create_random_bdd_recursive(depth=5)
    bdd2.draw("bdd2.png")
    bdd3 = Bdd.apply(IMP, bdd, bdd2)
    bdd3.draw("bdd3.png")
    start = time.time()
    Bdd.create_bdd(depth=1000)
    print(time.time()-start)

