from enum import Enum

from collections import defaultdict
import numpy as np

class Node:

    bdd = {"var_order" : list(), "node_pool": defaultdict(set)}

    uid = 2
    MAX_NUMBER_NODES = 10000
    def __init__(self, var, low, high):
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
            raise StopIteration
        return (self.low, self.high)[self.iter_index]

    def __eq__(self, other):
        if not type(self) == type(other):
            return False
        return self.low.uid == other.low.uid and self.high.uid == other.high.uid and self.var == other.var

    def __hash__(self):
        return self.var * Node.MAX_NUMBER_NODES**2 + self.low.uid * Node.MAX_NUMBER_NODES + self.high.uid

    def __str__(self):
        return '({}<-{}->{})'.format(self.low.var, self.var, self.high.var)

    @staticmethod
    def add_node_to_pool(node):
        Node.update_var_order(node.var)
        if node not in Node.bdd['node_pool'][node.var]:
            Node.uid += 1
        Node.bdd['node_pool'][node.var].add(node)

    @staticmethod
    def add_node(var, low, high):
        if low.uid == high.uid:
            if isinstance(low, Node):
                Node.add_node_to_pool(low)

            return low
        else:
            node = Node(var, low, high)
            Node.add_node_to_pool(node)
            return node

    @staticmethod
    def update_var_order(var):
        if var not in Node.bdd["var_order"]:
            Node.bdd["var_order"].insert(0, var)



class Bdd(Node):

    def __init__(self, root_node):
        self.root_node = root_node
        #Node.add_node(root_node.var, root_node.low, root_node.high)
        self.var_order = Node.bdd["var_order"]
        self.nodes = Node.bdd["node_pool"]



    def count_sat_from_bottom(self):
        count = 0
        tmp = set()
        for x in self.var_order[::-1]:
            nodes_with_edge_to_true = {x for x in self.nodes[self.var_order[x]] if x.low.uid == 1 or x.high.uid == 1}
            count += len(nodes_with_edge_to_true)
            tmp = tmp.union(nodes_with_edge_to_true)
            for n in self.nodes[x]:
                for child in (n.low,n.high):
                    if child in tmp:
                        count += 2 ** (self.var_order.index(child.var)-self.var_order.index(n.var) - 1)
                        tmp = tmp.union(n)
        return count

    def print_var_order(self):
        [print(x) for x in self.var_order]

    def print_nodes(self):
        for x,y in self.nodes.items():
            print(x)
            for node in y:
                print(node)

    def get_true_nodes(self):
        pass


    @staticmethod
    def create_random_bdd(depth = 10, concentration = 0.8, truth_ratio = 0.5, var_order = None):
        if not var_order:
            var_order = [x+1 for x in range(depth)]

        nodes = [Node(var_order[0], None, None)]
        root_node = nodes[0]
        tmp = list()
        tmp_pool = defaultdict(set)
        while nodes:
            for node in nodes:

                if not node.low:
                    for var in var_order[var_order.index(node.var)+1::]:
                        if np.random.binomial(1, concentration):
                            node.low = Node(var, None, None)
                            tmp.append(node.low)
                            break
                    else:
                        node.low = Node.TRUE if np.random.binomial(1,truth_ratio) else Node.FALSE

                if not node.high:
                    for var in var_order[var_order.index(node.var)+ 1::]:

                        if np.random.binomial(1, concentration):
                            node.high = Node(var, None, None)
                            tmp.append(node.high)
                            break
                    else:
                        node.high = Node.TRUE if np.random.binomial(1, truth_ratio) else Node.FALSE
                tmp_pool[node.var].add(node)
            if tmp:
                nodes = tmp
                tmp = list()
            else:
                nodes = []
        for var in var_order[::-1]:
            if var in tmp_pool:
                for node in tmp_pool[var]:
                    Node.add_node(node.var, node.low, node.high)
        # for var in list(Node.bdd["node_pool"].keys())[::-1]:
        #     Node.bdd["node_pool"][var] = {node.low if node.low == node.high else node for node in Node.bdd["node_pool"][var]}
        # for var in Node.bdd["node_pool"].keys():
        #     Node.bdd["node_pool"][var].discard(Node.TRUE)
        #     Node.bdd["node_pool"][var].discard(Node.FALSE)
        return root_node







node = lambda v,l,h: Node.add_node(v, l, h)
if __name__ == '__main__':
    #bdd = Bdd(node(0, node (2, node (3, Node.TRUE, Node.FALSE), node(3, Node.TRUE, Node.FALSE)),node(1, Node.TRUE, Node.FALSE)))
    # bdd.print_var_order()
    #bdd.print_nodes()

    bdd = Bdd(Bdd.create_random_bdd())

    bdd.print_nodes()
