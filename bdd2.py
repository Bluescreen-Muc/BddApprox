from collections import defaultdict
from enum import Enum

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
            raise StopIteration
        return (self.low, self.high)[self.iter_index]

    def __eq__(self, other):
        return self.var == other.var and self.low.uid == other.low.uid and self.high.uid == other.high.uid

    def __hash__(self):
        return self.var * Node.MAX_NUMBER_NODES**2 + self.low.uid * Node.MAX_NUMBER_NODES + self.high.uid

    def __str__(self):
        return '({}<-{}->{})'.format(self.low.var, self.var, self.high.var)

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



    def node(self, var,low,high):
        node = Node(var,low,high)
        if node.low.uid == node.high.uid:
            self.node_pool[node.low.uid] = node.low
            return self.node_pool[node.low.uid]
        else:
            if var not in self.var_order:
                self.var_order.insert(0, var)
            if node not in self.node_pool.values():
                self.node_pool[node.uid]=node
                Node.uid += 1

            return self.node_pool[node.uid]

    def print_nodes(self):
        print("Node list sorted by uid:")
        for uid, node in self.node_pool.items():
            print(uid, node)
        print("\n")

    def print_var_order(self):
        print("Variable Order")
        for id, var in enumerate(self.var_order,1):
            print("{}. {}".format(id,var))
        print("\n")
if __name__ == '__main__':
    bdd = Bdd()
    bdd.node(0,bdd.node(1,Terminal(1), Terminal(0)), bdd.node(1,Terminal(1), Terminal(1)))
    bdd.print_nodes()
    bdd.print_var_order()
