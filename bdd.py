from enum import Enum

class Terminal:

    def __init__(self, uid):
        self.uid = uid
        self.var = "TRUE" if uid == 1 else "FALSE"
        self.low = None
        self.high = None

    def __str__(self):
        return "TRUE" if self.uid == 1 else "FALSE"

    def __eq__(self, other):
        return self.uid == other.uid

class Node:
    TRUE = Terminal(1)
    FALSE = Terminal(0)

    bdd = {"var_order" : list(), "node_pool": set()}

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
        Node.bdd['node_pool'].add(node)

    @staticmethod
    def make_node(var, low, high):
        if low.uid == high.uid:
            Node.add_node_to_pool(low)
            return low
        else:
            if low == high:
                Node.uid += 1
            Node.update_var_order(var)
            Node.add_node_to_pool(Node(var, low, high))
            return Node(var, low, high)

    @staticmethod
    def update_var_order(var):
        if var not in Node.bdd["var_order"]:
            Node.bdd["var_order"].insert(0, var)



class Bdd(Node):

    def __init__(self, root_node):
        self.root_node = root_node
        Node.make_node(root_node.var, root_node.low, root_node.high)
        self.var_order = Node.bdd["var_order"]
        self.nodes = Node.bdd["node_pool"]



    def count_sat_brute(self, node):
        count = 0
        for x in node:
            if x.uid == 1:
                count += 1
            elif x.uid == 0:
                pass
            else:
                count += self.count_sat_brute(x)
        return count

    def print_var_order(self):
        [print(x) for x in self.var_order]

    def print_nodes(self):
        [print(x) for x in self.nodes]

    def get_true_nodes(self):
        tmp = [x for x in self.var_order[-1] if ]

        for x in self.var_order[::-1]:





node = lambda v,l,h: Node.make_node(v, l, h)
if __name__ == '__main__':
    bdd = Bdd(Node(0, node (1, Node.TRUE, Node.FALSE), Node.FALSE))
    # bdd.print_var_order()
    bdd.print_nodes()
    print(bdd.count_sat_brute(bdd.root_node))
    print(bdd.get_true_nodes())
