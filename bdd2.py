import numpy as np

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
            return self.node_pool[low.uid]
        else:
            if var not in self.var_order:
                self.var_order.insert(0, var)
            if node not in self.node_pool.values():
                self.node_pool[node.uid]=node
                Node.uid += 1

            return [x for x in self.node_pool.values() if x == node][0]

    def get_root_node(self):
        max_uid = max(self.node_pool.keys())
        return self.node_pool[max_uid]

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

    @staticmethod
    def create_random_bdd_recursive(bdd, depth=12, concentration = 0.8, truth_ratio = 0.5):
        var = depth
        if depth == 0:
            return Terminal(1) if np.random.binomial(1,truth_ratio) else Terminal(0)


        else:
            if np.random.binomial(1, concentration):
                return bdd.node(var, Bdd.create_random_bdd_recursive(bdd, depth-1, concentration, truth_ratio),Bdd.create_random_bdd_recursive(bdd, depth-1, concentration, truth_ratio))

            else:
                return Bdd.create_random_bdd_recursive(bdd, depth-1, concentration, truth_ratio)


if __name__ == '__main__':
    # bdd = Bdd()
    # bdd.node(0,bdd.node(1,bdd.node(2,Terminal(0), Terminal(1)), Terminal(0)), bdd.node(1,bdd.node(2,Terminal(0), Terminal(1)), Terminal(0)))
    # bdd.print_nodes()
    # bdd.print_var_order()
    # print(bdd.get_root_node())
    # print(bdd.node_pool[3].low.uid)


    bdd = Bdd()
    Bdd.create_random_bdd_recursive(bdd)
    bdd.print_nodes()
