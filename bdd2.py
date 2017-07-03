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
        return '({}<-{}->{}) True: {}   False: {}'.format(self.low.var if self.low.var != "TERMINAL" else self.low, self.var, self.high.var if self.high.var != "TERMINAL" else self.high, self.true_paths, self.false_paths)


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

            node = [x for x in self.node_pool.values() if x == node][0]
        node.false_paths, node.true_paths = self.count_paths(node)
        return node

    # def set_node_true(self, node):
    #     if isinstance(node, Terminal):
    #         return
    #     node_to_replace = self.node_pool.pop(node.uid)
    #
    #     for node in self.get_nodes():
    #         if node.uid == 0 or node.uid == 1:
    #             continue
    #         print(node_to_replace)
    #
    #         if node.low.uid == node_to_replace.uid:
    #             self.set_node_true(node.low.low)
    #             self.set_node_true(node.low.high)
    #             self.node_pool[node.uid].low = self.node_pool[1]
    #             if self.node_pool[node.uid].low == self.node_pool[1] and self.node_pool[node.uid].high == \
    #                     self.node_pool[1]:
    #                 self.set_node_true(node)
    #
    #         if node.high.uid == node_to_replace.uid:
    #             self.set_node_true(node.high.low)
    #             self.set_node_true(node.high.high)
    #             self.node_pool[node.uid].high = self.node_pool[1]
    #             if self.node_pool[node.uid].low == self.node_pool[1] and self.node_pool[node.uid].high == \
    #                     self.node_pool[1]:
    #                 self.set_node_true(node)
    #
    #     self.update_counters()

    def set_node_true(self, node):
        if isinstance(node, Terminal):
            return
        if node == self.get_root_node():
            raise Exception('Trying to set root node to true')

        node = self.node_pool[node.uid] = 

    def update_counters(self):
        for node in self.get_nodes(key = lambda x: x.uid):
            self.node_pool[node.uid].false_paths, self.node_pool[node.uid].true_paths = self.count_paths(self.node_pool[node.uid])

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
            multiplier = 2 ** (abs(self.var_order.index(node.var) - self.var_order.index(child.var)) - 1)
            if child.uid == 0:
                false_count += multiplier
                continue
            if child.uid == 1:
                true_count += multiplier
                continue

            false_count += multiplier*child.false_paths
            true_count += multiplier*child.true_paths
        return false_count, true_count


    @staticmethod
    def create_random_bdd_recursive(bdd, depth=3, concentration = 0.8, truth_ratio = 0.5):
        var = depth
        recursion = lambda: Bdd.create_random_bdd_recursive(bdd, depth-1, concentration, truth_ratio)
        if depth == 0:
            return Terminal(1) if np.random.binomial(1,truth_ratio) else Terminal(0)


        else:
            if np.random.binomial(1, concentration):
                return bdd.node(var, recursion(),recursion())

            else:
                return recursion()


if __name__ == '__main__':
    # bdd = Bdd()
    # bdd.node(0,bdd.node(1,bdd.node(2,Terminal(0), Terminal(1)), Terminal(0)), bdd.node(1,bdd.node(2,Terminal(0), Terminal(1)), Terminal(0)))
    # bdd.print_nodes()
    # bdd.print_var_order()
    # print(bdd.get_root_node())
    # print(bdd.node_pool[3].low.uid)


    bdd = Bdd()
    np.random.seed(124445)
    Bdd.create_random_bdd_recursive(bdd, depth = 9)
    nodes = bdd.get_nodes(key=lambda x: x.false_paths / x.true_paths)
    # print(nodes[0])
    # bdd.print_info()
    # bdd.print_nodes()
    bdd.set_node_true(nodes[0])
    # bdd.print_nodes()
    # bdd.print_info()
    # print(bdd.get_root_node())
    print('fff')
