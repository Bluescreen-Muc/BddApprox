import numpy as np
import json

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

    def to_json(self, node=None, parent=None, no_false = True, variant=None):
        if not node:
            node = self.get_root_node()
            d = [{"group" : node.uid, "level": self.var_order.index(node.var), "name": self.get_root_node().var, "parent" : None, "children" : [self.to_json(node.low, node, no_false=no_false), self.to_json(node.high, node, no_false=no_false)]}]
            return json.dumps(d)
        if isinstance(node, Terminal):
            if node.uid == 0 and no_false:
                return []
            d = {"group" : node.uid,"level": self.var_order.index(parent.var)+1, "name": "T" if node.uid == 1 else "F", "parent": parent.var, "children": None}
            return d
        else:
            d = {"group" : node.uid,"level": self.var_order.index(node.var), "name": node.var, "parent" : parent.var, "children" : [self.to_json(node.low, node, no_false=no_false), self.to_json(node.high, node, no_false=no_false)]}
            return d

    def to_json2(self):
        node_pool = self.get_nodes(terminal=True, key=lambda x: x.uid)
        node_pool.pop(0)
        nodes = []
        links = []
        for node in node_pool:
            nodes.append({'name': node.uid, "group":self.var_order.index(node.var)})
        for id, node in enumerate(node_pool):
            if not isinstance(node, Terminal):
                if not node.low.uid == 0:
                    links.append({"source": id, "target": node_pool.index(node.low)})
                if not node.high.uid == 0:
                    links.append({"source": id, "target": node_pool.index(node.high)})
        return json.dumps({'nodes':nodes, "links": links})


    def __mul__(self, other):
        def and_nodes(node1, node2, bdd):
            if node1.uid == 0 or node2.uid == 0:
                return bdd.node_pool[0]
            if node1.uid == 1:
                if isinstance(node2, Terminal):
                    return bdd.node_pool[node2.uid]
                return bdd.node(node2.var, node2.low, node2.high)
            if node2.uid ==1:
                if isinstance(node1, Terminal):
                    return bdd.node_pool[node2.uid]
                return bdd.node(node1.var, node1.low, node1.high)
            var_index_node1 = self.get_var_index(node1.var)
            var_index_node2 = self.get_var_index(node2.var)
            if var_index_node1 < var_index_node2:
                return bdd.node(node1.var, and_nodes(node1.low,node2, bdd), and_nodes(node1.high,node2,bdd))
            elif var_index_node1 == var_index_node2:
                return bdd.node(node1.var, and_nodes(node1.low,node2.low,bdd),  and_nodes(node1.high,node2.high, bdd))
            if var_index_node1 > var_index_node2:
                return bdd.node(node2.var, and_nodes(node1,node2.low, bdd), and_nodes(node1,node2.high,bdd))

        bdd = Bdd()
        bdd.var_order = self.var_order[::]
        node_self = Node.copy_node(self.get_root_node())
        node_other = Node.copy_node(other.get_root_node())
        and_nodes(node_self, node_other, bdd)
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
                        if node.uid == 1:
                            true_count += 1
                            break
                        elif node.uid == 0:
                            false_count += 1
                            break
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
    def create_random_bdd_recursive(bdd=None, depth=3, concentration = 0.5, truth_ratio = 0.5):
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



if __name__ == '__main__':
    bdd = Bdd()
    print(Bdd.check_bdd(depth=10, nodes=10000))
    bdd = Bdd.create_random_bdd_recursive(depth=10)
    with open('/Users/oliverheidemanns/GitHub/BddApprox/Webstorm/treeData.json', 'w') as file:
       file.write(bdd.to_json())
    with open('/Users/oliverheidemanns/GitHub/BddApprox/Webstorm/graph.json', 'w')as file:
        file.write(bdd.to_json2())
    bdd = Bdd.create_random_bdd_recursive(depth=3)
    bdd2 = Bdd.create_random_bdd_recursive(depth=3)
    bdd3 = bdd * bdd2
    bdd.print_nodes()
    bdd2.print_nodes()
    bdd3.print_nodes()