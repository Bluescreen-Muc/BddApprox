
from copy import copy
from weakref import WeakSet, WeakValueDictionary
import sys
import gfx
import time
from evaluation import *
from collections import defaultdict, Counter
import gc



class Node:
    uid = 2
    MAX_NUMBER_NODES = 10000
    def __init__(self, var=None, low=None, high=None):
        self.var = var
        self.uid = Node.uid
        self.low = low
        self.high = high
        self.iter_index = -1

    def is_terminal(self):
        return self.uid in [0,1]

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

    def node(self, var, low, high):
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

    def node_count(self):
        return len(list(self.hash_pool.values())) - 2

    def depth(self):
        return self.max_var

    def delete_orphans(self):
        while True:
            children = []
            parents = self.hash_pool
            for node in parents:
                if node.is_terminal():
                    continue
                children += [node.low, node.high]
            orphans = [x for x in parents if x not in children and x != self.root_node]
            if not orphans:
                break
            else:
                [self.hash_pool.remove(x) for x in orphans]

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
        for x in range(lower,min(self.max_var+1, upper+1)):
            if self.var_pool[x]:
                output.append(x)
        return output[::-1] if from_bottom else output

    def count_rec(self):
        self.info.clear()
        vars = self.get_vars()
        for var in vars:
            for node in self.var_pool[var]:
                for child in node:
                    if child.uid == 0:
                        self.info[node.uid].false_paths += 2**(self.max_var-var)
                    elif child.uid == 1:
                        self.info[node.uid].true_paths += 2 ** (self.max_var - var)

                    else:
                        self.info[child.uid].parents.append(node)
                        self.info[node.uid].false_paths += (2**(child.var - node.var - 1))*self.info[child.uid].false_paths
                        self.info[node.uid].true_paths += (2**(child.var - node.var - 1))*self.info[child.uid].true_paths
        if self.root_node.is_terminal():
            self.info[self.root_node.uid].false_paths = 2**self.max_var if self.root_node == Bdd.FALSE else 0
            self.info[self.root_node.uid].true_paths = 2 ** self.max_var if self.root_node == Bdd.TRUE else 0
        else:
            root_multiplier = 2**(self.root_node.var - 1)
            self.info[self.root_node.uid].false_paths *= root_multiplier
            self.info[self.root_node.uid].true_paths *= root_multiplier
        return self.info[self.root_node.uid].false_paths,  self.info[self.root_node.uid].true_paths

    def __copy__(self):
        bdd = Bdd()
        node_list= dict()
        node_list[1] = Bdd.TRUE
        node_list[0] = Bdd.FALSE
        for var in self.get_vars(from_bottom=True):
            for node in self.var_pool[var]:
                if node.is_terminal():
                    continue
                if node == self.root_node:
                    bdd.root_node = bdd.node(node.var, node_list[hash(node.low)], node_list[hash(node.high)])
                else:
                    node_list[hash(node)] = bdd.node(node.var, node_list[hash(node.low)], node_list[hash(node.high)])
        return bdd


    def get_node_by_uid(self, uid):
        for x in self.hash_pool.values():
            if x.uid == uid:
                return x
        if x.uid == 0:
            return Bdd.FALSE
        if x.uid == 1:
            return Bdd.TRUE
        print('oops')
        return None

    def get_ancestors(self, node, counter = 0):
        for ref_node in gc.get_referrers(node):
            print(ref_node)




    @staticmethod
    def rounding_up(bdd, level=1):
        bdd.count_rec()
        vars_to_check = bdd.get_vars(lower = bdd.max_var-level+1)

        for var in vars_to_check:
            for node in list(bdd.var_pool[var]):
                if node.low == Bdd.TRUE or node.high == Bdd.TRUE:
                    continue

                else:
                    bdd.set_node(node.low, Bdd.TRUE) if bdd.info[node.low.uid].true_paths > bdd.info[node.high.uid].true_paths \
                        else bdd.set_node(node.high, Bdd.TRUE)
                    bdd.update_var_pool_hash_values()
    @staticmethod
    def rounding(bdd, level=1):
        bdd.count_rec()
        vars_to_check = bdd.get_vars(lower=bdd.max_var - level + 1)

        for var in vars_to_check:
            for node in list(bdd.var_pool[var]):
                if not node.low.is_terminal():
                    bdd.set_node(node.low, Bdd.TRUE) if bdd.info[node.low.uid].path_difference() >= 0 else bdd.set_node(node.low, Bdd.FALSE)
                if not node.high.is_terminal():
                    bdd.set_node(node.high, Bdd.TRUE) if bdd.info[node.high.uid].path_difference() >= 0 else bdd.set_node(
                    node.high, Bdd.FALSE)

    def get_nodes(self):
        return self.hash_pool.values()

    def print_info(self):
        for node, info in self.info:
            print(info)

    def update_var_pool_hash_values(self):
        for var in self.var_pool:
            self.var_pool[var] = WeakSet({x for x in self.var_pool[var]})

    @staticmethod
    def apply(f, bdd1, bdd2):
        def apply_nodes(f, node1, node2, bdd):
            if node1.is_terminal() and node2.is_terminal():
                return f(node1,node2)

            if bdd.get_var(node1) < bdd.get_var(node2):
                return bdd.node(node1.var, apply_nodes(f, node1.low, node2, bdd),
                                apply_nodes(f, node1.high, node2, bdd))
            elif bdd.get_var(node1) == bdd.get_var(node2):
                return bdd.node(node1.var, apply_nodes(f, node1.low, node2.low, bdd),
                                apply_nodes(f, node1.high, node2.high, bdd))
            elif bdd.get_var(node1) > bdd.get_var(node2):
                return bdd.node(node2.var, apply_nodes(f, node1, node2.low, bdd),
                                apply_nodes(f, node1, node2.high, bdd))

        bdd = Bdd()
        bdd.max_var = max(bdd1.max_var, bdd2.max_var)
        bdd.root_node = apply_nodes(f, bdd1.root_node, bdd2.root_node, bdd)
        return bdd


    @staticmethod
    def create_bdd(depth=10, truth_rate = 0.5):
        while True:
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
            if not bdd.root_node.is_terminal():
                array = None
                bdd.check_weak_references()
                return bdd

    def check_weak_references(self):
        if self.hash_pool._pending_removals:
            print("FOUND WEAK REFERENCES")
            self.hash_pool._commit_removals()

    def check_bdd(self):
        list = []
        for x in self.get_vars():
            for node in self.var_pool[x]:
                if node not in self.var_pool[x]:
                    return True
        for node in self.hash_pool.values():
            # if node not in self.hash_pool.values():
            #     return True
            if node.is_terminal():
                continue
            list.append((node.var, node.low.uid, node.high.uid))
            if node.low == node.high:
                return True
            if node.low.var == node.var or node.high.var == node.var:
                return True
        return (any(filter(lambda x: x >1, Counter(list).values())))

    def set_node(self, old_node, new_node):
        vars = self.get_vars(from_bottom=True, upper=old_node.var-1)
        for var in vars:
            for node in list(self.var_pool[var]):
                if node.low == old_node:
                    tmp_node = Node(node.var, new_node, node.high)
                    if tmp_node.high == tmp_node.low:
                        self.set_node(node, new_node)
                    else:
                        node_hash = hash(tmp_node)
                        if node_hash in self.hash_pool:
                            self.set_node(node, self.hash_pool[node_hash])
                        else:
                            node = self.hash_pool.pop(hash(node))
                            self.var_pool[node.var].remove(node)
                            node.low = new_node
                            self.hash_pool[hash(node)] = node
                            self.var_pool[node.var].add(node)
                            self.update_var_pool_hash_values()
                if node.high == old_node:
                    tmp_node = Node(node.var, node.low, new_node)
                    if tmp_node.high == tmp_node.low:
                        self.set_node(node, new_node)
                    else:
                        node_hash = hash(tmp_node)
                        if node_hash in self.hash_pool:
                            self.set_node(node, self.hash_pool[node_hash])

                        else:
                            node = self.hash_pool.pop(hash(node))
                            self.var_pool[node.var].remove(node)
                            node.high = new_node
                            self.hash_pool[hash(node)] = node
                            self.var_pool[node.var].add(node)
                            self.update_var_pool_hash_values()

    def get_var(self, node):
        if node.is_terminal():
            return self.max_var + 1
        else:
            return node.var



class NodeInfo:

    def __init__(self):
        self.true_paths = 0
        self.false_paths = 0
        self.parents = []

    def path_difference(self):
        return self.true_paths - self.false_paths

    def __str__(self):
        return "{} {}".format(self.false_paths, self.true_paths)

if __name__ == '__main__':
    count = 0
    while True:
        count += 1
        bdd = Bdd.create_bdd(10)
        #gfx.draw(bdd, 'bdd1.png')
        #Bdd.rounding(bdd, 2)
        Bdd.rounding_up(bdd, np.random.randint(31))
        gfx.draw(bdd, 'bdd2.png')

        #bdd.get_ancestors(bdd.var_pool[10].pop())
        break
        if bdd.check_bdd():
            break
