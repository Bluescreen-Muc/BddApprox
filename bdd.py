
from copy import copy
from weakref import WeakSet, WeakValueDictionary
import sys
import gfx
import time
from evaluation import *
from collections import defaultdict, Counter
import gc

from fun import *


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
        self.uid_pool = WeakValueDictionary()
        self.uid_pool[0] = Bdd.FALSE
        self.uid_pool[1] = Bdd.TRUE
        self.hash_pool[0]= Bdd.FALSE
        self.hash_pool[1]= Bdd.TRUE
        self.var_pool = defaultdict(WeakSet)
        self.root_node = None
        self.info = defaultdict(NodeInfo)

    def set_max_var(self, n):
        self.max_var == n

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
                self.var_pool[node.var].add(node)
                self.uid_pool[node.uid]=node
                self.info[node.low.uid].parents.add(node)
                self.info[node.high.uid].parents.add(node)
                Node.uid += 1
                return node

    def node_count(self):
        return len(list(self.hash_pool.values())) - 2

    def depth(self):
        return self.max_var

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
        for info in self.info.values():
            info.true_paths = 0
            info.false_paths = 0

        vars = self.get_vars()
        for var in vars:
            for uid in {x.uid for x in self.var_pool[var]}:
                node = self.uid_pool[uid]
                for child in node:
                    if child.uid == 0:
                        self.info[node.uid].false_paths += 2**(self.max_var-var)
                    elif child.uid == 1:
                        self.info[node.uid].true_paths += 2 ** (self.max_var - var)

                    else:
                        self.info[node.uid].false_paths += (2**(child.var - node.var - 1))*self.info[child.uid].false_paths
                        self.info[node.uid].true_paths += (2**(child.var - node.var - 1))*self.info[child.uid].true_paths
        if self.root_node.is_terminal():
            self.info[self.root_node.uid].false_paths = 2**self.max_var if self.root_node == Bdd.FALSE else 0
            self.info[self.root_node.uid].true_paths = 2 ** self.max_var if self.root_node == Bdd.TRUE else 0
        else:
            root_multiplier = 2**(self.root_node.var - 1)
            self.info[self.root_node.uid].false_paths *= root_multiplier
            self.info[self.root_node.uid].true_paths *= root_multiplier
        return {x.uid : [self.info[x.uid].false_paths, self.info[x.uid].true_paths] for x in self.hash_pool.values()}

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



    def count_paths(self):
        for info in self.info.values():
            info.paths = 0
        paths = int(2**self.depth())
        self.info[self.root_node.uid].paths = paths
        if self.root_node.is_terminal():
            return {x.uid: self.info[x.uid].paths for x in self.hash_pool.values() if x.uid not in [0, 1]}
        vars = self.get_vars(self.root_node.var + 1, from_bottom=False)
        for var in vars:
            for node in self.var_pool[var]:
                node = self.uid_pool[node.uid]
                paths = 0
                for parent in self.info[node.uid].parents:
                    paths += self.info[parent.uid].paths // 2
                self.info[node.uid].paths = paths
        return {x.uid : self.info[x.uid].paths for x in self.hash_pool.values() if x.uid not in [0,1]}


    @staticmethod
    def rounding_up(bdd, level=1):
        bdd.count_rec()
        true_paths = bdd.info[bdd.root_node.uid].true_paths

        vars_to_check = bdd.get_vars(lower = bdd.max_var-level+1)

        for var in vars_to_check:
            for node in list(bdd.var_pool[var]):
                node = bdd.uid_pool[node.uid]
                if node.low == Bdd.TRUE or node.high == Bdd.TRUE:
                    continue

                else:
                    bdd.set_node(node.low, Bdd.TRUE) if bdd.info[node.low.uid].true_paths > bdd.info[node.high.uid].true_paths \
                        else bdd.set_node(node.high, Bdd.TRUE)
        bdd.count_rec()
        wrong_true = bdd.info[bdd.root_node.uid].true_paths - true_paths
        return (0,wrong_true)


    @staticmethod
    def rounding(bdd, level=1):
        stats_before = dict()
        false_true_before = bdd.count_rec()
        paths_before = bdd.count_paths()
        for uid in paths_before.keys():
            low_uid = bdd.uid_pool[uid].low.uid
            high_uid = bdd.uid_pool[uid].high.uid
            if low_uid == 1:
                false_before = 0
            elif low_uid == 0:
                false_before = paths_before[uid] / 2

            else:
                false_before = paths_before[uid] / 2 * (false_true_before[low_uid][0] / (false_true_before[low_uid][0] + false_true_before[low_uid][1]))

            if high_uid == 1:
                pass
            elif high_uid == 0:
                false_before += paths_before[uid] / 2
            else:
                false_before += paths_before[uid] / 2 * (
                                false_true_before[high_uid][0] / (false_true_before[high_uid][0] + false_true_before[high_uid][1]))

            if low_uid == 1:
                true_before = paths_before[uid] / 2
            elif low_uid == 0:
                true_before = 0

            else:
                true_before = paths_before[uid] / 2 * (
            false_true_before[low_uid][1] / (false_true_before[low_uid][0] + false_true_before[low_uid][1]))

            if high_uid == 1:
                true_before += paths_before[uid] / 2
            elif high_uid == 0:
                pass

            else:
                true_before += paths_before[uid] / 2 * (false_true_before[high_uid][1] / (false_true_before[high_uid][0] + false_true_before[high_uid][1]))

            stats_before[uid]=[false_before, true_before]
        vars_to_check = bdd.get_vars(lower=bdd.max_var - level+1, from_bottom=False)

        for var in vars_to_check:

            for uid in list(x.uid for x in bdd.var_pool[var]):
                if uid in bdd.uid_pool:
                    node = bdd.uid_pool[uid]
                else:
                    continue


                if not node.low.is_terminal():
                    if bdd.info[node.low.uid].path_difference() >= 0:
                        bdd.set_node(node.low, Bdd.TRUE)



                    else:
                        bdd.set_node(node.low, Bdd.FALSE)



                if not node.high.is_terminal():
                    node = bdd.uid_pool[node.uid]
                    if bdd.info[node.high.uid].path_difference() >= 0:

                        bdd.set_node(node.high, Bdd.TRUE)

                    else:

                        bdd.set_node(node.high, Bdd.FALSE)


        stats_after = dict()
        false_true_after = bdd.count_rec()
        paths_after = bdd.count_paths()
        for uid in paths_after.keys():
            low_uid = bdd.uid_pool[uid].low.uid
            high_uid = bdd.uid_pool[uid].high.uid
            if low_uid == 1:
                false_after = 0
            elif low_uid == 0:
                false_after = paths_before[uid] / 2

            else:
                false_after = (paths_after[uid] / 2 * (false_true_after[low_uid][0] / (false_true_after[low_uid][0] + false_true_after[low_uid][1])))

            if high_uid == 1:
                pass
            elif high_uid == 0:
                false_after += paths_before[uid] / 2

            else:
                false_after += (paths_after[uid] / 2 * (false_true_after[high_uid][0] / (false_true_after[high_uid][0] + false_true_after[high_uid][1])))


            if low_uid == 1:
                true_after = paths_before[uid] / 2
            elif low_uid == 0:
                true_after = 0

            else:
                true_after = (paths_after[uid] / 2 * (false_true_after[low_uid][1] / (false_true_after[low_uid][0]+ false_true_after[low_uid][1])))


            if high_uid == 1:
                true_after += paths_before[uid] / 2
            elif high_uid == 0:
                pass

            else:
                true_after += paths_after[uid] / 2 * (false_true_after[high_uid][1] / (false_true_after[high_uid][0] + false_true_after[high_uid][1]))

            stats_after[uid] = [false_after, true_after]

        same_nodes = set(paths_before.keys()).intersection(set(paths_after.keys()))
        true_difference = 0
        false_difference = 0
        for uid in same_nodes:

            false_difference +=  abs(stats_before[uid][0] - stats_after[uid][0])
            true_difference += abs(stats_before[uid][1]-stats_after[uid][1])
        print(false_difference, true_difference)

        return 0, 0

    def remap_node_child(self, node, child, position):
        node = self.hash_pool[hash(node)]
        if position not in ["LOW", "HIGH"]:
            print('error')
            return
        tmp_node = Node(node.var, node.low, node.high)

        if position == "LOW":
            tmp_node.low = child
        elif position == "HIGH":
            tmp_node.high = child

        if tmp_node.low == tmp_node.high:
            self.remove_parent(node.low, node)
            self.remove_parent(node.high, node)
            self.set_node(node, tmp_node.low)

        elif hash(tmp_node) in self.hash_pool:

            self.set_node(node, self.hash_pool[hash(tmp_node)])

        else:
            node = self.hash_pool.pop(hash(node))
            self.var_pool[node.var].remove(node)
            if position == "LOW":
                self.remove_parent(node.low, node)
                node.low = tmp_node.low
                self.add_parent(node.low, node)

            if position == "HIGH":
                self.remove_parent(node.high, node)
                node.high = tmp_node.high
                self.add_parent(node.high, node)

            self.hash_pool[hash(node)] = node
            self.var_pool[node.var].add(node)




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
    def Var(x):
        bdd = Bdd()
        bdd.root_node = bdd.node(x, Bdd.FALSE, Bdd.TRUE)
        return bdd

    @staticmethod
    def random_function(n = 3, max = 10, repeat = 1):
        result = None
        for _ in range(repeat):
            var = np.random.random_integers(1,max,n)
            bdds = [Bdd.Var(int(x)) for x in var]
            tmp = bdds.pop()
            funcs = np.random.choice(functions, n-1)
            for i, func in enumerate(funcs):
                if func == NOT:
                    tmp = NOT(tmp)

                else:
                    tmp = Bdd.apply(func, tmp, bdds.pop())
            if not result:
                result = tmp
            else:
                func = np.random.choice(functions)
                if func == NOT:
                    result = NOT(result)
                else:
                    result = Bdd.apply(func, result, tmp)

        return result

    @staticmethod
    def create_bdd(depth=10, truth_rate = 0.5):
        while True:
            bdd = Bdd()
            bdd.set_max_var(depth)
            depth = depth - 1
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

    def set_node(self, old_node, new_node, animation=False):
        if old_node == self.root_node:
            self.root_node = new_node
            return
        vars = self.get_vars(from_bottom=True, upper=old_node.var-1)
        for var in vars:
            for uid in list(x.uid for x in self.var_pool[var]):
                if not uid in self.uid_pool:
                    continue
                else:
                    node = self.uid_pool[uid]
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
                            self.add_parent(new_node, node)
                            self.hash_pool[hash(node)] = node
                            self.var_pool[node.var].add(node)
                            if animation:
                                gfx.draw(self, 'animation{}.png'.format(gfx.ANIMATION_COUNTER))
                                gfx.ANIMATION_COUNTER += 1

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
                            self.add_parent(new_node, node)
                            self.hash_pool[hash(node)] = node
                            self.var_pool[node.var].add(node)

    def get_parents(self, node):
        return self.info[node.uid].parents

    def check_correct_parents(self):
        gc.collect()
        for node in self.hash_pool.values():
            if node.is_terminal():
                continue
            parents = self.get_parents(node)

            for parent in parents:
                if len(parents) != sys.getrefcount(node)-3:
                    print("ERROR")
                    sys.exit(2)
                if node != parent.low and node != parent.high:
                    print("ERROR")
                    print(node)
                    print(parents)
                    gfx.draw(bdd, info=True)
                    sys.exit(1)

    def get_var(self, node):
        if node.is_terminal():
            return self.max_var + 1
        else:
            return node.var

    def add_parent(self, node, new_parent):
        node = self.hash_pool[hash(node)]
        self.info[node.uid].parents.add(new_parent)

    def remove_parent(self, node, parent):
        node = self.hash_pool[hash(node)]
        self.info[node.uid].parents.discard(parent)

class NodeInfo:

    def __init__(self):
        self.true_paths = 0
        self.false_paths = 0
        self.paths = 0
        self.parents = WeakSet()

    def path_difference(self):
        return self.true_paths - self.false_paths

    def __str__(self):
        return "{} {}".format(self.false_paths, self.true_paths)


if __name__ == '__main__':
    count = 0

    bdd = Bdd.create_bdd(5)
    gfx.draw(bdd, 'bdd1.png')
    bdd = NOT(bdd)
    gfx.draw(bdd, 'bdd2.png')
    gfx.draw(bdd)
    # for _ in range(1):
    #     bdd = Bdd.random_function(n=50, max=60, repeat=4)
    #     gfx.draw(bdd)