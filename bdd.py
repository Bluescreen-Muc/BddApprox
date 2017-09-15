
from copy import copy
from weakref import WeakSet, WeakValueDictionary
import sys
import gfx
from scipy.stats import norm, truncnorm
from scipy import stats
from evaluation import *
from collections import defaultdict, Counter
import gc

from fun import *
from stati import norm_distr
class Node:
    uid = 2
    MAX_NUMBER_NODES = 10000
    def __init__(self, var=None, low=None, high=None, uid=None):
        self.var = var
        if not uid:
            self.uid = Node.uid
        else:
            self.uid = uid
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

    def node(self, var, low, high, uid=None):
        node = Node(var, low, high, uid)
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
                    bdd.root_node = bdd.node(node.var, node_list[hash(node.low)], node_list[hash(node.high)], uid=node.uid)
                else:
                    node_list[hash(node)] = bdd.node(node.var, node_list[hash(node.low)], node_list[hash(node.high)], uid=node.uid)
        return bdd

    def calculate_score(self, dom_variant=1):
        self.count_paths()
        self.count_rec()
        self.count_dom_score(variant=dom_variant)
        all_true = self.info[self.root_node.uid].paths
        node_count = self.node_count()
        for node in self.hash_pool.values():
            if node.is_terminal():
                continue
            if node.low.is_terminal() or node.high.is_terminal():
                continue
            if node.low.var == node.high.var:
                scores = []
                info = self.info[node.uid]
                node_true = info.truth_ratio() * info.paths
                for child in node:
                    if child.is_terminal():
                        break
                    child_true = self.info[child.uid].truth_ratio() * info.paths
                    child_score = child_true - node_true
                    scores.append(child_score/all_true)

                    info.score = scores
                    info.score = [((info.dom_score[i])/node_count) / x for i,x in enumerate(info.score) if x != 0]
        gfx.draw(self, 'bdd3.png', info=True)
        if self.info[self.root_node.uid].score[1] > 1:
            self.set_node(self.root_node, self.root_node.low)
            self.count_paths()
            self.count_rec()
            self.count_dom_score(variant=dom_variant)
            gfx.draw(self, 'bdd4.png', info=True)


    def count_dom_score(self, variant=2):
        for node in self.hash_pool.values():
            if node.is_terminal():
                continue
            result = []
            result.append(self.nodes_dominated(node.high, variant=variant))
            result.append(self.nodes_dominated(node.low, variant=variant))

            self.info[node.uid].dom_score = result
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

        vars_to_check = bdd.get_vars(lower = bdd.max_var-level+1, from_bottom=True)

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
            if node1.is_terminal() or node2.is_terminal():
                if f == AND:
                    if node1.uid == 0 or node2.uid == 0:
                        return Bdd.FALSE
                elif f == OR:
                    if node1.uid == 1 or node2.uid == 1:
                        return Bdd.TRUE
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
    def add_subtree(self, node):
        if node.uid == 0:
            return Bdd.FALSE
        elif node.uid == 1:
            return Bdd.TRUE

        self.node(node.var, self.add_subtree(node.low), self.add_subtree((node.high)))
        return node



    @staticmethod
    def Var(x):
        bdd = Bdd()
        bdd.root_node = bdd.node(x, Bdd.FALSE, Bdd.TRUE)
        return bdd

    @staticmethod
    def random_function(func_size=10, depth=50, nodes=2000, file='random.pkl'):
        result = None
        while True:
            #var = np.random.random_integers(1,depth,func_size)
            var = np.random.choice(np.arange(1,depth), func_size,replace=False)
            bdds = [Bdd.Var(int(x)) for x in var]
            tmp = bdds.pop()
            funcs = np.random.choice(functions, func_size-1)
            for i, func in enumerate(funcs):
                if func == NOT:
                    tmp = NOT(tmp)

                else:
                    tmp = Bdd.apply(func, tmp, bdds.pop())
            if not result:
                result = copy(tmp)
            else:
                func = np.random.choice(functions)
                if func == NOT:
                    result = NOT(result)
                else:
                    print(tmp.node_count())
                    print(result.node_count())
                    result = Bdd.apply(func, result, tmp)
                    print()
                    if result.node_count() > nodes:
                        break

        if result.node_count() == 0:
            return Bdd.random_function(func_size,depth,nodes)
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

    @staticmethod
    def create_bdd2(y_func, depth=20, n=1000, scale=0, loc=0):
        def generate_list(prob_func):
            node_list = defaultdict(int)
            while True:
                for x in  prob_func.rvs(size=n):
                    node_list[x] += 1
                for x in sorted(node_list.keys()):
                    sum_free_nodes = 2 + sum(2 * node_list[y] for y in range(0,x))
                    sum_nodes = sum(node_list[y] for y in range(0,x+1))
                    if sum_free_nodes < sum_nodes:
                        node_list[x] += sum_free_nodes - sum_nodes
                    print(sum_free_nodes)
                break
            return node_list

        depth = max(2, depth)
        n = min(2**(depth)-2, n)
        xk = np.arange(0, depth-1)
        pk = norm_distr(depth, scale, loc)
        custm = stats.rv_discrete(name='custm', values=(xk, pk))
        node_list = generate_list(custm)
        print(node_list)
        plt.plot(xk,pk)
        xn = []
        yn = []
        for key in sorted(list(node_list.keys())):
            yn += [node_list[key]]
            xn += [key]
        plt.plot(xn,yn)
        plt.show()

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
    def nodes_dominated_slow(self, node):
        bdd = copy(self)
        node_count = bdd.node_count()
        for n in bdd.uid_pool.values():

            if n.is_terminal():
                continue

            if n.low.uid == node.uid:

                n.low = None
            if n.high.uid  == node.uid:
                n.high = None
        return node_count - bdd.node_count()

    def nodes_dominated(self, node, variant=1):

        nodes_gone = []
        if node.is_terminal():
            return 1
        nodes_to_check = [node]
        nodes_waiting = dict()
        while True:
            if not nodes_to_check:
                break

            node = nodes_to_check.pop()
            if node.is_terminal():
                continue
            if len(self.info[node.uid].parents) == 1:
                nodes_gone.append(node)
                for child in node:
                    if child.is_terminal():
                        continue
                    nodes_to_check.append(child)
            else:
                if not node in nodes_waiting:
                    nodes_waiting[node] = len(self.info[node.uid].parents) - 1
                else:
                    nodes_waiting[node] -= 1
                    if nodes_waiting[node] == 0:
                        nodes_gone.append(node)
                        for child in node:
                            if child.is_terminal():
                                continue
                            nodes_to_check.append(child)
        if variant == 1:
            output = len(nodes_gone) + 1
        elif variant == 2:
            output = len(nodes_gone) + 1 + sum(1/(x+1) for x in nodes_waiting.values() if x != 0)
        return output

    def create_test ():
        n = np.random.randint()
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


    def check_subtree_methods(self):
        for node in self.hash_pool.values():
            if node == self.root_node or node.is_terminal():
                continue
            time1 = time.time()
            sub1 = self.nodes_dominated_slow(node)
            time1 = time.time() - time1
            time2 = time.time()
            sub2 = self.nodes_dominated(node)
            time2 = time.time() - time2
            assert(self.nodes_dominated(node)==self.nodes_dominated_slow(node))
        print(time1/time2)

    def skew(self):
        for var in self.get_vars(from_bottom=False):
            pass



class NodeInfo:

    def __init__(self):
        self.true_paths = 0
        self.false_paths = 0
        self.paths = 0
        self.parents = WeakSet()
        self.score = 0
        self.dom_score = 0

    def path_difference(self):
        return self.true_paths - self.false_paths

    def truth_ratio(self):
        return self.true_paths / (self.true_paths + self.false_paths)

    def false_ratio(self):
        return self.false_paths / (self.true_paths + self.false_paths)

    def __str__(self):
        return "{} {}".format(self.false_paths, self.true_paths)


def trunc(mean, std, a,b ):
    a, b = (a - mean) / std, (b - mean) / std
if __name__ == '__main__':
    #Bdd.create_bdd2(norm_distr, depth=11, n= 100, scale=1, loc=-20)
    #rnp.random.seed(333333333)
    start = time.time()
    bdd = Bdd.random_function(func_size=3, depth=10, nodes=20)
    print(bdd.node_count())
    #bdd2 = Bdd.random_function(func_size=15, depth=40, nodes=700)
    print(time.time() - start)
    gfx.draw(bdd, dot=True)
    #bdd3 = Bdd.apply(AND, bdd, bdd2)
    #print(bdd3.node_count())

    # count = 0
    # start = time.time()

    # while True:
    #     bdd = Bdd.create_bdd(10)
    #     bdd.count_paths()
    #     bdd.count_rec()
    #
    #
    #     break
    #
    #
    #gfx.draw(bdd)
    # bdd2 = NOT(copy(bdd))
    # print(Bdd.rounding_up(bdd,2)[1])
    # bdd.count_rec()
    # gfx.draw(bdd, 'bdd2.png', info = True)
    # bdd3 = Bdd.apply(AND, bdd2, bdd)
    # bdd3.count_rec()
    # gfx.draw(bdd3, 'bdd3.png', info=True)

    # for _ in range(1):

