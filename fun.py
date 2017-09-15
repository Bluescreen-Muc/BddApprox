
def AND(x1,x2):
    if x1.uid in [0,1] and x2.uid in [0,1]:
        return BOOL_TO_BDD(x1.uid and x2.uid)

def OR(x1,x2):
    if x1.uid in [0,1] and x2.uid in [0,1]:
        return BOOL_TO_BDD(x1.uid or x2.uid)

def XOR(x1,x2):
    pass

def NOT(x1):
    from bdd import Bdd

    for var in x1.get_vars(from_bottom=True):
        to_do = []
        for node in x1.var_pool[var]:
            if node.uid in [0, 1] or ( node.low.uid not in [0, 1] and  node.high.uid not in [0, 1]):
                continue
            node = x1.hash_pool.pop(hash(node))
            if node.low.uid == 0:
                node.low = Bdd.TRUE
            elif node.low.uid == 1:
                node.low = Bdd.FALSE
            if node.high.uid == 1:
                node.high = Bdd.FALSE
            elif node.high.uid == 0:
                node.high = Bdd.TRUE
            to_do.append(node)
        for node in to_do:
            x1.hash_pool[hash(node)] = node
        x1.update_var_pool_hash_values()
    return x1



def IMP(x1, x2):
    return (not(x1) or x2)


def BOOL_TO_BDD(x):
    from bdd import Bdd
    return Bdd.FALSE if not x else Bdd.TRUE

functions = [AND, OR]
