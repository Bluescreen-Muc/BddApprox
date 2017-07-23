import operator

def AND(x1,x2):
    from bdd import Bdd
    if x1.uid in [0,1] and x2.uid in [0,1]:
        return BOOL_TO_BDD(BOOL(x1) and BOOL(x2))
    if x1.uid == 0 or x2.uid == 0:
        return Bdd.FALSE
    if x1.uid == 1:
        return x2
    if x2.uid == 1:
        return x1
    return operator.and_(x1,x2)

def OR(x1,x2):
    from bdd import Bdd
    if x1.uid in [0,1] and x2.uid in [0,1]:
        return BOOL_TO_BDD(BOOL(x1) or BOOL(x2))
    if x1.uid == 1 or x2.uid == 1:
        return Bdd.TRUE
    if x1.uid == 0:
        return x2
    if x2.uid == 0:
        return x1
    return operator.or_(x1,x2)

def XOR(x1,x2):
    return operator.xor(x1,x2)

def NOT(x1):
    from bdd import Bdd
    for node in x1.hash_pool.values():
        if node.uid in [0,1]:
            continue
        if node.low.uid == 0:
            node.low = Bdd.TRUE
        elif node.low.uid == 1:
            node.low = Bdd.FALSE
        if node.high.uid == 1:
            node.high = Bdd.FALSE
        elif node.high.uid == 0:
            node.high = Bdd.TRUE

def IMP(x1, x2):
    return (not(x1) or x2)

def EQ(x1, x2):
    return x1 == x2

def BOOL(x):
    return True if x.uid == 1 else False

def BOOL_TO_BDD(x):
    from bdd import Bdd
    return Bdd.FALSE if not x else Bdd.TRUE