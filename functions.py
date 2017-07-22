import operator

from bdd import Bdd
from bdd import Terminal

def AND(x1,x2):
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

def IMP(x1, x2):
    return (not(x1) or x2)

def EQ(x1, x2):
    return x1 == x2

def BOOL(x):
    return True if x.uid == 1 else False

def BOOL_TO_BDD(x):
    return Bdd.FALSE if not x else Bdd.TRUE