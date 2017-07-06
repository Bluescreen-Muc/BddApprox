import operator

def AND(x1,x2):
    return operator.and_(x1,x2)

def OR(x1,x2):
    return operator.or_(x1,x2)

def XOR(x1,x2):
    return operator.xor(x1,x2)

def IMP(x1, x2):
    return (not(x1) or x2)

def EQ(x1, x2):
    return x1 == x2