import numpy as np
from copy import copy
import pickle
import fun

class BddDataset:
    ID = 0
    def __init__(self, bdd):
        self.ID = BddDataset.ID
        BddDataset.ID += 1
        self.bdd = bdd

def random_function(file, func_size=10, depth=50, min_nodes = 100, max_nodes=2000, n=100):
    from bdd import Bdd


    with open(file, 'wb') as output_file:
        result=None
        counter = 0
        while True:
        #var = np.random.random_integers(1,depth,func_size)
            var = np.random.choice(np.arange(1,depth), func_size,replace=False)
            bdds = [Bdd.Var(int(x)) for x in var]
            tmp = bdds.pop()
            funcs = np.random.choice(fun.functions, func_size-1)
            for i, func in enumerate(funcs):
                if func == fun.NOT:
                    tmp = fun.NOT(tmp)

                else:
                    tmp = Bdd.apply(func, tmp, bdds.pop())
            if not result:
                result = copy(tmp)
            else:
                func = np.random.choice(fun.functions)
                if func == fun.NOT:
                    result = fun.NOT(result)
                else:
                    result = Bdd.apply(func, result, tmp)
                    if result.node_count() > min_nodes:
                        pickle.dump(BddDataset(result.hash_pool), output_file, pickle.HIGHEST_PROTOCOL)
                    if result.node_count() > max_nodes:
                        result=None
            counter += 1
            if counter >= n:
                break

    return n

if __name__ == '__main__':
    random_function('test.pkl', n=100)