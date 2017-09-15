from scipy.stats import norm

import numpy as np

def norm_distr(depth, scale, loc):
    loc = loc * (depth-1)/2
    scale = (depth-1)/2 *1.96*scale
    probs = [norm.pdf(x, loc, scale) for x in np.linspace(-(depth - 1) / 2, (depth - 1) / 2, depth - 1)]
    sum_prob = sum(probs)
    pk = [x / sum_prob for x in probs]
    print(pk)
    return pk