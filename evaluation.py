import csv
from threading import Thread

import numpy as np
import os.path
import seaborn as sns
from functools import partial
import pandas as pd
import matplotlib.pyplot as plt

import time

class Counter:
    count = 0

class ApproxInfo:
    def __init__(self, node_count, bdd_depth, node_reduction, chance_wrong_output, chance_false_true_output, truth_ratio):
        self.node_count = node_count
        self.truth_ratio = truth_ratio
        self.bdd_depth = bdd_depth
        self.chance_false_true_output = chance_false_true_output
        self.node_reduction = node_reduction
        self.chance_wrong_output = chance_wrong_output

    def __str__(self):
        output = ''
        output += 'Node Reduction: {}\n'.format(self.node_reduction)
        output += "Chance wrong output: {}\n".format(self.chance_wrong_output)
        output += "Chance wrong TRUE output: {}".format(self.chance_false_true_output)
        return output

    def to_dict(self, digits=2):
        return {
            'node_count' : self.node_count,
            'bdd_depth' : self.bdd_depth,
            'node_reduction' : round(self.node_reduction, digits),
            'chance_wrong_output' : round(self.chance_wrong_output, digits),
            'chance_false_true_output' : round(self.chance_false_true_output, digits),
            'truth_ratio' : round(self.truth_ratio, digits)

        }

def evaluate(f, bdd):
    bdd.count_rec()
    node_stats_before_approx = bdd.info[bdd.root_node.uid]
    node_count_before_approx = bdd.node_count()
    sum_paths = node_stats_before_approx.false_paths + node_stats_before_approx.true_paths
    truth_ratio = (node_stats_before_approx.true_paths / sum_paths) * 100                  
    (wrong_false, wrong_true) = f(bdd)

    print(truth_ratio)
    node_count_after_approx = bdd.node_count()
    node_reduction = (1 - node_count_after_approx / node_count_before_approx) * 100
    chance_wrong_output = ((wrong_true + wrong_false) / sum_paths) * 100
    chance_false_true_output = 1#wrong_true / (node_stats_before_approx.true_paths + wrong_true) * 100
    return ApproxInfo(node_count=node_count_before_approx, bdd_depth=bdd.depth(), node_reduction=node_reduction, chance_wrong_output=chance_wrong_output,
                      chance_false_true_output=chance_false_true_output, truth_ratio=truth_ratio)


def approximation_eval(f, n=100, min_depth=10, max_depth=10, min_truth=0.5, max_truth=0.5, file='stats.txt', digits=2, min_levels=1, max_levels=None):
    METHOD_DICT = {Bdd.rounding_up: "ROUNDING_UP", Bdd.rounding: "ROUNDING"}
    method = METHOD_DICT[f] if f in METHOD_DICT else "UNKNOWN"

    if not max_levels:
        max_levels = max_depth
    fields = ['node_count', 'bdd_depth', 'node_reduction', 'chance_wrong_output', 'chance_false_true_output', 'truth_ratio', 'levels', 'time', 'approx_method']
    if not os.path.isfile(file):
        with open(file, 'w') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=fields)
            writer.writeheader()

    with open(file, 'a') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fields)
        for _ in range(n):
            Counter.count += 1
            print(Counter.count)
            depth = np.random.randint(min_depth, max_depth + 1)
            truth_rate = np.random.randint(int(min_truth), int(max_truth) + 1) / 100
            level = min(depth-1, np.random.randint(min_levels, max_levels+1))
            bdd = Bdd.create_bdd(depth=depth, truth_rate=truth_rate)
            if Counter.count == 22:
                import gfx
                gfx.draw(bdd, 'bdd1.png')
            process_time = time.time()
            result= evaluate(partial(f, level=level), bdd)
            process_time = time.time() - process_time
            if result:
                result_dict = result.to_dict(digits=digits)
                result_dict['levels'] = level
                result_dict['time'] = round(process_time,2)
                result_dict['approx_method'] = method
                writer.writerow(result_dict)
            if Counter.count == 22:
                import gfx
                gfx.draw(bdd)



def load_csv(file):
    return pd.read_csv(file)




if __name__ == '__main__':
    from bdd import Bdd

    #np.random.seed(12345)
    while True:
        #approximation_eval(Bdd.rounding_up, min_depth=10, max_depth=100, min_truth=5, max_truth=95, n=500, file='stats.txt')
        break

    data = load_csv('stats.txt')
    #data = data[data['chance_wrong_output'] < 20]
    data['quality'] = data['node_reduction'] - data['chance_wrong_output']
    data['depth_level_ratio'] = data['levels'] / data['bdd_depth']
    #data = data[data['depth_level_ratio'] >0.8]

    data = data[data['truth_ratio'] > 50]
    #data = data[data['bdd_depth'] > 20]
    #data = data[data['node_reduction'] < 95]
    #
    sns.lmplot(y = 'node_reduction', x='chance_wrong_output',data=data, hue='approx_method')
    sns.lmplot(y='quality', x='depth_level_ratio', data=data, hue='approx_method')
    #sns.lmplot(y='chance_wrong_output', x='node_reduction', data=data)#, hue='levels')
    #sns.lmplot(y='time', x='node_count', data=data, hue='levels')
    plt.show()