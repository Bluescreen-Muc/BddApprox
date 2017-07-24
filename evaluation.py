import csv
from threading import Thread

import numpy as np
import os.path
import seaborn as sns
from functools import partial
import pandas as pd
import matplotlib.pyplot as plt
from enum import Enum

import time


class Methods(Enum):
    ROUNDING = 1

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
    f(bdd)
    bdd.count_rec()
    truth_ratio = (node_stats_before_approx.true_paths / sum_paths) * 100
    node_stats_after_approx = bdd.info[bdd.root_node.uid]
    node_count_after_approx = bdd.node_count()
    node_reduction = (1 - node_count_after_approx / node_count_before_approx) * 100
    chance_wrong_output = ((node_stats_after_approx.true_paths - node_stats_before_approx.true_paths) / sum_paths) * 100
    chance_false_true_output = ((node_stats_after_approx.true_paths - node_stats_before_approx.true_paths) / node_stats_after_approx.true_paths) * 100
    return ApproxInfo(node_count=node_count_before_approx, bdd_depth=bdd.depth(), node_reduction=node_reduction, chance_wrong_output=chance_wrong_output,
                      chance_false_true_output=chance_false_true_output, truth_ratio=truth_ratio)


def approximation_eval(f, n=100, min_depth=10, max_depth=10, min_truth=0.5, max_truth=0.5, file='stats.txt', digits=2, min_levels=2, max_levels=2, method = Methods.ROUNDING):
    from gfx import draw
    fields = ['node_count', 'bdd_depth', 'node_reduction', 'chance_wrong_output', 'chance_false_true_output', 'truth_ratio', 'levels', 'time']
    if not os.path.isfile(file):
        with open(file, 'w') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=fields)
            writer.writeheader()

    with open(file, 'a') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fields)
        for _ in range(n):
            depth = np.random.randint(min_depth, max_depth + 1)
            truth_rate = np.random.randint(int(min_truth), int(max_truth) + 1) / 100
            level = np.random.randint(min_levels, max_levels+1)
            bdd = Bdd.create_bdd(depth=depth, truth_rate=truth_rate)
            process_time = time.time()
            result= evaluate(partial(f, level=level), bdd)
            process_time = time.time() - process_time
            if result:
                result_dict = result.to_dict(digits=digits)
                result_dict['levels'] = level
                result_dict['time'] = round(process_time,2)
                writer.writerow(result_dict)



def load_csv(file):
    return pd.read_csv(file)




if __name__ == '__main__':
    from bdd import Bdd

    np.random.seed(1234)
    while True:
        approximation_eval(Bdd.approximation1, min_depth=10, max_depth=100, min_truth=5, max_truth=95, n=50, min_levels=1, max_levels=3)
        break
    data = load_csv('stats.txt')
    sns.lmplot(y = 'chance_false_true_output', x='node_reduction',data=data, hue='levels')
    plt.show()