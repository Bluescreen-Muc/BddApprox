import csv

class ApproxInfo:
    def __init__(self, bdd_depth, node_reduction, chance_wrong_output, chance_false_true_output, truth_ratio):
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

    def to_dict(self):
        return {
            'bdd_depth' : self.bdd_depth,
            'node_reduction' : self.node_reduction,
            'chance_wrong_output' : self.chance_wrong_output,
            'chance_false_true_output' : self.chance_false_true_output,
            'truth_ratio' : self.truth_ratio
        }

def evaluate(f, bdd, output=False):
    bdd.count_rec()
    node_stats_before_approx = bdd.info[bdd.root_node.uid]
    node_count_before_approx = bdd.node_count()
    sum_paths = node_stats_before_approx.false_paths + node_stats_before_approx.true_paths
    print(bdd)
    print(f)
    f(bdd)
    bdd.count_rec()
    truth_ratio = node_stats_before_approx.true_paths / sum_paths
    node_stats_after_approx = bdd.info[bdd.root_node.uid]
    node_count_after_approx = bdd.node_count()
    print(node_stats_after_approx)
    print(node_stats_before_approx)
    node_reduction = 1 - node_count_after_approx / node_count_before_approx
    chance_wrong_output = (node_stats_after_approx.true_paths - node_stats_before_approx.true_paths) / sum_paths
    chance_false_true_output = (node_stats_after_approx.true_paths - node_stats_before_approx.true_paths) / node_stats_after_approx.true_paths
    return ApproxInfo(bdd_depth=bdd.depth(), node_reduction=node_reduction, chance_wrong_output=chance_wrong_output,
                      chance_false_true_output=chance_false_true_output, truth_ratio=truth_ratio)


def approximation_eval(f, n=100, min_depth=10, max_depth=10, min_truth=0.5, max_truth=0.5, variant=1):
    import numpy as np
    fields = ['bdd_depth', 'node_reduction', 'chance_wrong_output', 'chance_false_true_output', 'truth_ratio']
    depth = np.random.randint(min_depth, max_depth + 1)
    truth_rate = np.random.randint(int(min_truth * 100), int(max_truth * 100) + 1) / 100
    with open('stats.txt', 'a') as csv_file:
        for _ in range(n):
            bdd = Bdd.create_bdd(depth=depth, truth_rate=truth_rate)
            result= evaluate(f, bdd)
            fieldnames = ['first_name', 'last_name']
            writer = csv.DictWriter(csv_file, fieldnames=fields)
            writer.writerow(result.to_dict())


if __name__ == '__main__':
    from bdd import Bdd
    approximation_eval(Bdd.approximation1, n=10)
