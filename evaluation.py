class Stats:

    def __init__(self, nodes, counter):
        self.nodes = nodes
        self.false = counter[0]
        self.true = counter[1]

    def approximation(self, new_nodes, counter):
        node_reduction = 1 - (new_nodes / self.nodes)
        error_rate = (counter[1] - self.true) / (self.false + self.true)
        return node_reduction, error_rate