import pygraphviz as pgv
import gc
import re

ANIMATION_COUNTER = 0
def load_bdd_dataset(file):
    with open(file) as file:
        line = file.readline()
        print(line)
def draw(bdd, file=None, info=False, dot=False):
    if not file:
        file = 'bdd.png'
    g = pgv.AGraph(strict=False, directed=True)
    g.node_attr['shape'] = 'circle'
    g.node_attr['style'] = 'filled'
    g.node_attr['colorscheme'] = 'set312'

    for node in bdd.get_nodes():

        g.add_node('%d' % node.uid, rank=node.var)
        new_node = g.get_node(node.uid)
        new_node.attr['fillcolor'] = 0 if node.is_terminal() else (node.var % 12 + 1)
        if node.uid in [0, 1]:
            new_node.attr['fillcolor'] = 'White'
            new_node.attr['shape'] = 'doublecircle'
            new_node.attr['label'] = ['F', 'T'][node.uid]

        else:
            label = node.var if not info else ("D:{}\nP:{}\nFT:{} {}\nS:{}".format(bdd.info[node.uid].dom_score,bdd.info[node.uid].paths,bdd.info[node.uid].false_paths, bdd.info[node.uid].true_paths, bdd.info[node.uid].score))#len(bdd.info[node.uid].parents))#len(gc.get_referrers(node))-2) #bdd.info[node.uid]
            g.get_node(node.uid).attr['label'] = label

    for node in bdd.get_nodes():
        if node.is_terminal():
            continue
        g.add_edge('%d' % node.uid, '%d' % node.low.uid, style='dotted')
        g.add_edge('%d' % node.uid, '%d' % node.high.uid)
    for var in bdd.var_pool.keys():
        g.add_subgraph([node.uid for node in bdd.var_pool[var]], rank="same")
    if not dot:
        g.draw(file, g.layout(prog='dot'))
    else:
        g.write("file.dot")

if __name__ == '__main__':
    file = "/Users/oliverheidemanns/GitHub/BddApprox/OCaml/test2.dot"
    png_file = "/Users/oliverheidemanns/GitHub/BddApprox/OCaml/test2.png"
    G = pgv.AGraph(file)
    G.draw(png_file, G.layout(prog='dot'))