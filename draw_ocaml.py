import pygraphviz as pgv

def load_file (file):
    with open(file, 'r') as file:
        pass

def drawfile=None, score=False):
    if not file:
        file = 'bdd.png'
    g = pgv.AGraph(strict=False, directed=True)
    g.node_attr['shape'] = 'circle'
    g.node_attr['style'] = 'filled'
    g.node_attr['colorscheme'] = 'set312'

    for node in self.get_nodes(key=lambda x: x.uid, terminal=True):

        g.add_node('%d' % node.uid)
        new_node = g.get_node(node.uid)
        new_node.attr['fillcolor'] = (self.get_var_index(node.var) % 12) + 1
        if node.uid in [0, 1]:
            new_node.attr['fillcolor'] = 'White'
            new_node.attr['shape'] = 'doublecircle'
            new_node.attr['label'] = ['F', 'T'][node.uid]

        else:
            if score:
                label = (node.var, node.references, node.false_paths, node.true_paths, node.references)
            else:
                label = node.var
            g.get_node(node.uid).attr['label'] = label

    for node in self.get_nodes(key=lambda x: x.uid):
        if isinstance(node, Terminal):
            continue
        g.add_edge('%d' % node.uid, '%d' % node.low.uid, style='dotted')
        g.add_edge('%d' % node.uid, '%d' % node.high.uid)

    g.draw(file, g.layout(prog='dot'))