import networkx as nx
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(15,9))
G = nx.DiGraph()
G.add_edges_from(
    [('A', 'B'), ('B', 'D'), ('C', 'B')])
# Specify the edges you want here
red_edges = [('C', 'D'),('B', 'C'),('B', 'D'),('A', 'B')]
# Need to create a layout when doing
# separate calls to draw nodes and edges
pos = nx.spring_layout(G)
nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_color = "green", node_size = 2200)
nx.draw_networkx_labels(G, pos, font_size=30)
nx.draw_networkx_edges(G, pos, edgelist=red_edges, edge_color='r', arrows=True, min_target_margin = 30)
plt.show()