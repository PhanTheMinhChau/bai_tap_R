import networkx as nx
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(15,9))
G = nx.DiGraph()
G.add_edges_from(
    [('A', 'B'), ('B', 'D'), ('C', 'B'),('C','D')])
 
val_map = {'A': 1.0,
           'D': 0.5714285714285714,
           'H': 0.0}

values = [val_map.get(node, 0.25) for node in G.nodes()]

pos = nx.spring_layout(G)
nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_color = "green", node_size = 2000)
nx.draw_networkx_labels(G, pos,font_size=30)
nx.draw_networkx_edges(G, pos, edge_color='black', arrows=True)
nx.draw_networkx_edges(G, pos, arrows=False)
plt.show()