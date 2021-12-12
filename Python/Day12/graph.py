# -*- coding: utf-8 -*-
"""Graph module.

Provide an implementation of graphs with adjacency lists.

In a graph, vertices are considered numbered from 0 to the order of the graph
minus one. The vertex number can then be used to access its adjacency list.

"""


class Graph:
    """ Simple class for graph: adjacency lists

    Attributes:
        order (int): Number of vertices.
        directed (bool): True if the graph is directed. False otherwise.
        adjlists (List[List[int]]): Lists of connected vertices for each vertex.
        labels (list[str]): optionnal vector of vertex labels

    """

    def __init__(self, order, directed, labels=None):
        """Init graph, allocate adjacency lists

        Args:
            order (int): Number of nodes.
            directed (bool): True if the graph is directed. False otherwise.
            labels (list[str]): optionnal vector of vertex labels
        """

        self.order = order
        self.directed = directed
        self.adjlists = []
        for _ in range(order):
            self.adjlists.append([])
        self.labels = labels

    def addedge(self, src, dst):
        """Add egde to graph.

        Args:
            src (int): Source vertex.
            dst (int): Destination vertex.

        Raises:
            IndexError: If any vertex index is invalid.

        """
        if src >= self.order or src < 0:
            raise IndexError("Invalid src index")
        if dst >= self.order or dst < 0:
            raise IndexError("Invalid dst index")

        self.adjlists[src].append(dst)
        if not self.directed and dst != src:
            self.adjlists[dst].append(src)


    def addvertex(self, number=1, labels=None):
        """Add number vertices to graph.

        Args:
            ref (Graph).
            number (int): Number of vertices to add.
            labels (str list)optionnal list of new vertex labels

        """

        # Increment order and extend adjacency list
        self.order += number
        for _ in range(number):
            self.adjlists.append([])
        if labels:
            self.labels += labels

    def removeedge(self, src, dst):
        """Remove egde from the graph.

        Args:
            src (int): Source vertex.
            dst (int): Destination vertex.

        Raises:
            IndexError: If any vertex index is invalid.

        """

        if src >= self.order or src < 0:
            raise IndexError("Invalid src index")
        if dst >= self.order or dst < 0:
            raise IndexError("Invalid dst index")
        if dst in self.adjlists[src]:
            self.adjlists[src].remove(dst)
            if not self.directed and dst != src:
                self.adjlists[dst].remove(src)

def sortgraph(G):
    """
    sorts adjacency lists -> to have same results as those asked in tutorials/exams
    """
    for i in range(G.order):
        G.adjlists[i].sort()


def todot(G):
    """Dot format of graph.

    Args:
        Graph

    Returns:
        str: String storing dot format of graph.

    """

    if G.directed:
        link = " -> "
        dot = "digraph {\n"
    else:
        link = " -- "
        dot = "graph {\n"

    for s in range(G.order):
        if G.labels:
            dot += "  " + str(s) + '[label = "' + G.labels[s] + '"]\n'
        else:
            dot += "  " + str(s) + '\n'
        for adj in G.adjlists[s]:
            if G.directed or adj <= s:
                dot += str(s) + link + str(adj) + "\n"

    dot += "}"
    return dot


def display(G, eng=None):
    """
    *Warning:* Made for use within IPython/Jupyter only.
    eng: graphivz.Source "engine" optional argument (try "neato", "fdp", "sfdp", "circo")

    """

    try:
        from graphviz import Source
        from IPython.display import display
    except:
        raise Exception("Missing module: graphviz and/or IPython.")
    display(Source(todot(G), engine=eng))


# load / save gra format

def load(filename):
    """Build a new graph from a GRA file.

    Args:
        filename (str): File to load.

    Returns:
        Graph: New graph.

    Raises:
        FileNotFoundError: If file does not exist.

    """

    f = open(filename)
    lines = f.readlines()
    f.close()

    infos = {}
    i = 0
    while '#' in lines[i]:
        (key, val) = lines[i][1:].strip().split(": ")
        infos[key] = val
        i += 1

    directed = bool(int(lines[i]))
    order = int(lines[i+1])

    if infos and "labels" in infos:
        labels = infos["labels"].split(',') #labels is a list of str
        G = Graph(order, directed, labels)  # a graph with labels
    else:
        G = Graph(order, directed)  # a graph without labels
    if infos:
        G.infos = infos

    for line in lines[i+2:]:
        edge = line.strip().split(' ')
        (src, dst) = (int(edge[0]), int(edge[1]))
        G.addedge(src, dst)
    return G


def save(G, fileOut):
    gra = ""
    if G.labels:
        lab = "#labels: "
        for i in range(G.order - 1):
            lab += G.labels[i] + ','
        lab += G.labels[-1]
        gra += lab + '\n'
    gra += str(int(G.directed)) + '\n'
    gra += str(G.order) + '\n'
    for s in range(G.order):
        for adj in G.adjlists[s]:
            if G.directed or s >= adj:
                gra += str(s) + " " + str(adj) + '\n'
    fout = open(fileOut, mode='w')
    fout.write(gra)
    fout.close()
