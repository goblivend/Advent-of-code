from typing import List
import graph

inputFile = "../../Data/Day12/input.txt"
exampleFile  = "../../Data/Day12/ExampleFile.txt"
exampleFile2  = "../../Data/Day12/ExampleFile2.txt"
exampleFile3  = "../../Data/Day12/ExampleFile3.txt"

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [line.replace('\n', '').split('-') for line in lines]

canadd = lambda inpt, labels : inpt not in ["start", "end"] and inpt not in labels

def GetGraph(edges) :
    labels = ["start"]
    for edge in edges :
        if canadd(edge[0], labels) : labels.append(edge[0])
        if canadd(edge[1], labels) : labels.append(edge[1])
    labels.append('end')
    g = graph.Graph(len(labels), False, labels)
    for src, dst in edges :
        g.addedge(labels.index(src), labels.index(dst))
    return g

def Getnbpaths(graph, curr, end, path, double) :
    if curr == end :return 1
    path.append(graph.labels[curr])
    nb = 0
    for adj in graph.adjlists[curr] :
        notseen = (graph.labels[adj].isupper() or graph.labels[adj] not in path)
        if graph.labels[adj] != "start" and (notseen or double):
            nb += Getnbpaths(graph, adj, end, path.copy(), notseen if double else False)
    return nb

def GetPuzzle(fileName) :
    G = GetGraph(GetData(fileName))
    print(Getnbpaths(G, 0, G.order -1, [], False))
    print(Getnbpaths(G, 0, G.order -1, [], True))
GetPuzzle(inputFile)