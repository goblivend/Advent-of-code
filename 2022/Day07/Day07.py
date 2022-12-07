#!/usr/bin/env python3

class Tree:
    def __init__(self, name: str, weight: int, children = [], parent = None):
        self.name = name
        self.weight = weight
        self.children = children
        self.parent = parent

    def __str__(self):
        return self.name + " " + str(self.weight)

    def __repr__(self):
        return __str__(self)


def ParseLs(lines: list, parent: Tree, i: int) -> (list, int):
    children = []
    i += 1
    while i < len(lines) and lines[i].strip()[0] != '$' :
        line = lines[i].strip()
        parts = line.split(" ")
        if len(parts) == 2 :
            name = parts[1]
            if parts[0] == 'dir' :
                weight = 0
            else :
                weight = int(parts[0])
            tree = Tree(name, weight, parent = parent)
            children.append(tree)
        i += 1
    return (children, i)

def FindTree(tree: Tree, name: str) -> Tree:
    for child in tree.children :
        if child.name == name :
            return child
    return None

def ParseInput(lines: list) -> Tree:
    root = Tree("root", 0, [Tree('/', 0)])
    tree = root
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        parts = line.split(" ")
        if parts[0] == '$' :
            if parts[1] == 'ls' :
                (tree.children, i) = ParseLs(lines, tree, i)
            elif parts[1] == 'cd' :
                if parts[2] == '..' :
                    tree = tree.parent
                else :
                    tree = FindTree(tree, parts[2])
                i+=1
        else :
            print("Error: " + line)
    return root


def CalculateWeight(tree: Tree) -> int:
    weight = tree.weight
    for child in tree.children :
        weight += CalculateWeight(child)
    tree.weight = weight
    return weight

def CalculateWeightBellowX(tree: Tree, tresh: int) -> list:
    if len(tree.children) == 0 :
        return []
    l =  [tree.weight] if tree.weight < tresh else []
    for child in tree.children :
        l += CalculateWeightBellowX(child, tresh)
    return l

def CalculateWeightAboveX(tree: Tree, tresh: int) -> list:
    if len(tree.children) == 0 :
        return []
    l =  [tree.weight] if tree.weight >= tresh else []
    for child in tree.children :
        l += CalculateWeightAboveX(child, tresh)
    return l

if __name__ == "__main__" :
    with open("input.txt", "r") as f:
        lines = f.readlines()
    tree = ParseInput(lines)
    print('Total weight : ', CalculateWeight(tree))
    print(tree)
    print('Weight bellow : ', sum(CalculateWeightBellowX(tree, 100000)))
    unused = 70000000 - tree.children[0].weight
    print('Unused : ', unused)
    print('Weight above : ', min(CalculateWeightAboveX(tree, 30000000 - unused)))
