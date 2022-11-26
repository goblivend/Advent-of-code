from typing import List
from collections import namedtuple

inputFile = "../../Data/Day17/input.txt"
exampleFile = "../../Data/Day17/ExampleFile.txt"
velocities = "../../Data/Day17/velocities.txt"

Point = namedtuple('Point', ['x', 'y'])

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        line = file.readline().replace('\n', '').replace('target area: x=', '').replace(' y=', '').split(',')
    vals = [val.split('..') for val in line]
    return [(int(val[0]), int(val[1])) for val in vals]


def inIt(box, p) :
    x, y = p
    return box[0].x <= x and x <= box[1].x and box[0].y >= y and y >= box[1].y

def launch(box, v) :
    curr = [0, 0]
    maxH = -1
    while curr[0] <= box[1].x and curr[1] >= box[1].y :
        if curr[1] > maxH :
            maxH = curr[1]
        if inIt(box, curr) :
            return (0, maxH, curr)
        else :
            curr[0] += v[0]
            curr[1] += v[1]
            v[0] += 1 if v[0] < 0 else 0 if v[0] == 0 else -1
            v[1] -= 1
    if curr[1] < box[1].y :
        return (2, -1, curr)
    if curr[0] > box[1].x:
        return (1, -1, curr)

def findMax(box) :
    y = box[1].y
    stat = 0
    maxes = [-1, -1, 0]
    vels = []

    while True :
        for x in range(box[1].x+1) :
            (stat, height, pos) = launch(box, [x, y])
            if stat == 0 :
                vels.append((x, y))
                if maxes[2] < height :
                    maxes[0], maxes[1], maxes[2] = x, y, height
        if  y > 100:
            break
        y += 1

    return maxes[2], vels

def GetVelocities(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    data = [(int(line.split(',')[0]), int(line.split(',')[1])) for line in lines]
    return data

def maxHeight(fileName) :
    field = GetData(fileName)
    box = (Point(min(field[0]), max(field[1])), Point(max(field[0]), min(field[1])))
    (res, vels) = findMax(box)
    print('', res, len(vels))

    """myvelocities = GetVelocities(velocities)
    myvelocities.sort()

    for vel in myvelocities :
        if vel not in vels :
            print(vel)"""
maxHeight(exampleFile)
#1739




