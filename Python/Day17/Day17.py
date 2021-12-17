from typing import List
from collections import namedtuple

inputFile = "../../Data/Day17/input.txt"
exampleFile = "../../Data/Day17/ExampleFile.txt"
exampleFile2 = "../../Data/Day17/ExampleFile2.txt"
exampleFile3 = "../../Data/Day17/ExampleFile3.txt"

Point = namedtuple('Point', ['x', 'y'])

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        line = file.readline().replace('\n', '').replace('target area: x=', '').replace(' y=', '').split(',')
    vals = [val.split('..') for val in line]
    return [(int(val[0]), int(val[1])) for val in vals]

status = {
    0 : 'Landed',
    1 : 'Overshot',
    2 : 'Got under'
}

def inIt(box, p) :
    return box[0].x <= p[0] and p[0] <= box[1].x and box[0].y <= p[1] and p[1] <= box[1].y

def launch(box, v) :
    curr = [0, 0]
    maxH = -1
    print(box)
    while curr[0] <= box[1].x and curr[1] >= box[1].y :
        if curr[1] > maxH :
            maxH = curr[1]

        if inIt(box, curr) :
            return (0, maxH)
        else :
            curr[0] += v[0]
            curr[1] += v[1]
            v[0] += 1 if v[0] < 0 else 0 if v[0] == 0 else -1
            v[1] -= 1
            print('current point', curr, 'current speed', v)
    if curr[1] < box[1].y :
        return (2, -1)
    if curr[0] > box[1].x:
        return (1, -1)

def findMax(box, v) :
    if v[0] == 0 :
        return (-1, -1, -1)
    maxes = (-1, -1, -1)
    curr = v
    keep = True

    res = launch(box, curr.copy())
    if res[0] == 0 :
        if maxes[2] < res[1] :
            maxes[0], maxes[1], maxes[2] = curr[0], curr[1], res[1]

    if res[0] == 1 and v[0] == 1 :
        return maxes

    newtry = curr.copy()
    newtry[1] += 1
    newtry = findMax(box, newtry)
    if maxes[2] < newtry[2] :
        maxes[0], maxes[1], maxes[2] = newtry[0], newtry[1], newtry[2]

    if res[0] == 2 :
        newtry = curr.copy()
        newtry[0] += 1
        newtry = findMax(box, newtry)
        if maxes[2] < newtry[2] :
            maxes[0], maxes[1], maxes[2] = newtry[0], newtry[1], newtry[2]
    else :
        newtry = curr.copy()
        newtry[0] -= 1
        newtry = findMax(box, newtry)
        if maxes[2] < newtry[2] :
            maxes[0], maxes[1], maxes[2] = newtry[0], newtry[1], newtry[2]

    return maxes

def maxHeight(fileName) :
    field = GetData(fileName)
    box = (Point(min(field[0]), max(field[1])), Point(max(field[0]), min(field[1])))
    print(box)

    res = findMax(box, [1, 1])




    print(res)
maxHeight(exampleFile)
