#!/usr/bin/env python3


with open('input.txt', 'r') as f :
    lines = f.readlines()
input = [[int(c) for c in line.strip()]for line in lines]
visibility = [[False for _ in line.strip()] for line in lines]

(lenx, leny) =(len(input), len(input[0]))
for x in range(lenx) :
    for y in range(leny) :
        if x == 0 or y == 0 or x == lenx-1 or y == leny-1 :
            visibility[x][y] = True
        curr = input[x][y]
        left = all([input[x2][y] < curr for x2 in range(0, x)])
        right = all(input[x2][y] < curr for x2 in range(x+1, lenx))
        top = all([input[x][y2] < curr for y2 in range(0, y)])
        bottom = all(input[x][y2] < curr for y2 in range(y+1, leny))
        visibility[x][y] = left or right or bottom or top


count = 0
#[[1 for v in line if v] for line in visibility]
for line in visibility :
    for v in line :
        if v :
            count += 1

print(count)


## Part 2

def inRange(input, x, y) :
    return x >= 0 and y >= 0 and x < len(input) and y < len(input[0])

def increateScenic(input, x1, y1, x, y, tox2, toy2) :
    (x2, y2) = (tox2(x), toy2(y))
    if not inRange(input, x2, y2) :
        return 0

    if input[x1][y1] <= input[x2][y2] :
        return 1

    return 1 + increateScenic(input, x1, y1, x2, y2, tox2, toy2)

def identity(x) :
    return x

def fromTop(x) :
    return x + 1 

def fromBottom(x) :
    return x - 1

def scenicScore(input, x, y) :
    left = increateScenic(input,x, y, x, y, fromBottom, identity)

    right = increateScenic(input, x, y,x, y, fromTop, identity)
    top = increateScenic(input, x, y,x, y, identity, fromBottom)
    bottom = increateScenic(input, x, y,x, y, identity, fromTop)
    return left * right * top * bottom



scenicScores = [[scenicScore(input, x, y) for y in range(leny)] for x in range(lenx)]

#[print(scenic) for scenic in scenicScores]
print(max([max(score) for score in scenicScores]))

