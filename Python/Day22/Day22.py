from typing import List

inputFile = "../../Data/Day22/input.txt"
exampleFile = "../../Data/Day22/ExampleFile.txt"
exampleFile2 = "../../Data/Day22/ExampleFile2.txt"


def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    steps = [('on' in line, line.replace('on ', '').replace('off ', '').replace('\n', '').split(',')) for line in lines]
    #      [(is on, xmin, xmax, ymin, ymax, zmin, zmax)]
    return [(on,
            (int(ranges[0].replace('x=', '').split('..')[0]), int(ranges[0].replace('x=', '').split('..')[1])),
            (int(ranges[1].replace('y=', '').split('..')[0]), int(ranges[1].replace('y=', '').split('..')[1])),
            (int(ranges[2].replace('z=', '').split('..')[0]), int(ranges[2].replace('z=', '').split('..')[1]))
    ) for (on, ranges) in steps]

def cleanData(data) :
    data = [(ison, x, y, z) for ison, x, y, z in data if (x[1]>-50 and x[0]<50) and (y[0]>-50 and y[1]<50) and (z[0]>-50 and z[1]<50)]
    data = [(ison, (x[0] if x[0] >= -50 else -50, x[1] if x[1] <=50 else 50), y, z) for ison, x, y, z in data]
    data = [(ison, x, (y[0] if y[0] >= -50 else -50, y[1] if y[1] <=50 else 50), z) for ison, x, y, z in data]
    data = [(ison, x, y, (x[0] if x[0] >= -50 else -50, z[1] if z[1] <=50 else 50)) for ison, x, y, z in data]
    return data

"""
def split(square, step) :
    (xsq, ysq, zsq) = square
    (xstep, ystep, zstep) = step
    inter = ((max(xstep[0], xsq[0]), min(xstep[1], xsq[1])), # setting the intersection square
            (max(ystep[0], ysq[0]), min(ystep[1], ysq[1])),
            (max(zstep[0], zsq[0]), min(zstep[1], zsq[1])))

    step = [
        ((xstep[0], xsq[0]), (ystep[0], ysq[0]), (zstep[0], zsq[0])),
        ((xstep[0], xsq[0]), (ysq[0], ysq[1]), (zstep[0], zsq[0])),
        ((xstep[0], xsq[0]), (ysq[0], ystep[1]), (zstep[0], zsq[0])),

        ((xsq[0], xsq[1]), (ystep[0], ysq[0]), (zstep[0], zsq[0])),
        ((xsq[0], xsq[1]), (ysq[0], ysq[1]), (zstep[0], zsq[0])),
        ((xsq[0], xsq[1]), (ysq[0], ystep[1]), (zstep[0], zsq[0])),

        ((xsq[1], xstep[1]), (ystep[0], ysq[0]), (zstep[0], zsq[0])),
        ((xsq[1], xstep[1]), (ysq[0], ysq[1]), (zstep[0], zsq[0])),
        ((xsq[1], xstep[1]), (ysq[0], ystep[1]), (zstep[0], zsq[0])),


        ((xstep[0], xsq[0]), (ystep[0], ysq[0]), (zsq[0], zsq[1])),
        ((xstep[0], xsq[0]), (ysq[0], ysq[1]), (zsq[0], zsq[1])),
        ((xstep[0], xsq[0]), (ysq[0], ystep[1]), (zsq[0], zsq[1])),

        ((xsq[0], xsq[1]), (ystep[0], ysq[0]), (zsq[0], zsq[1])),
        ((xsq[0], xsq[1]), (ysq[0], ysq[1]), (zsq[0], zsq[1])),
        ((xsq[0], xsq[1]), (ysq[0], ystep[1]), (zsq[0], zsq[1])),

        ((xsq[1], xstep[1]), (ystep[0], ysq[0]), (zsq[0], zsq[1])),
        ((xsq[1], xstep[1]), (ysq[0], ysq[1]), (zsq[0], zsq[1])),
        ((xsq[1], xstep[1]), (ysq[0], ystep[1]), (zsq[0], zsq[1])),


        ((xstep[0], xsq[0]), (ystep[0], ysq[0]), (zsq[1], zstep[1])),
        ((xstep[0], xsq[0]), (ysq[0], ysq[1]), (zsq[1], zstep[1])),
        ((xstep[0], xsq[0]), (ysq[0], ystep[1]), (zsq[1], zstep[1])),

        ((xsq[0], xsq[1]), (ystep[0], ysq[0]), (zsq[1], zstep[1])),
        ((xsq[0], xsq[1]), (ysq[0], ysq[1]), (zsq[1], zstep[1])),
        ((xsq[0], xsq[1]), (ysq[0], ystep[1]), (zsq[1], zstep[1])),

        ((xsq[1], xstep[1]), (ystep[0], ysq[0]), (zsq[1], zstep[1])),
        ((xsq[1], xstep[1]), (ysq[0], ysq[1]), (zsq[1], zstep[1])),
        ((xsq[1], xstep[1]), (ysq[0], ystep[1]), (zsq[1], zstep[1])),
    ]


    square = [
        ((xsq[0], xstep[0]), (ysq[0], ystep[0]), (zsq[0], zstep[0])),
        ((xsq[0], xstep[0]), (ystep[0], ystep[1]), (zsq[0], zstep[0])),
        ((xsq[0], xstep[0]), (ystep[0], ysq[1]), (zsq[0], zstep[0])),

        ((xstep[0], xstep[1]), (ysq[0], ystep[0]), (zsq[0], zstep[0])),
        ((xstep[0], xstep[1]), (ystep[0], ystep[1]), (zsq[0], zstep[0])),
        ((xstep[0], xstep[1]), (ystep[0], ysq[1]), (zsq[0], zstep[0])),

        ((xstep[1], xsq[1]), (ysq[0], ystep[0]), (zsq[0], zstep[0])),
        ((xstep[1], xsq[1]), (ystep[0], ystep[1]), (zsq[0], zstep[0])),
        ((xstep[1], xsq[1]), (ystep[0], ysq[1]), (zsq[0], zstep[0])),


        ((xsq[0], xstep[0]), (ysq[0], ystep[0]), (zstep[0], zstep[1])),
        ((xsq[0], xstep[0]), (ystep[0], ystep[1]), (zstep[0], zstep[1])),
        ((xsq[0], xstep[0]), (ystep[0], ysq[1]), (zstep[0], zstep[1])),

        ((xstep[0], xstep[1]), (ysq[0], ystep[0]), (zstep[0], zstep[1])),
        ((xstep[0], xstep[1]), (ystep[0], ystep[1]), (zstep[0], zstep[1])),
        ((xstep[0], xstep[1]), (ystep[0], ysq[1]), (zstep[0], zstep[1])),

        ((xstep[1], xsq[1]), (ysq[0], ystep[0]), (zstep[0], zstep[1])),
        ((xstep[1], xsq[1]), (ystep[0], ystep[1]), (zstep[0], zstep[1])),
        ((xstep[1], xsq[1]), (ystep[0], ysq[1]), (zstep[0], zstep[1])),


        ((xsq[0], xstep[0]), (ysq[0], ystep[0]), (zstep[1], zsq[1])),
        ((xsq[0], xstep[0]), (ystep[0], ystep[1]), (zstep[1], zsq[1])),
        ((xsq[0], xstep[0]), (ystep[0], ysq[1]), (zstep[1], zsq[1])),

        ((xstep[0], xstep[1]), (ysq[0], ystep[0]), (zstep[1], zsq[1])),
        ((xstep[0], xstep[1]), (ystep[0], ystep[1]), (zstep[1], zsq[1])),
        ((xstep[0], xstep[1]), (ystep[0], ysq[1]), (zstep[1], zsq[1])),

        ((xstep[1], xsq[1]), (ysq[0], ystep[0]), (zstep[1], zsq[1])),
        ((xstep[1], xsq[1]), (ystep[0], ystep[1]), (zstep[1], zsq[1])),
        ((xstep[1], xsq[1]), (ystep[0], ysq[1]), (zstep[1], zsq[1])),
    ]



    return ({(x, y, z) for (x, y, z) in square if x[0] < x[1] and y[0] < y[1] and z[0] < z[1] and (x, y, z) != inter},
            inter,
            {(x, y, z) for (x, y, z) in step if x[0] < x[1] and y[0] < y[1] and z[0] < z[1] and (x, y, z) != inter})



def UpdateStep(squares, step) :
    ison, x, y, z = step # unpacking input
    stepSquares = {(x, y, z)} # the new form of the add
    newSquares = set() # final result
    for square in squares : #loop on each current squares present
        newStepSquares = set()
        for newstep in stepSquares : #loop on each step square (at first only one, but might be split after)
            newsquare, inter, stepSquare = split(square, newstep) # splitting everything
            newStepSquares.update(stepSquare) # updating the steps
            newSquares.update(newsquare) # updating the final result
            if ison :   # adding the intersection if we want to
                newSquares.add(inter)
        stepSquares = newStepSquares
    newSquares.update(stepSquares)
    return newSquares

def Size(squares) :
    size = 0
    for x, y, z in squares :
        size += (x[1]-x[0]+1)*(y[1]-y[0]+1)*(z[1]-z[0]+1)
    return size

def puzzle1(fileName) :
    data = GetData(fileName)
    data = cleanData(data)
    squares = set()
    i=0
    for step in data :
        print(i, len(squares))
        squares = UpdateStep(squares, step)
        i+=1
    print(Size(squares))



puzzle1(exampleFile2)"""

def UpdateStep(mat, step) :
    ison, x, y, z = step
    for i in range(x[0], x[1]+1) :
        for j in range(y[0], y[1]+1) :
            for k in range(z[0], z[1]+1) :
                mat[k][j][i] = ison


def Size(mat) :
    size = 0
    [(size:=elt+size) for plane in mat for line in plane for elt in line]
    return size

def GetMat() :
    return [[[False]*100 for _ in range(100)]for _ in range(100)]

def puzzle1(fileName) :
    data = GetData(fileName)
    data = cleanData(data)
    mat = GetMat()
    for step in data :
        UpdateStep(mat, step)
    print(Size(mat))



puzzle1(inputFile)
