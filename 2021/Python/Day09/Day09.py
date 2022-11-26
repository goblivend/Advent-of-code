from typing import List

inputFile = "../../Data/Day09/input.txt"
exampleFile  = "../../Data/Day09/ExampleFile.txt"

def print_board(data) :
    for line in data :
        for digit in line :
            print(digit, end="")
        print()

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [[int(digit) for digit in list(line.replace('\n', ''))] for line in lines]

def isLower(data : List[List[int]], x : int, y : int, sizex : int, sizey : int) -> bool:
    if y-1 >= 0 and data[y-1][x] <= data[y][x] : return False
    if x-1 >= 0 and data[y][x-1] <= data[y][x] : return False
    if y+1 < sizey and data[y+1][x] <= data[y][x] : return False
    if x+1 < sizex and data[y][x+1] <= data[y][x] : return False
    return True

def NbLower(fileName) :
    data = GetData(fileName)
    res = 0
    sizex, sizey = len(data[0]), len(data)
    for x in range(sizex) :
        for y in range(sizey) :
            if isLower(data, x ,y, sizex, sizey) :
                res += data[y][x] +1
    print("The number of lower points is :", res)

NbLower(inputFile)

def GetBasinSize(data, M, x, y, sizex, sizey) :
    """
    x and y in sizex, sizey
    """
    #print_board(M)
    if data[y][x] == 9 or M[y][x] : return 0
    #print(data[y][x], x, y, M[y][x])
    M[y][x] = True
    size = 1
    if y-1 >= 0 and data[y-1][x] >= data[y][x] : size += GetBasinSize(data, M, x, y-1, sizex, sizey)
    if x-1 >= 0 and data[y][x-1] >= data[y][x] : size += GetBasinSize(data, M, x-1, y, sizex, sizey)
    if y+1 < sizey and data[y+1][x] >= data[y][x] : size += GetBasinSize(data, M, x, y+1, sizex, sizey)
    if x+1 < sizex and data[y][x+1] >= data[y][x] : size += GetBasinSize(data, M, x+1, y, sizex, sizey)
    return size

def LowerSize(fileName) :
    data = GetData(fileName)
    xxl, xl, l = 0, 0, 0
    sizex, sizey = len(data[0]), len(data)
    M = [[]]*sizey
    for i in range(sizey) :
        M[i] = [False]*sizex
    for x in range(sizex) :
        for y in range(sizey) :
            if data[y][x] != 9 and isLower(data, x ,y, sizex, sizey) :
                size = GetBasinSize(data, M, x, y, sizex, sizey)
                if size > xxl : l, xl, xxl =  xl, xxl, size
                elif size > xl : l, xl = xl, size
                elif size > l: l = size
    print("The product of the sizes of the 3 bigest low points is :", l * xl * xxl, "and their respective size is", l, xl, xxl)

LowerSize(inputFile)
