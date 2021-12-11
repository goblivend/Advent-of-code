from typing import List

inputFile = "../../Data/Day11/input.txt"
exampleFile  = "../../Data/Day11/ExampleFile.txt"

neighbours = [(-1, -1),(-1, 0),(-1, 1),(-1, -1),(-1, -1),(-1, -1),(-1, -1),(-1, -1),(-1, -1),(-1, -1),(-1, -1),]

def print_board(data) :
    for line in data :
        for digit in line :
            print(digit, end="")
        print()

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [[int(digit) for digit in list(line.replace('\n', ''))] for line in lines]

def handleincrease(data, flashing, i, j) :
    data[i][j] += 1
    if data[i][j] > 9 and (i, j) not in flashing :
        flashing.append((i, j))

def increaseNeighbour(data, flashing, i, j) :
    for deltaI in range(-1, 2) :
        for deltaJ in range(-1, 2) :
            if i + deltaI >= 0 and j + deltaJ >= 0 and i+deltaI < len(data) and j+deltaJ < len(data[0]) and data[i+deltaI][j+deltaJ] != 0 : handleincrease(data, flashing, i+deltaI, j+deltaJ)

def step(data) :
    flashing = []
    flashes = 0
    for i in range(len(data)) :
        for j in range(len(data[0])) :
            data[i][j] += 1
            if data[i][j] > 9 :
                flashing.append((i, j))

    while len(flashing) != 0 :
        (i, j) = flashing.pop()
        flashes += 1
        data[i][j] = 0
        increaseNeighbour(data, flashing, i, j)

    return flashes

def puzzle1(fileName, steps) :
    data = GetData(fileName)
    flashes = 0
    for _ in range(steps) :
        flashes += step(data)
    print("There is", flashes, "flashes occuring during the", steps, "first steps")
puzzle1(inputFile, 100)

def GetFullFlash(fileName) :
    data = GetData(fileName)
    i = 0
    flashes = 0
    while flashes != len(data) * len(data[0]) :
        flashes = step(data)
        i += 1
    print("You'd need", i, "in order to have all of them flashing at the same time")

GetFullFlash(inputFile)
