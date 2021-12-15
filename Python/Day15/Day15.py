from typing import List

inputFile = "../../Data/Day15/input.txt"
exampleFile = "../../Data/Day15/ExampleFile.txt"
exampleFile2 = "../../Data/Day15/ExampleFile2.txt"

neighbours = [(1, 0),(0, 1), (-1, 0),(0, -1)]

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [[int(digit) for digit in list(line.replace('\n', ''))] for line in lines if line]

def lowest_path(board) :
    lenx, leny = (len(board[0]), len(board))
    end = (lenx-1, leny-1)
    mylist = [(board[end[1]][end[0]], end)]
    visited = [[False]*lenx for _ in range(leny)]
    visited[end[1]][end[0]] = True
    while mylist :
        (risk, (currX, currY)) = min(mylist)
        mylist.remove((risk, (currX, currY)))

        for x, y in neighbours :
            newX, newY = currX + x, currY + y
            if newX < 0 or lenx <= newX or newY < 0 or leny <= newY or visited[newY][newX] :
                continue
            visited[newY][newX] = True
            mylist.append((board[newY][newX] + risk, (newX, newY)))
            if (newX, newY) == (0, 0) :
                print("At 0, 0 :", mylist[-1][0] - board[0][0])

def puzzle1(fileName) :
    board = GetData(fileName)
    lowest_path(board)

def GetPuzzle2board(board) :
    lenx, leny = (len(board[0]), len(board))
    newboard = [[0]*(len(board[0])*5) for _ in range(len(board)*5)]
    for i in range(lenx*5):
        for j in range(leny*5) :
            newboard[j][i] = (board[j%leny][i%lenx] + i//lenx + j//leny-1) % 9 + 1
    return newboard

def puzzle2(fileName) :
    board = GetData(fileName)
    board = GetPuzzle2board(board)
    lowest_path(board)

import time

start_time = time.time()
puzzle1(inputFile)
print('puzzle1 in', time.time() - start_time)


start_time = time.time()
puzzle2(inputFile)
print('puzzle2 in', time.time() - start_time)