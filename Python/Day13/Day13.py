from typing import List

import time

inputFile = "../../Data/Day13/input.txt"
exampleFile = "../../Data/Day13/ExampleFile.txt"
exampleFile2 = "../../Data/Day13/ExampleFile2.txt"

def print_board(data) :
    for line in data :
        for elt in line :
            print('#' if elt else '.', end="")
        print()

def GetData(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    dots = [(int(line.split(',')[0]), int(line.replace('\n', '').split(',')[1])) for line in lines if ',' in line]
    folds = [(line.split()[2][0], int(line.split()[2].split('=')[1])) for line in lines if '=' in line]
    return dots, folds


def GetBoard(dots) :
    maxY = max(dot[1] for dot in dots) + 1
    maxX = max(dot[0] for dot in dots) + 1
    board = [[False]*maxX for _ in range(maxY)]
    for x, y in dots :
        board[y][x] = True
    return board

def foldx(board, col) :
    newboard = [[False]*col for _ in range(len(board))]
    for x in range(col) :
        for y in range(len(board)) :
            newboard[y][col - x - 1] |= board[y][col - x - 1]
            if col + x + 1 < len(board[0]) : newboard[y][col - x - 1] |= board[y][col + x + 1]
    return newboard

def foldy(board, raw) :
    newboard = [[False]*len(board[0]) for _ in range(raw)]
    for x in range(len(board[0])) :
        for y in range(raw) :
            newboard[raw - y - 1][x] |= board[raw - y - 1][x]
            if raw + y + 1 < len(board) : newboard[raw - y - 1][x] |= board[raw + y + 1][x]
    return newboard

def fold_board(board, fold) :
    if fold[0] == 'x' : return foldx(board, fold[1])
    else : return foldy(board, fold[1])

def nbDots(board) :
    return sum(sum(line) for line in board)

def puzzle1(fileName) :
    dots, folds = GetData(fileName)
    board = GetBoard(dots)
    newboard = fold_board(board, folds[0])
    print(nbDots(newboard))


def puzzle2(fileName) :
    dots, folds = GetData(fileName)
    board = GetBoard(dots)
    for fold in folds :
        board = fold_board(board, fold)
    print(nbDots(board))
    print_board(board)

start_time = time.time()
puzzle1(inputFile)

puzzle2(inputFile)

board_time = time.time() - start_time
print('board version ---- %s ----' % board_time)

##########################################################################
################### Using the dots and not the board : ###################
##########################################################################



















def fold_dots(dots, fold) :
    if fold[0] == 'x' :  return {(x,y) if x < fold[1] else (fold[1]-(x-fold[1]),y) for x,y in dots if x <= 2*fold[1]}
    else : return {(x,y) if y < fold[1] else (x,fold[1]-(y-fold[1])) for x,y in dots if y <= 2*fold[1]}

def puzzle1_dots(fileName) :
    dots, folds = GetData(fileName)
    dots = print(len(fold_dots(dots, folds[0])))

def puzzle2_dots(fileName) :
    dots, folds = GetData(fileName)
    dots = set(dots)
    for fold in folds :
        dots = fold_dots(dots, fold)
    print(len(dots))
    print_board(GetBoard(dots))

start_time = time.time()
puzzle1_dots(inputFile)
puzzle2_dots(inputFile)

dots_time = time.time() - start_time
print('dots version ---- %s ----' % dots_time)

print(board_time-dots_time, board_time/dots_time)