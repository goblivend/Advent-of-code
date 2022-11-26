from typing import List

inputFile = "../../Data/Day20/input.txt"
exampleFile = "../../Data/Day20/ExampleFile.txt"

def print_board(data) :
    for line in data :
        for digit in line :
            print('.' if digit=='0' else '#', end="")
        print()
    print()

def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()

    enhancing = ['1' if c=='#' else '0' for c in lines[0].replace('\n', '')]
    board = [['1' if c=='#' else '0' for c in line.replace('\n', '')] for line in lines[2:]]
    return board, enhancing

def increaseBoard(board, n) :
    for i in range(len(board)) :
        for _ in range(2*n) :
            board[i].insert(0, '0')
            board[i].append('0')

    newl = len(board[0])
    for _ in range(2*n) :
        board.insert(0, ['0']*newl)
        board.append(['0']*newl)

def newB(width, height) :
    b = []
    for i in range(height) :
        b.append(['0']*width)
    return b

neighbours = [(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

def GetValue(board, x, y, w, h) :
    res = ""
    for i, j in neighbours :
        newx, newy = x+i, y+j
        if 0 <= newx and newx < w and 0 <= newy and newy < h :
            res += board[newy][newx]
    return int(res, 2)

def enhance(board, enhancing) :
    w, h = len(board[0]), len(board)
    b = newB(w, h)
    for i in range(w) :
        for j in range(h) :
            idx = GetValue(board, i, j, w, h)
            b[j][i] = enhancing[idx]
    return b

def nblight(board) :
    res =0
    for j in range(len(board)) :
        for i in range(len(board[0])) :
            res += int(board[j][i])
    return res

def puzzle1(fileName, iterations) :
    board, enhancing = GetData(fileName)
    print_board(board)
    increaseBoard(board, iterations)
    for _ in range(iterations) :

        board = enhance(board, enhancing)
        print(nblight(board))

puzzle1(inputFile, 50)


