from typing import List

def print_map(board: List[List[bool]]) :
    for y in range(len(board)) :
        for x in range(len(board[0])) :
            print('x' if board[y][x] else '-', end="")
        print()
    print()

def print_board(data) :
    for line in data :
        for elt in line :
            print('#' if elt else '.', end="")
        print()


def GetIntArr(fileName : str) -> List[List[int]]:
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [[int(digit) for digit in list(line.replace('\n', ''))] for line in lines if line]