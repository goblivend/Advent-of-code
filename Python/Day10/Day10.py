from typing import List

inputFile = "../../Data/Day10/input.txt"
exampleFile  = "../../Data/Day10/ExampleFile.txt"

opening = ['{', '(', '[', '<']
closing = ['}', ')', ']', '>']

errorTable = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137,
}
fixingTable = {
    ')': 1,
    ']': 2,
    '}': 3,
    '>': 4,
}

def GetData(fileName) :
    with open(fileName, 'r') as file :
        return [line.replace('\n', '') for line in file.readlines()]

def GetCorruptionScore(line) :
    stack = []
    for char in line :
        if char in opening :
            stack.append(char)
        else :
            if len(stack) == 0 : return 0
            elif char != closing[opening.index(stack[-1])] : return errorTable[char]
            else : stack.pop()
    return 0

def pointCorruptedLines(fileName) :
    data = GetData(fileName)
    res = 0
    for line in data :
        res += GetCorruptionScore(line)
    print(res)

pointCorruptedLines(inputFile)

def filterCorruptedLines(fileName) :
    return [line for line in GetData(fileName) if GetCorruptionScore(line) == 0]

def fixline(line) :
    stack = []
    for char in line :
        if char in opening : stack.append(char)
        else : stack.pop()
    res = 0
    for i in range(len(stack)-1, -1, -1) :
        newchar = closing[opening.index(stack[i])]
        line += newchar
        res = res*5 + fixingTable[newchar]
    return (line, res)

def fixIncompletelines(fileName) :
    data = filterCorruptedLines(fileName)
    scores = []
    for i in range(len(data)) :
        (data[i], tempres) = fixline(data[i])
        scores.append(tempres)
    scores.sort()
    print("The completion of not corrupted lines has a middle score of", scores[len(scores)//2])

fixIncompletelines(inputFile)

