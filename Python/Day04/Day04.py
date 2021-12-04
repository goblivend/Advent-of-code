inputFile = "../../Data/Day04/input.txt"
exampleFile  = "../../Data/Day04/ExampleFile.txt"

def GetData(fileName) :
    file = open(fileName, 'r')
    line = file.readline()
    line.strip()
    numbers = [int(x) for x in line.split(',') if x.isdigit()]
    boards = []
    line = file.readline()
    while (line) :
        boards.append([])
        for i in range(5) :
            line = file.readline()
            line.strip()
            boards[-1].append([int(x) for x in line.split() if x.isdigit()])
        line = file.readline()
    file.close()
    return numbers, boards

def GetSum(boards, boardnb) :
    sum = 0
    [[sum:=sum+x if x != -1 else sum for x in board] for board in boards[boardnb]]
    return sum

def isDone(board) :
    for i in range(5) :
        isDone = True
        for j in range(5) :
            if board[i][j] != -1 :
                isDone = False
                break
        if isDone :
            return isDone
    for i in range(5) :
        isDone = True
        for j in range(5) :
            if board[j][i] != -1 :
                isDone = False
                break
        if isDone :
            return isDone
    return False

def addnumber(boards, number) :
    boardnb = []
    for i in range(len(boards)) :
        if len(boards[i]) == 0 :
            continue
        for line in boards[i] :
            for j in range(len(line)) :
                if line[j] == number :
                    line[j] = -1
                    if  isDone(boards[i]) :
                        boardnb.append(i)
                    break

    return boardnb

def BingoWin(fileName) :
    numbers, boards = GetData(fileName)
    for number in numbers :
        boardnb = addnumber(boards, number)
        if boardnb :
            sum = GetSum(boards, boardnb[0])
            print("Sum : " + str(sum) + "\nboard : " + str(boardnb[0]) + "\nnumber : " + str(number) + "\ntotal = " + str(number * sum) + '\n')
            return

BingoWin(exampleFile)

def BingoLose(fileName) :
    numbers, boards = GetData(fileName)
    workingboards = len(boards)
    for number in numbers :
        boardnb = addnumber(boards, number)
        if boardnb :
            for bnb in boardnb :
                if workingboards > 1 :
                    boards[bnb] = []
                    workingboards -=1
                else :
                    sum = GetSum(boards, bnb)
                    print("Sum : " + str(sum) + "\nboard : " + str(bnb) + "\nnumber : " + str(number) + "\ntotal = " + str(number * sum))
                    return

BingoLose(exampleFile)
