inputFile = "../../Data/Day04/input.txt"
exampleFile  = "../../Data/Day04/ExampleFile.txt"



def printBoard(board) :
    print("[ ", end="")
    for elt in board :
        print(elt)
        print(", ", end="")
    print(" ]")

def GetData(fileName) :
    file = open(fileName, 'r')
    line = file.readline()
    line.strip()
    numbers = [int(x) for x in line.split(',')]
    boards = []

    line = file.readline()
    while (line) :
        currboard = []
        for i in range(5) :
            line = file.readline()
            line.strip()

            str_elts = list(map(int, line.split()))
            str_elts[:] = [x for x in str_elts if x not in [' ', '', '\n', '\n\r', '\n']]

            currboard.append(str_elts)
        boards.append(currboard)
        line = file.readline()


    file.close()
    return numbers, boards

def GetSum(boards, boardnb) :
    sum = 0
    for line in boards[boardnb] :
        for elt in line :
            if elt != -1 :
                sum += elt
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



def addnumber(boards, number) :
    boardnb = []
    for i in range(len(boards)) :
        board = boards[i]
        if len(board) == 0 :
            continue
        for line in board :
            for j in range(len(line)) :
                """if line[j] == 16 :
                    print(line[j], number, line[j] == number)"""
                if line[j] == number :
                    line[j] = -1
                #line[j] = -1 if line[j] == number else line[j]
        if  isDone(board) :
            boardnb.append(i)
    return boardnb


def BingoWin(fileName) :
    numbers, boards = GetData(fileName)

    for number in numbers :
        boardnb = addnumber(boards, number)
        if boardnb != [] :
            sum = GetSum(boards, boardnb[0])
            print("Sum : " + str(sum) + "\nboard : " + str(boardnb[0]) + "\nnumber : " + str(number) + "\ntotal = " + str(number * sum))
            return


BingoWin(inputFile)

def workingBoards(boards) :
    nb = 0
    for board in boards :
        #print(len(board))
        nb += len(board) != 0
    return nb

def BingoLose(fileName) :
    numbers, boards = GetData(fileName)
    workingboards = len(boards)

    for number in numbers :
        #print("Adding : " + str(number))
        boardnb = addnumber(boards, number)

        if boardnb != [] :
            for bnb in boardnb :
                print(workingboards)
                if workingboards > 1 :
                    print('Board ' + str(bnb) + ' is HS, with ' + str(number))
                    boards[bnb] = []
                    workingboards -=1
                else :
                    sum = GetSum(boards, bnb)
                    print("Sum : " + str(sum) + "\nboard : " + str(bnb) + "\nnumber : " + str(number) + "\ntotal = " + str(number * sum))
                    print(numbers.index(number))
                    print(len(boards))
                    return

    print(len(numbers))
    print(len(boards))
    """
    for board in boards :
        printBoard(board)

        """

BingoLose(inputFile)