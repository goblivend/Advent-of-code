inputFile = "../../Data/Day05/input.txt"
exampleFile  = "../../Data/Day05/ExampleFile.txt"

def GetData(fileName) :
    with open(fileName, 'r') as file :
        data = [(int(elt[0]), int(elt[1]), int(elt[2]), int(elt[3])) for elt in [line.replace('\n', '').replace(' -> ', ',').split(",") for line in file.readlines()]]
    xmax, ymax = 0, 0
    [(xmax:=max(xmax, line[0], line[2]), ymax:=max(ymax, line[1], line[3])) for line in data]
    return data, [[0]*(xmax+1) for _ in range(ymax+1)]

def GetOverlap(array) :
    nboverlap = 0
    [[nboverlap:=(nboverlap + int(spot>1)) for spot in line] for line in array]
    print(nboverlap)
    return nboverlap

def Writeline(arr, x1, y1, x2, y2) :
    for x in range(min(x1, x2), max(x1, x2)+1):
        for y in range(min(y1, y2), max(y1, y2)+1) :
            arr[y][x] += 1

def GetLineoverlap(fileName) :
    data, array = GetData(fileName)
    for (x1, y1, x2, y2) in data :
        if x1 == x2 or y1 == y2:
            Writeline(array, x1, y1, x2, y2)
    print("The number of overlap with line is : ", end = "")
    return GetOverlap(array)
GetLineoverlap(inputFile)

def WriteDiag(arr, x1, y1, x2, y2) :
    for x in range(abs(x2-x1)+1):
        arr[y2+((-1)*2*(y2-y1 > 0) +1) * x][x2+((-1)*2*(x2-x1 > 0) +1) * x] +=1

def GetDiagoverlap(fileName) :
    data, array = GetData(fileName)
    for (x1, y1, x2, y2) in data :
        if x1 == x2 or y1 == y2:
            Writeline(array, x1, y1, x2, y2)
        elif x1-x2 == y1-y2 :
            WriteDiag(array, x1, y1, x2, y2)
        elif x1-x2 == y2-y1 :
            WriteDiag(array, x2, y2, x1, y1)
    print("The number of overlap with diagonal as well is : ", end = "")
    return GetOverlap(array)
GetDiagoverlap(inputFile)