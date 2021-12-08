from typing import List

inputFile = "../../Data/Day06/input.txt"
exampleFile  = "../../Data/Day06/ExampleFile.txt"

def GetData(fileName) :
    with open(fileName, 'r') as file :
        data = [int(elt) for elt in file.readline().replace('\n', '').split(",")]
    return CleanData(data)

def CleanData(data) :
    mydata = [0]*9
    for i in [int(elt) for elt in data] :
        mydata[i] += 1
    return mydata

def OneDay(data) :
    return [data[1], data[2], data[3], data[4], data[5], data[6], data[7] + data[0], data[8], data[0]]

def datalen(data) :
    nb = 0
    [nb:=(nb + data[i]) for i in range(len(data))]
    return nb

def puzzle(fileName : str, time : int) :
    data = GetData(fileName)
    for _ in range(time) :
        data = OneDay(data)
    print(datalen(data))

puzzle(inputFile, 256)
