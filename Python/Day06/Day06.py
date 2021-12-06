inputFile = "../../Data/Day06/input.txt"
exampleFile  = "../../Data/Day06/ExampleFile.txt"

inputData = 1,4,1,1,1,1,1,1,1,4,3,1,1,3,5,1,5,3,2,1,1,2,3,1,1,5,3,1,5,1,1,2,1,2,1,1,3,1,5,1,1,1,3,1,1,1,1,1,1,4,5,3,1,1,1,1,1,1,2,1,1,1,1,4,4,4,1,1,1,1,5,1,2,4,1,1,4,1,2,1,1,1,2,1,5,1,1,1,3,4,1,1,1,3,2,1,1,1,4,1,1,1,5,1,1,4,1,1,2,1,4,1,1,1,3,1,1,1,1,1,3,1,3,1,1,2,1,4,1,1,1,1,3,1,1,1,1,1,1,2,1,3,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,5,1,1,1,2,2,1,1,3,5,1,1,1,1,3,1,3,3,1,1,1,1,3,5,2,1,1,1,1,5,1,1,1,1,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,1,1,1,1,5,1,4,3,3,1,3,4,1,1,1,1,1,1,1,1,1,1,4,3,5,1,1,1,1,1,1,1,1,1,1,1,1,1,5,2,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,2,1,4,4,1,1,1,1,1,1,1,5,1,1,2,5,1,1,4,1,3,1,1
exampleData = 3,4,3,1,2

def GetData(fileName) :
    file = open(fileName, 'r')
    file.readline()
    data = file.readline().replace('\n', '').split(",")
    file.close()

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

def puzzle1(fileName, time) :
    return puzzle1(GetData(fileName), time)

def puzzle1(data, time) :
    for _ in range(time) :
        data = OneDay(data)
    print(datalen(data))

puzzle1(inputData, 256)