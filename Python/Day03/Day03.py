inputFile = "../../Data/Day03/input.txt"
exampleFile  = "../../Data/Day03/ExampleFile.txt"

def toDecimal(l):
    length = len(l)
    gamma = 0
    notGamma = 0
    for i in range(length) :
        gamma += l[i] * 2**(length - i -1)
        notGamma +=(not l[i]) * 2**(length - i - 1)
    return (gamma, notGamma)

def GetSumlist(data) :
    nblines =len(data)
    bitperline =len(data[0])
    sumlist = [0]*bitperline
    for i in range(bitperline) :
        for j in range(nblines) :
            sumlist[i] += data[j][i]
    return sumlist

def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    lenperline = len(lines[0].strip())
    datalist = []
    for line in lines:
        line = line.strip()
        datalist.append([])
        for i in range(lenperline) :
            datalist[-1].append(int(line[i]))
    return datalist

def SetSumlist(sumlist, nblines, isup) :
    for i in range(len(sumlist)) :
        sumlist[i] = (isup and sumlist[i] >= nblines/2) or (not isup and sumlist[i] < nblines/2)
    return sumlist

def GetGamma(fileName) :
    datalist = GetData(fileName)
    sumlist = SetSumlist(GetSumlist(datalist), len(datalist), True)
    gamma, epsilon = toDecimal(sumlist)
    print("Gamma :" + str(gamma) + "\n" + "Epsilon :" + str(epsilon) + "\n" + str(epsilon * gamma) + "\n")

GetGamma(inputFile)

def Getlist(data, nblines, Bound) :
    prevok = data
    listok = []
    for i in range(len(data[0])) :
        sumlist = SetSumlist(GetSumlist(prevok), len(prevok), Bound)
        listok = []
        for currline in range(nblines) :
            if data[currline][i] == sumlist[i] and data[currline] in prevok:
                listok.append(data[currline])
        if len(listok) == 1 :
            return listok[0]
        prevok = listok
    return listok[0]

def GetOxygen(fileName) :
    datalist = GetData(fileName)
    oxygen = toDecimal(Getlist(datalist, len(datalist), True))[0]
    scrubber = toDecimal(Getlist(datalist, len(datalist), False))[0]
    print("Oxygen :" + str(oxygen) + "\n" + "Scrubber :" + str(scrubber) + "\n" + str(oxygen * scrubber))

GetOxygen(inputFile)