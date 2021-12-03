inputFile = "../../Data/Day03/input.txt"
exampleFile  = "../../Data/Day03/ExampleFile.txt"
#ls ../../Data/Day03/
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
    file = open(fileName, 'r')
    lines = file.readlines()
    file.close()

    lenperline = len(lines[0].strip())
    datalist = []
    for line in lines:
        line = line.strip()
        datalist.append([])
        for i in range(lenperline) :
            datalist[-1].append(int(line[i]))

    return datalist

def SetSumlistMax(sumlist, nblines) :
    for i in range(len(sumlist)) :
        sumlist[i] = 1 if sumlist[i] >= nblines/2 else 0
    return sumlist

def SetSumlistMin(sumlist, nblines) :
    for i in range(len(sumlist)) :
        sumlist[i] = 1 if sumlist[i] < nblines/2 else 0
    return sumlist

def GetGamma(fileName) :
    datalist = GetData(fileName)
    sumlist = SetSumlistMax(GetSumlist(datalist), len(datalist))
    gamma, epsilon = toDecimal(sumlist)
    print("Gamma :" + str(gamma) + "\n" + "Epsilon :" + str(epsilon) + "\n" + str(epsilon * gamma) + "\n")

GetGamma(inputFile)

def Getlist(data, nblines, SetSum) :
    prevok = data
    idxok = []

    for i in range(len(data[0])) :
        sumlist = SetSum(GetSumlist(prevok), len(prevok))
        idxok = []
        for currline in range(nblines) :
            if data[currline][i] == sumlist[i] and data[currline] in prevok:
                idxok.append(data[currline])

        if len(idxok) == 1 :
            return idxok[0]
        prevok = idxok
    return idxok[0]


def GetOxygen(fileName) :
    datalist = GetData(fileName)  

    oxygen = Getlist(datalist, len(datalist), SetSumlistMax)
    scrubber = Getlist(datalist, len(datalist), SetSumlistMin)

    oxy = toDecimal(oxygen)[0]
    scru = toDecimal(scrubber)[0]

    print("Oxygen :" + str(oxy) + "\n" + "Scrubber :" + str(scru) + "\n" + str(oxy * scru))



GetOxygen(inputFile)