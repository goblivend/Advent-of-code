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

def GetGamma(fileName) :
    file = open(fileName, 'r')
    lines = file.readlines()
    nblines = len(lines)
    lenperline = len(lines[0].strip())
    sumlist = [0]*lenperline

    for line in lines:
        line = line.strip()
        for i in range(lenperline) :

            sumlist[i] += int(line[i])

    for i in range(lenperline) :
        sumlist[i] = 1 if sumlist[i] > nblines//2 else 0

    gamma, epsilon = toDecimal(sumlist)

    print("Gamma :" + str(gamma))
    print("Epsilon :" + str(epsilon))
    print(epsilon * gamma)
    return (gamma, epsilon)


print(GetGamma(inputFile))



def GetOxylists(data, sumlist) :
    nblines = len(data)
    length = len(data[0])
    okfound = False
    prveok = []
    idxok = []
    notOkfound = False
    notidxok = []
    prenotok = []

    for i in range(length) :
        prveok = idxok
        prenotok = notidxok

        idxok = []
        notidxok = []

        sumlist[i] = 1 if sumlist[i]+1 > nblines//2 else 0
        for currline in range(nblines) :
            if data[currline][i] != sumlist[i] :
                if not notOkfound and (currline, data[currline]) in prenotok:
                    notidxok.append((currline, data[currline]))
            else :
                if not okfound and (currline, data[currline]) in prveok:
                    idxok.append((currline, data[currline]))

        if len(idxok) == 1 :
            okfound = True
            idxok = prveok
        if len(notidxok) == 1 :
            notOkfound = True
            notidxok = prenotok

    return (idxok, notidxok)

def GetOxylist(data, sumlist) :
    nblines = len(data)
    length = len(data[0])
    okfound = False
    prevok = data

    idxok = []


    for i in range(length) :
        idxok = []


        sumlist[i] = 1 if sumlist[i]+1 > nblines//2 else 0
        print(sumlist[i])
        for currline in range(nblines) :
            if data[currline][i] == sumlist[i] :
#                print("First criteria ok")
                if not okfound :
#                    print("Second criteria")
                    if data[currline] in prevok:
#                        print("Trird criteria")
                        idxok.append(data[currline])

        if len(idxok) == 1 :
            return idxok[0]
        print(idxok)
        prevok = idxok
    return idxok[0]

def GetDioxylist(data, sumlist) :
    nblines = len(data)
    length = len(data[0])
    okfound = False
    prevok = data
    idxok = []

    for i in range(length) :
        idxok = []


        print(sumlist[i])
        for currline in range(nblines) :
            if data[currline][i] != sumlist[i] :
#                print("First criteria ok")
                if not okfound :
#                    print("Second criteria")
                    if data[currline] in prevok:
#                        print("Trird criteria")
                        idxok.append(data[currline])

        if len(idxok) == 1 :
            return idxok[0]
        print(idxok)
        prevok = idxok
    return idxok[0]


def GetOxygen(fileName) :
    file = open(fileName, 'r')
    lines = file.readlines()
    nblines = len(lines)
    lenperline = len(lines[0].strip())
    sumlist = [0]*lenperline

    datalist = []

    for line in lines:
        line = line.strip()
        datalist.append([])
        for i in range(lenperline) :
            datalist[-1].append(int(line[i]))
            sumlist[i] += int(line[i])


    oxygen = GetOxylist(datalist, sumlist)
    dioxygen = GetDioxylist(datalist, sumlist)
    #oxygen, dioxygen = GetOxylists(datalist, sumlist)


    print(oxygen, dioxygen)

    oxy = toDecimal(oxygen)[0]
    dioxy = toDecimal(dioxygen)[0]

    return (oxy, dioxy)


print(GetOxygen(exampleFile))