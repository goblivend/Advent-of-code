from typing import List

inputFile = "../../Data/Day16/input.txt"
exampleFile = "../../Data/Day16/ExampleFile.txt"
exampleFile2 = "../../Data/Day16/ExampleFile2.txt"
exampleFile3 = "../../Data/Day16/ExampleFile3.txt"


def GetData(fileName) :
    with open(fileName, 'r') as file :
        line = file.readline().replace('\n', '')
    return line

def GetHexa(line) :
    res = ""
    for c in line :
        res +=str(bin(int(c, 16))[2:].zfill(4))
    print(res)
    return res


def GetPackageVersions(binline, i, c=0) :
    if i+10 >= len(binline) :
        return (0, len(binline)-1)
    start = i
    version = int(binline[i:i+3], 2)
    id = int(binline[i+3:i+6], 2)
    i+=6
    if id == 4 :
        keepgoing = True

        while keepgoing :
            keepgoing = binline[i] != "0"
            i += 5
    else :
        byte_size = 4 if id == 4 else 15 if binline[i] == "0" else 11
        nbsub = int(binline[i+1:i+byte_size+1], 2)
        i += byte_size+1
        for _ in range(nbsub):
            if i> len(binline):
                break
            (newversion, finali) = GetPackageVersions(binline, i, c+1)
            c+=1
            i = finali
            version += newversion
    return (version, i)




def puzzle1(fileName) :
    line = GetData(fileName)
    binline = GetHexa(line)
    res = 0
    i = 0
    length = len(binline)
    (version, i) = GetPackageVersions(binline, i)
    print()
    print("the package version is", version)


puzzle1(exampleFile3)




def GetPackageValues(binline, i, c=0) :
    if i+10 >= len(binline) :
        return (0, len(binline)-1)
    start = i
    version = int(binline[i:i+3], 2)
    id = int(binline[i+3:i+6], 2)
    i+=6
    if id == 4 :
        keepgoing = True

        while keepgoing :
            keepgoing = binline[i] != "0"
            i += 5
    else :
        byte_size = 4 if id == 4 else 15 if binline[i] == "0" else 11
        nbsub = int(binline[i+1:i+byte_size+1], 2)
        i += byte_size+1
        for _ in range(nbsub):
            if i> len(binline):
                break
            (newversion, finali) = GetPackageVersions(binline, i, c+1)
            c+=1
            i = finali
            version += newversion
    return (version, i)

def puzzle2(fileName) :
    line = GetData(fileName)
    binline = GetHexa(line)
    res = 0
    i = 0
    length = len(binline)
    (values, i) = GetPackageValues(binline, i)
    print()
    print("the package values is", values)


puzzle2(exampleFile3)

"""
def puzzle1(fileName) :
    line = GetData(fileName)
    binline = GetHexa(line)
    res = 0
    i = 0
    length = len(binline)
    while i + 6< length :
        pack = int(binline[i:i+3], 2)
        res += pack
        if pack == 0 :
            break
        id = int(binline[i+3:i+6], 2)
        print(pack)
        print(id)
        i += 6
        numbers = []
        if id == 4 :
            while keepgoing :
                keepgoing = binline[i] != "0"
                numbers.append(binline[i:i+5]) # change i by i+1 to remove first bit
                i += 5
        else :
            byte_size = 4 if id == 4 else 15 if binline[i] == "0" else 11
            first = False
            i += 1
            nbpackage = int(binline[i:i+byte_size], 2)
            print(binline[i:i+byte_size], nbpackage)
            i += byte_size
            for _ in range(nbpackage) :
                numbers.append(binline[i:i+byte_size+1]) # change i by i+1 to remove first bit
                i += byte_size+1
        #print(numbers)
        #print(int("".join(numbers), 2))
        print()
    print(res)


puzzle1(exampleFile3)"""