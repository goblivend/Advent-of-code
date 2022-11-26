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
    #print(res)
    return res

def prod(arr) :
    if not arr :
        return 1
    res = 1
    for val in arr:
        res *= val
    return res

version = 0
def GetPackageValues(line) :
    global version

    version += int(line[0:3], 2)
    id = int(line[3:6], 2)
    line = line[6:]

    result = 0
    if id == 4 :
        valstr=""
        while line :
            valstr+=line[1:5]
            c = line[0]
            line = line[5:]
            if c == '0' :
                break
        result = int(valstr, 2)
    else :
        values = []
        byte_size = 15 if line[0] == "0" else 11
        nb = int(line[1:byte_size+1], 2)
        line = line[byte_size+1:]

        if byte_size == 15:
            subpackets = line[:nb]
            line = line[nb:]
            while subpackets :
                (newvalue, subpackets) = GetPackageValues(subpackets)
                values.append(newvalue)
        else :
            for _ in range(nb):
                (newvalue, line) = GetPackageValues(line)
                values.append(newvalue)

        switchid = {
            0 : lambda val : sum(val),
            1 : lambda val : prod(val),
            2 : lambda val : min(val),
            3 : lambda val : max(val),
            5 : lambda val : val[0] > val[1],
            6 : lambda val : val[0] < val[1],
            7 : lambda val : val[0] == val[1],
        }

        result = int(switchid[id](values))
    return (result, line)



def puzzle(fileName) :
    line = GetHexa(GetData(fileName))
    (values, line) = GetPackageValues(line)
    print("the package version is", version)
    print("the package values is", values)


puzzle(inputFile)
