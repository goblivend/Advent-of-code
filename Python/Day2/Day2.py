inputFile = "../../Data/Day2/input.txt"
exampleFile  = "../../Data/Day2/ExampleFile.txt"

def GetIncrease(fileName) :
    file = open(fileName, 'r')
    lines = file.readlines()
    horizontal = 0
    depth = 0
    for line in lines:
        data = line.split(' ')
        if len(data) !=2 :
            print(line)
        else :
            data[1].strip()
            if data[0] == "down" :
                depth += int(data[1])
            elif data[0] == "up" :
                depth -= int(data[1])
            elif data[0] == "forward" :
                horizontal += int(data[1])
            else :
                print(line)
    print((horizontal, depth))
    return horizontal*depth

print(GetIncrease(exampleFile))

def GetIncreaseSecond(fileName) :
    file = open(fileName, 'r')
    lines = file.readlines()
    horizontal = 0
    depth = 0
    aim = 0
    for line in lines:
        data = line.split(' ')
        if len(data) !=2 :
            print(line)
        else :
            data[1].strip()
            if data[0] == "down" :
                aim += int(data[1])
            elif data[0] == "up" :
                aim -= int(data[1])
            elif data[0] == "forward" :
                horizontal += int(data[1])
                depth += aim*int(data[1])
            else :
                print(line)
    print((horizontal, depth))
    return horizontal*depth

print(GetIncreaseSecond(exampleFile))