inputFile = "../../Data/Day02/input.txt"
exampleFile  = "../../Data/Day02/ExampleFile.txt"

def GetIncrease(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    horizontal, depth, aimdepth = 0, 0, 0
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
                aimdepth += depth*int(data[1])
            else :
                print(line)
    print("For puzzle 1 :", (horizontal, depth), horizontal*depth, '\nAnd for Puzzle 2 :', (horizontal, aimdepth), horizontal*aimdepth)
    return horizontal*depth

GetIncrease(exampleFile)
