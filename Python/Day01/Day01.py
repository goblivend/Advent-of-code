inputFile = "../../Data/Day01/input.txt"
exampleFile  = "../../Data/Day01/ExampleFile.txt"

def GetIncrease(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    prev = 100000
    nbincrease = 0
    for line in lines:
        curr = int(line)
        if prev < curr :
            nbincrease+=1
        prev = curr
    return nbincrease

print(GetIncrease(inputFile))

def GetGeneralIncreast(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    curr_1, curr_2, curr_3 = 10000, 10000, 10000

    nbincrease = 0
    for line in lines:
        curr = int(line)
        if curr_1 + curr_2 + curr_3 < curr + curr_1 + curr_2:
            nbincrease+=1
        curr_1, curr_2, curr_3 = curr, curr_1, curr_2
    return nbincrease

print(GetGeneralIncreast(inputFile))