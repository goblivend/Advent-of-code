inputFile = "../../Data/Day 1/input.txt"

def GetIncrease(fileName) :
    file = open(fileName, 'r')
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
    file = open(fileName, 'r')
    lines = file.readlines()
    curr_1 = 10000
    curr_2 = 10000
    curr_3 = 10000


    nbincrease = 0
    for line in lines:
        curr = int(line)
        if curr_1 + curr_2 + curr_3 < curr + curr_1 + curr_2:
            nbincrease+=1
        curr_3 = curr_2
        curr_2 = curr_1
        curr_1 = curr
    return nbincrease


print(GetGeneralIncreast(inputFile))