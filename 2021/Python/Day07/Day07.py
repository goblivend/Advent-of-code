from typing import List

inputFile = "../../Data/Day07/input.txt"
exampleFile  = "../../Data/Day07/ExampleFile.txt"

def GetData(fileName : str) -> List[int]:
    with open(fileName, 'r') as file :
        data = file.readline().replace('\n', '').split(",")
    return [int(elt) for elt in data if elt]

somme = lambda value : value*(value+1)//2
identity = lambda value : value

def fuelCost(data : List[int], pos : int, cost) -> int:
    return sum([cost(abs(pos- elt)) for elt in data])

def GetMinimalfuelstr(fileName : str, cost) -> None :
    data = GetData(fileName)
    mini, maxi = min(data), max(data)
    minipos, minicost = 0, fuelCost(data, 0, cost)
    for i in range(mini, maxi) :
        fc = fuelCost(data, i, cost)
        if fc < minicost : minicost, minipos = fc, i
    print(minicost, minipos)

GetMinimalfuelstr(inputFile, identity)
GetMinimalfuelstr(inputFile, somme)
