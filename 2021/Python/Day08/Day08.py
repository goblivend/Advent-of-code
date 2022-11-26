from typing import List

inputFile = "../../Data/Day08/input.txt"
exampleFile  = "../../Data/Day08/ExampleFile.txt"
exampleFile2  = "../../Data/Day08/ExampleFile2.txt"

def GetData(fileName : str) -> List[int]:
    with open(fileName, 'r') as file :
        data = [([ input for input in line.split(' | ')[0].replace('\n', '').split(' ')],[ output for output in line.split(' | ')[1].replace('\n', '').split(' ')]) for line in file.readlines()]
    return data

def GetEasynb(fileName) :
    res = 0
    for line in GetData(fileName) :
        for output in line[1] :
            if len(output) <= 4 or len(output) == 7 :
                res += 1
    return res

print("There is", GetEasynb(exampleFile2), "easy numbers in this file")

def filterSegment(segments : List[str], i : int, new : str) -> None :
    segment = ""
    for letter in "abcdefg" :
        if letter in new and letter in segments[i] :
            segment += letter
    segments[i] = segment

def filterSolo(segments : List[str]) -> None :
    for segment in segments :
        if len(segment) == 1 :
            for i in range(len(segments)) :
                if segment in segments[i] and segment != segments[i] :
                    listsegment = list(segments[i])
                    listsegment.remove(segment)
                    segments[i] = "".join(listsegment)

def updateSegments(segments : List[str], number : str) -> None :
    nbsegments = len(number)
    if nbsegments == 7 : return
    if nbsegments == 2 :
        filterSegment(segments, 2, number)
        filterSegment(segments, 5, number)
    if nbsegments == 3 :
        filterSegment(segments, 0, number)
        filterSegment(segments, 2, number)
        filterSegment(segments, 5, number)
    if nbsegments == 4 :
        filterSegment(segments, 1, number)
        filterSegment(segments, 2, number)
        filterSegment(segments, 3, number)
        filterSegment(segments, 5, number)
    if nbsegments == 5 :
        filterSegment(segments, 0, number)
        filterSegment(segments, 3, number)
        filterSegment(segments, 6, number)
    if nbsegments == 6 :
        filterSegment(segments, 0, number)
        filterSegment(segments, 1, number)
        filterSegment(segments, 5, number)
        filterSegment(segments, 6, number)

    filterSolo(segments)

def similarNumber(nb1 : str, nb2 : str) -> bool:
    if len(nb1) != len(nb2) : return False
    for i in range(len(nb1)) :
        if nb1[i] not in nb2 or nb2[i] not in nb1 : return False
    return True

def GetNumberFromSegments(segments : List[str], number : str) -> str :
    if len(number) == 2 : return "1"
    if len(number) == 4 : return "4"
    if len(number) == 3 : return "7"
    if len(number) == 7 : return "8"

    newnb = ""
    for letter in number :
        newnb += chr(segments.index(letter) + ord('a'))
    if similarNumber(newnb, "acdeg") : return "2"
    if similarNumber(newnb, "acdfg") : return "3"
    if similarNumber(newnb, "abdfg") : return "5"
    if similarNumber(newnb, "abdefg") : return "6"
    if similarNumber(newnb, "abcdfg") : return "9"
    if similarNumber(newnb, "abcefg") : return "0"

def GetResult(line) :
    segments = ["abcdefg"]*7
    while len("".join(segments)) != 7 :
        for aninput in line[0] :
            updateSegments(segments, aninput)
    res = ""
    for output in line[1] :
        res += GetNumberFromSegments(segments, output)
    return int(res)

def GetSumResult(fileName) :
    return sum([GetResult(line) for line in GetData(fileName)])

print("The sum of the output numbers is", GetSumResult(inputFile))
