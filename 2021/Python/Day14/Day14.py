from typing import List

import time

inputFile = "../../Data/Day14/input.txt"
exampleFile = "../../Data/Day14/ExampleFile.txt"
exampleFile2 = "../../Data/Day14/ExampleFile2.txt"

#################################################################################################
################### Trying to get the full polymer instead of just the result ###################
#################################################################################################

def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    polymer = [c for c in lines[0].replace('\n', '')]
    splitter = ' -> '
    toinsert = {line.replace('\n', '').split(splitter)[0] : line.replace('\n', '').split(splitter)[1] for line in lines if splitter in line}
    patterns = list(toinsert.keys())
    return polymer,  (patterns, toinsert)


def insert(polymer, insertions) :
    patt, toinsert = insertions
    newpol = []
    for i in range(len(polymer) -1) :
        newpol.append(polymer[i])
        crrPatt = polymer[i] + polymer[i+1]
        if crrPatt in patt :
            newpol.append(toinsert[crrPatt])

    return newpol

def histo(polymer) :
    histo = {c:polymer.count(c) for c in polymer}
    """ for c in polymer :
        if not c in header :
            header += c
            histo.append(polymer.count(c))
    histo.sort()"""
    return histo

def puzzle(fileName, nbinsertions) :
    polymer,  insertions = GetData(fileName)
    #print(polymer,  insertions)
    mytime = time.time()
    for i in range(nbinsertions) :
        polymer = insert(polymer, insertions)
        #print(i+1, time.time()-mytime)
        mytime = time.time()
    hist = histo(polymer)
    print(hist)
    print(max(hist.values()) - min(hist.values()))



#start_time = time.time()
#puzzle(exampleFile, 10)
#print('puzzle1 in', time.time() - start_time)

#################################################################################################
##################### Beeing smart and only counting the number of each pair ####################
#################################################################################################

def mypuzzleV2(fileName, nbinsertions) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    splitter = ' -> '
    poly = lines[0].strip()

    dico_pairs = {key : (key[0]+val, val+key[1]) for key, val in [line.strip().split(splitter) for line in lines[2:]]}

    nb_pairs = {pair:0 for pairs in [{key, val[0], val[1]} for key, val in dico_pairs.items()] for pair in pairs}

    histo = {pair[0]:poly.count(pair[0]) for pair in nb_pairs.keys()}

    # init nb_pairs with real values :
    for i in range(len(poly) -1) :
        nb_pairs[poly[i:i+2]] += 1

    for _ in range(nbinsertions) :
        pairs = nb_pairs.copy()
        for key, val in pairs.items() :
            nb_pairs[key] -= val
            nb_pairs[dico_pairs[key][0]] += val
            nb_pairs[dico_pairs[key][1]] += val
            histo[dico_pairs[key][1][0]] += val
    print(max(histo.values()) - min(histo.values()))

start_time = time.time()
mypuzzleV2(inputFile, 40)

print('puzzle2 in', time.time() - start_time)

