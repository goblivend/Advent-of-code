from typing import List

inputFile = "../../Data/Day24/input.txt"
exampleFile = "../../Data/Day24/ExampleFile.txt"
exampleFile2 = "../../Data/Day24/ExampleFile2.txt"
exampleFile3 = "../../Data/Day24/ExampleFile3.txt"

def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [line.strip() for line in lines]

add = lambda a, b : a+b
mul = lambda a, b : a*b
div = lambda a, b : a//b
mod = lambda a, b : a%b
eql = lambda a, b : a==b

def is_digit(n):
    try:
        int(n)
        return True
    except ValueError:
        return  False


class Alu :
    def __init__(self, instructions) :
        self.instructions = Alu.ParseInstructions(instructions)

    def ParseInstructions(instructions) :
        newinsts = []
        for instruction in instructions :
            instarr = instruction.split(' ')
            if instarr[0] == 'inp' :
                newinsts.append(('inp', instarr[1], 0))
            else :
                if instarr[0] == 'add' :
                    ope = '+'
                    if instarr[2] == '0' :
                        continue
                elif instarr[0] == 'mul' :
                    ope = '*'
                    if instarr[2] == '1' :
                        continue
                elif instarr[0] == 'div' :
                    ope = '//'
                    if instarr[2] == '1' :
                        continue
                elif instarr[0] == 'mod' :
                    ope = '%'
                elif instarr[0] == 'eql' :
                    ope = '=='
                else :
                    raise ValueError('Bas input was given, received ' + instarr[0])
                newinsts.append((ope, instarr[1], instarr[2] if not is_digit(instarr[2]) else int(instarr[2])))
        return newinsts
    def GetFormula(self, path) :
        with open(path, 'w') as file :
            file.write('def calculate(number) :\n')
            file.write('    w=0\n')
            file.write('    x=0\n')
            file.write('    y=0\n')
            file.write('    z=0\n')
            file.write('    nb = [ int(c) for c in list(str(number))]\n')
            file.write('    i = 0\n')
            for (ope, a, b) in self.instructions :
                file.write('    ')
                if ope == 'inp' :
                    file.write(f'{a} = nb[i]\n')
                    file.write('    i += 1\n')
                else :
                    file.write(f'{a} = {a} {ope} {b}\n')
            file.write('    return (w, x, y, z)\n')

    def Execute(self, number):
        nb = [ int(c) for c in list(str(number))]
        values = {
            'w':0,
            'x':0,
            'y':0,
            'z':0,
        }
        idx = 0
        for (ope, a, b) in self.instructions :
            if ope == 'inp' :
                values[a] = nb[idx]
                idx += 1
            else :
                values[a] = ope(values[a], values[b] if not is_digit(b) else b)
        return values


import time

data = GetData(inputFile)
#print(data)
alu = Alu(data)
#print(alu.instructions)
alu.GetFormula('./formula.py')
maxi = 99996991111111
mini = 11111127991111
#print(alu.Execute(maxi))
start = time.time()

from formulas import findZ
prev = [ (0, (0, 0, 0, 0))] * 15
#for i in range(maxi, mini, -1) :
"""i = maxi
while i > mini :
    str_min = list(str(i))
    if '0' not in str_min :
        if i%500000 == 111111 :
            print(i)
            print(i, time.time()-start)
            start = time.time()
        findZ(prev, i)
        if prev[-1][-1][-1] == 0 :
            print(i)
            print(prev[-1])
            break
        i -= 1
    else :
        idx = str_min.index('0')
        str_min[idx-1] = str(int(str_min[str_min.index('0')-1])-1)
        str_min[idx] = '9'
        i = int("".join(str_min))
"""

for i in range(maxi, mini, -1) :
    str_min = str(i)
    if '0' not in str_min :
        if i%500000 == 111111 :
            #print(i)
            print(i, time.time()-start)
            start = time.time()
        findZ(prev, i)
        if prev[-1][-1][-1] == 0 :
            print(i)
            print(prev[-1])
            break



#99998453611111
"""from formula import calculate

for i in range(maxi, mini, -1) :
    if '0' not in str(i) :
        if i%100000 == 11111 :
            print(i, time.time()-start)
            start = time.time()
        (w, x, y, z) = calculate(i)
        if z == 0 :
            print(i)
            print(dico)
            break"""



"""
for i in range(maxi, mini, -1) :
    #for i in range(mini, maxi) :
    if '0' not in str(i) :
        if i%10000 == 1111 :
            print(i, time.time()-start)
            start = time.time()
        dico = alu.Execute(i)
        if dico['z'] == 0 :
            print(i)
            print(dico)
            break"""
