from typing import List

inputFile = "../../Data/Day18/input.txt"
exampleFile = "../../Data/Day18/ExampleFile.txt"

class BinTree:
    def __init__(self, key, left=None, right=None):
        self.key = key
        self.left = left
        self.right = right

    def IsVal(self) :
        return self.key != -1

    def Magnitude(self) :
        if not self.IsVal() :
            return 3*self.left.Magnitude() + 2*self.right.Magnitude()
        return self.key

    def Display(self, backstr = "") :
        if self :
            print(backstr + "- " + str(self.key))
            if not self.IsVal() :
                self.left.Display(backstr + " | ")
                self.right.Display(backstr + "   ")
    def ToStr(self) :
        if not self :
            return ""
        else :
            if not self.IsVal() :
                s = "[" +self.left.ToStr() + ','
                s += self.right.ToStr() + ']'
                return s
            else :
                return str(self.key)

    def Addleft(self, value) :
        if self.IsVal() :
            self.key += value
        else :
            self.left.Addleft(value)

    def Addright(self, value) :
        if self.IsVal() :
            self.key += value
        else :
            self.right.Addright(value)

    def Explode(self, depth = 0) :
        if self.IsVal() :
            return False, 0, 0
        if depth < 4 :
            exploded, l, r = self.left.Explode(depth+1)
            if exploded :
                self.right.Addleft(r)
                r = 0
            else :
                exploded, l, r = self.right.Explode(depth+1)
                if exploded :
                    self.left.Addright(l)
                    l = 0
            return exploded, l, r
        else :
            self.key = 0
            l, r= self.left.key, self.right.key
            self.left, self.right = None, None
            return True, l, r

    def Split(self) :
        if self.IsVal() :
            if self.key > 9 :
                k = self.key
                self.key = -1
                self.left = BinTree(k//2)
                self.right = BinTree(k-k//2)
                return True
            return False
        else :
            splitted = self.left.Split()
            if not splitted :
                splitted = self.right.Split()
            return splitted

    def Clean(self) :
        keep = True
        while keep :
            keep = self.Explode()[0]
            if not keep :
                keep = self.Split()



def Add(B1, B2) :
    B = BinTree(-1, B1, B2)
    B.Clean()
    return B

def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    return [line.replace('\n', '') for line in lines]

def __LoadTree(line) :
    (key, left, right) = (-1, None, None)
    if line == "[]" :
        return None
    else :
        imax = len(line)-1
        openpara = 1
        closedpara = 0
        imin = 1
        if line[imin] == '[' :
            i, left = __LoadTree(line[imin:])
            imin += i
        else :
            val = line[imin]
            imin += 1
            if line[imin].isdigit() :
                val += line[imin]
                imin += 1
            left = BinTree(int(val))

        imin += 1
        if line[imin] == '[' :
            i, right = __LoadTree(line[imin:])
            imin += i
        else :
            val = line[imin]
            imin += 1
            if line[imin].isdigit() :
                val += line[imin]
                imin += 1
            right = BinTree(int(val))

        return imin+1, BinTree(-1, left, right)

def LoadTree(line) :
    return __LoadTree(line)[1]


explodes = [
    ('[[[[[9,8],1],2],3],4]', '[[[[0,9],2],3],4]'),
    ('[7,[6,[5,[4,[3,2]]]]]', '[7,[6,[5,[7,0]]]]'),
    ('[[6,[5,[4,[3,2]]]],1]', '[[6,[5,[7,0]]],3]'),
    ('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]'),
    ('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]', '[[3,[2,[8,0]]],[9,[5,[7,0]]]]'),
    ('[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]', '[[[[0,7],4],[7,[[8,4],9]]],[1,1]]'),
    ('[[[[0,7],4],[7,[[8,4],9]]],[1,1]]', '[[[[0,7],4],[15,[0,13]]],[1,1]]'),
]
for i, expl in enumerate(explodes) :
    B = LoadTree(expl[0])
    B.Explode()
    if B.ToStr() != expl[1] :
        print('Explosions at', i)
        print(expl[0])
        print(B.ToStr())
        print(expl[1])


splits = [
    ('[[[[0,7],4],[15,[0,13]]],[1,1]]', '[[[[0,7],4],[[7,8],[0,13]]],[1,1]]'),
    ('[[[[0,7],4],[[7,8],[0,13]]],[1,1]]', '[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]'),
]
for spl in splits :
    B = LoadTree(spl[0])
    B.Split()
    if B.ToStr() != spl[1] :
        print('Splitting at', i)
        print(spl[0])
        print(B.ToStr())
        print(spl[1])

magns = [
    ('[[1,2],[[3,4],5]]', 143),
    ('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]', 1384),
    ('[[[[1,1],[2,2]],[3,3]],[4,4]]', 445),
    ('[[[[3,0],[5,3]],[4,4]],[5,5]]', 791),
    ('[[[[5,0],[7,4]],[5,5]],[6,6]]', 1137),
    ('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]', 3488)
]
for magn in magns :
    B = LoadTree(magn[0])
    if B.Magnitude() != magn[1] :
        print('Magnitudes at', i)
        print(magn[0])
        print(B.Magnitude())
        print(magn[1])


def puzzle1(fileName) :
    lines = GetData(fileName)
    curr = LoadTree(lines.pop(0))
    while len(lines) != 0 :
        curr = Add(curr, LoadTree(lines.pop(0)))
    print(curr.Magnitude())
puzzle1(inputFile)

def GetPermutations(lines) :
    permutations = []
    for i in range(len(lines)) :
        for j in range(len(lines)) :
            if i != j :
                permutations.append((lines[i], lines[j]))
                permutations.append((lines[j], lines[i]))
    return permutations

def puzzle2(fileName) :
    lines = GetData(fileName)
    maxi = 0

    for l1, l2 in GetPermutations(lines) :
        B1 = LoadTree(l1)
        B2 = LoadTree(l2)
        B = Add(B1, B2)
        magn = B.Magnitude()
        if magn > maxi :
            maxi = magn


    print(maxi)

puzzle2(inputFile)
"""
[
    [
        [
            [
                0,
                7
            ],
            4
        ],
        [
            15,
            [
                0,
                13
            ]
        ]
    ],
    [
        1,
        1
    ]
]
"""