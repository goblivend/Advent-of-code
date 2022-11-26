from typing import List

inputFile = "../../Data/Day19/input.txt"
exampleFile = "../../Data/Day19/ExampleFile.txt"
exampleFile2 = "../../Data/Day19/ExampleFile2.txt"


def print_arr(arr) :
    for elt in arr :
        print(elt)

def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()
    scanners = [[Pos(x=int(line.split(',')[0]), y=int(line.split(',')[1]), z=int(line.split(',')[2]))  for line in scanner.split('\n') if line and '---' not in line]for scanner in "".join(lines).split('\n\n')]

    return scanners

class Pos :
    def __init__(self, x=0, y=0, z=0, pos=None) :
        if not pos :
            self.x = x
            self.y = y
            self.z = z
        elif type(pos) != Pos :
            self.x = pos[0]
            self.y = pos[0]
            self.z = pos[0]
        else :
            self.x = pos.x
            self.y = pos.y
            self.z = pos.z

    def Add(self, pos2) :
        #print('pos2 :', type(pos2), 'Self :' , type(self))
        #print(pos2.x, pos2.y, pos2.z)
        self.x += pos2.x
        self.y += pos2.y
        self.z += pos2.z

    def __str__(self) :
        return f'({self.x}, {self.y}, {self.z})'

    def __eq__(self, other) :
        return self.x == other.x and self.y == other.y and self.z == other.z

    def factPos(self, fact) :
        return Pos(x=self.x*fact.x, y=self.y*fact.y, z=self.z*fact.z)

    def __ne__(self, other):
        return (not self.__eq__(other))

    def __repr__(self):
        return self.__str__()

    def __hash__(self):
        return hash(self.__repr__())


class Scanner:
    def __init__(self, beacons, pos=None) :
        self.beacons = set(beacons)
        self.pos = pos if type(pos) != type(None) else Pos(0, 0, 0)
        self.scanners = {self.pos}


    def GetFact(self, scan) :
        facts = {
            (1, 1, 1):0,}

            (-1, 1, 1):0,
            (1, -1, 1):0,
            (1, 1, -1):0,
            (-1, -1, 1):0,
            (-1, 1, -1):0,
            (1, -1, -1):0,
            (-1, -1, -1):0
        }
        keys = list(facts.keys())
        for beacon in scan.beacons :
            for key in keys :
                #print(facts[key])
                facts[key] += self.IsCommon(scan, beacon, Pos(pos=key))


        return Pos(pos=list(facts.keys())[list(facts.values()).index(max(facts.values()))]), max(facts.values())

    def IsCommon(self, scan, beac, fact) :
        """
        Looks if the beacon beac relative to scan is also present in self with the factor fact
        """
        #print(type(scan), type(scan.pos), type(beac), type(fact))
        return Pos(x=scan.pos.x + fact.x*beac.x, y=scan.pos.y + fact.y*beac.y, z=scan.pos.z + fact.z*beac.z) in self.beacons

    def Update(self, fact) :
        for beac in self.beacons :
            beac.x *= fact.x
            beac.y *= fact.y
            beac.z *= fact.z
            beac.Add(self.pos)

    def UpdateBeacons(self, scanner) :
        for beac in scanner.beacons :
            self.beacons.append(beac)

    def Add(self, scanner) :
        """
        Try to find the new position of the scanner to complete this Scanners object
        """
        facts = [
            (1, 1, 1)]

            (-1, 1, 1),
            (1, -1, 1),
            (1, 1, -1),
            (-1, -1, 1),
            (-1, 1, -1),
            (1, -1, -1),
            (-1, -1, -1)
        ]
        for fact in facts :
            fact = Pos(pos = fact)
            #print('Fact', type(fact), fact)
            for beac in scanner.beacons :
                beacpos = beac.factPos(fact)
                for beacon in self.beacons :
                    #print(beacpos)
                    #print(beacon)
                    scanner.pos = Pos(pos=beacon)
                    scanner.pos.Add(beacpos)
                    print(scanner.pos)
                    fact, nb = self.GetFact(scanner)

                    if nb > 1 :
                        scanner.Update(fact)
                        self.UpdateBeacons(scanner)
                        self.scanners.append(scanner)
                        print('Scanner Added now,', len(self.beacons))
                        return




def puzzle1(fileName) :
    data = GetData(fileName)
    scanners = Scanner(data.pop(0))
    while len(data) != 0 :
        scanners.Add(Scanner(data.pop(0)))
    print(len(scanners.beacons))
    print(scanners.beacons)




#puzzle1(exampleFile2)



# 79
# 3621


# 403
# 10569