from typing import List

inputFile = "../../Data/Day21/input.txt"
exampleFile = "../../Data/Day21/ExampleFile.txt"

def GetCase(score) :
    return score % 10 if score%10 != 0 else 10

class Player1 :
    def __init__(self, case, maxi) :
        self.case = case
        self.score = 0
        self.max = maxi

    def __str__(self) :
        return f'Player on case {self.case} has a score of {self.score}'
    def __repr__(self):
        return self.__str__()

    def GetTurn(self, die) :
        score = self.case
        #print(f'on case {self.case} / ', end='')
        for _ in range(3) :
            temp = die.GetValue()
            #print(f'{temp}, ', end='')
            score += temp
        #print(f'to finish with a total of {score} ', end='')
        score = GetCase(score)
        #print(f'adding a score of {score} ', end='')
        self.score += score
        #print(f'with a total of {self.score}')
        self.case = score

        if self.score >= self.max :
            return True
        return False


class Die :
    def __init__(self) :
        self.value = 0
        self.nbroll = 0

    def GetValue() :
        self.nbroll += 1
        return value

    def __str__(self) :
        return f'Die currently at {self.value} after {self.nbroll} rolls'
    def __repr__(self):
        return self.__str__()

class Die1(Die):
    def __init__(self, maxi) :
        self.max = maxi
        self.value = 0
        self.nbroll = 0

    def GetValue(self) :
        self.value = (self.value+1) if self.value < self.max else 1
        self.nbroll +=1
        return self.value


def GetData(fileName) :
    with open(fileName, 'r') as file :
        lines = file.readlines()

    return [int(line.replace('\n', '')[-1]) for line in lines]


def puzzle1(fileName) :
    d1, d2 = GetData(fileName)
    p1, p2 = Player1(d1, 1000), Player1(d2, 1000)
    die = Die1(100)
    finished = False
    while True :
        #print('P1 rolls ', end='')
        if p1.GetTurn(die) :
            break
        #print('P2 rolls ', end='')
        if p2.GetTurn(die) :
            break
    print(p1, p2)
    print(die)
    print(die.nbroll*(min(p1.score, p2.score)))


puzzle1(exampleFile)