import sys
from functools import cache

def algo(stone) :
    if stone == 0 :
        return [1]
    elif len(str(stone)) % 2 == 0 :
        half_len = len(str(stone)) // 2
        return [int(str(stone)[:half_len]), int(str(stone)[half_len:])]
    else :
        return [2024*stone]

@cache
def solvei(stone, n) :
    res1 = algo(stone)
    if n == 1 :
        return len(res1)
    return sum(map (lambda x : solvei(x, n-1), res1))

def solve(arr, n) :
    return sum(map(lambda x : solvei(x, n), arr))

def part1(arr) :
    return solve(arr, 25)

def part2(arr) :
    return solve(arr, 75)

if __name__ == "__main__" :
    file = sys.argv[1]
    with open(file) as f :
        arr = list(map(int, f.read().split(' ')))
    print(part1(arr))
    print(part2(arr))
