def getlist(lines) :
    res = []
    acc=0
    while len(lines) != 0 :
        if len(lines[0]) == 0 :
            res += [acc]
            acc=0
            lines.pop(0)
            continue
        acc+= int(lines[0])
        lines.pop(0)
    return res

with open('input.txt', 'r') as f :
    lines = [l.strip() for l in f.readlines()]

l = getlist(lines)
print(max(l))

l.sort()
l.reverse()
print(sum(l[:3]))
