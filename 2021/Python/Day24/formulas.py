
def findZ(prev, number) :
    nb = [ int(c) for c in list(str(number))]
    diff = False


    for i in range(14) :
        if prev[i+1][0] != nb[i] :
            diff = True
        if diff :
            prev[i+1] = (nb[i], GetRes(i, prev[i][1], nb[i]))


def GetRes(i, prev, n) :
    (w, x, y, z) = prev
    functs = [
        GetRes0,
        GetRes1,
        GetRes2,
        GetRes3,
        GetRes4,
        GetRes5,
        GetRes6,
        GetRes7,
        GetRes8,
        GetRes9,
        GetRes10,
        GetRes11,
        GetRes12,
        GetRes13,
    ]
    #print('Updating digit ' + str(i))
    return functs[i](w, x, y, z, n)


def GetRes0(w, x, y, z, n) :
    w = n
    y = y + w
    y = y + 1
    z = z + y
    x = x + z
    x = x % 26
    x = x + 12
    return (w, x, y, z)

def GetRes1(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 1
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    x = x + 15
    return (w, x, y, z)

def GetRes2(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 16
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -8
    return (w, x, y, z)

def GetRes3(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 5
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -4
    return (w, x, y, z)

def GetRes4(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 9
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    x = x + 15
    return (w, x, y, z)

def GetRes5(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 3
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    x = x + 14
    return (w, x, y, z)

def GetRes6(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 2
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    x = x + 14
    return (w, x, y, z)

def GetRes7(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 15
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -13
    return (w, x, y, z)

def GetRes8(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 5
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -3
    return (w, x, y, z)

def GetRes9(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 11
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -7
    return (w, x, y, z)

def GetRes10(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 7
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    x = x + 10
    return (w, x, y, z)

def GetRes11(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 1
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -6
    return (w, x, y, z)

def GetRes12(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 10
    y = y * x
    z = z + y
    x = x * 0
    x = x + z
    x = x % 26
    z = z // 26
    x = x + -8
    return (w, x, y, z)

def GetRes13(w, x, y, z, n) :
    w = n
    x = x == w
    x = x == 0
    y = y * 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = y * 0
    y = y + w
    y = y + 3
    y = y * x
    z = z + y
    return (w, x, y, z)
