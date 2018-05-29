class Stack:
    def __init__(self):
        self._data = []

    def __len__(self):
        return len(self._data)

    def isempty(self):
        return len(self._data) == 0

    def top(self):
        return self._data[-1]

    def push(self, e):
        self._data.append(e)

    def pop(self):
        return self._data.pop()
    
    def str(self):
        res = ""
        for el in self._data:
            res = res + str(el) + ' '
        return res

    def clear(self):
        self._data = []

rules = {
    1 : ("X", "S"),
    2 : ("S", "OS"),
    3 : ("S", "O"),
    4 : ("O", "a[S]"),
    5 : ("O", "a[S][S]"),
    6 : ("O", "a=E"),
    7 : ("E", "E+T"),
    8 : ("E", "T"),
    9 : ("T", "T*P"),
    10 : ("T", "P"),
    11 : ("P", "(E)"),
    12 : ("P", "a")
}


table = {
    "a" :
        ["P2", "P9", "C12", "", "P2", "C10", "", "C8", "", "C12", #0
         "P2", "P9", "P9", "", "P9", "C11", "", "C16", "C7", "C9", #10
         "C4", "P2", "", "C5"],                     #20
    "[" :
        ["", "", "P10", "", "", "", "", "", "", "", #0
         "", "", "", "", "", "", "", "", "", "", #10
         "P21", "", "", ""],                    #20
    "]" :
        ["", "", "C12", "", "C3", "C10", "", "C8", "", "C12", #0
         "", "", "", "C2", "", "C11", "P20", "C6", "C7", "C9", #10
         "C4", "", "P23", "C5"],                    #20
    "+" :
        ["", "", "C12", "P12", "", "C10", "", "С8", "P12", "C12", #0
         "", "", "", "", "", "C11", "", "P12", "C7", "C9"], #10
    "*" :
        ["", "", "C12", "", "", "C10", "", "P14", "", "C12", #0
         "", "", "", "", "", "C11", "", "", "P14", "C9"], #10
    "=" :
        ["", "", "P11"],                    #0
    "(" :
        ["P1", "P1", "", "", "", "", "", "", "", "", #0
         "", "P1", "P1", "", "P1", "", "", "", "", "", #10
         "", "", "", "", ""],                    #20
    ")" :
        ["", "", "C12", "", "", "C10", "", "С8", "P15", "C12", #0
         "", "", "", "", "", "C11", "", "", "C7", "C9"],     #20
    "0" :
        ["", "", "C12", "", "C3", "C10", "D", "С8", "", "C12", #0
         "", "", "", "C2", "", "C11", "", "C6", "C7", "C9", #10
         "C4", "", "", "C5"],                    
    "S" :
        ["V6", "", "", "", "V13", "", "", "", "", "", #0
         "V16", "", "", "", "", "", "", "", "", "", #10
         "", "V22"],                    
    "O" :
        ["V4", "", "", "", "V4", "", "", "", "", "", #0
         "V4", "", "", "", "", "", "", "", "", "", #10
         "", "v4"],                   
    "T" :
        ["V7", "V7", "", "", "", "", "", "", "", "", #0
         "", "V7", "V18", "", "", "", "", "", "", "", #10
         "", "", "", "", ""],                   
    "P" :
        ["V5", "V5", "", "", "", "", "", "", "", "", #0
         "", "V5", "V5", "", "V19"],                 
    "E" :
        ["V3", "V8", "", "", "", "", "", "", "", "", #0
         "", "V17"]
}

magNum = Stack()

def check(chain):
    magNum.clear()
    magNum.push(0)
    chain += "0"
    
    imax = len(chain)
    currentCond = 0
    nonTetm = ""
    i = 0
    imax = len(chain)
    print("%-14s %-40s %-25s" %("Правило", "Необработанная часть входной цепочки", "Содержимое магазина"))
    try:
        while (i < imax):
            sym = chain[i]
            action = table[sym][currentCond]
            type = action[0]
            if action == "D":
                print("Допустить")
                break
            number = int(action[1:])
            if type == "P":
                magNum.push(number)
                currentCond = number
                i += 1
            elif type == "C" or type == "С":
                rightPart = rules[number][1]
                nLet = len(rightPart) 
                nonTerm = rules[number][0]
                print("%2s.%-1s-> %-7s %-40s %-25s" %(number, nonTerm, rightPart, chain[i+1:], magNum.str()))
                for k in range(nLet): 
                    magNum.pop()
                currentCond = magNum.top()
                V = table[nonTerm][currentCond]
                if len(V) == 0:
                    break
                else:
                    currentCond = int(V[1:])
                    magNum.push(currentCond)
            #print("%-3s %-10s %-40s %-25s" %(" ", " ", chain[i+1:], magNum.str()))
    except Exception:
        print("Отвергнуть")
                        

check("a[a=a+a*a][a=(a+a)]a[a=a]")