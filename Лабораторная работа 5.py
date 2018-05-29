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

def inTopAndPop(values):
    return mag.top() in values and mag.pop();

def inTop(values):
    return mag.top() in values;
 
def inTopAndPopArray(values):
    i = 0
    while i < len(values) and inTopAndPop(values[i]):
        i += 1
    return i == len(values)

def identification():
    if inTopAndPop("S"):
        if inTop("▼"):
            return True
    elif inTopAndPop("O"):
        if inTopAndPop("S"):
            return 1
        elif inTop("{[▼"):
            return 2
    elif inTopAndPop("}"):
        if inTopAndPop("Y"):
            if inTopAndPopArray(["]", "S", "[", "{"]):
                return 3
        elif inTopAndPop("]"):
            if inTopAndPopArray(["S", "[", "Y", "{"]):
                return 4
    elif inTopAndPopArray(["E", "=", "a"]):
        return 5
    elif inTopAndPop("a"):
        if inTopAndPopArray(["=", "a"]):
            return 6
        elif inTopAndPopArray(["<", "a"]):
            return 7
    elif inTopAndPop(")"):
        if inTopAndPop("Y"):
            if inTopAndPopArray(["(", "!"]):
                return 8
        elif inTopAndPop("E"):
            if inTopAndPopArray([",", "E", "("]):
                if inTopAndPop("+"):
                    return 9
                if inTopAndPop("*"):
                    return 10
    elif(inTopAndPop("b")):
        return 11
    return False

manipulateTable = {
    "П": {
        "{": "S[▼",
        "[": "Y{",
        "]": "S",
        "}": "Y]",
        "=": "a",
        "<": "a",
        "!": "{](",
        "(": "!+*",
        ")": "YE",
        "+": "=(,",
        ",": "E",
        "*": "=(,",
        "a": "S{[]=<(,▼",
        "b": "=(,"
    },
    "ОП": {
        "{": "O})b",
        "[": ")a",
        "]": "OE}b)",
        "}": ")a",
        ")": ")ab",
        ",": ")b",
        "a": "OE})b",
        "0": "SOE})b"
    }
}    

rules = {
    1 : "S -> SO",
    2 : "S -> O",
    3 : "O -> {[S]Y}",
    4 : "O -> {Y[S]}",
    5 : "O -> a=E",
    6 : "Y -> a=a",
    7 : "Y -> a<a",
    8 : "Y -> !(Y)",
    9 : "E -> +(E,E)",
    10 : "E -> *(E,E)",
    11 : "E -> b"
}

mag = Stack()

def check(chain):
    chain = chain + "0"
    mag.clear()
    mag.push("▼")
    i = 0
    imax = len(chain)
    print(" %-19s %-35s %3s %20s" %("Содержимое магазина", "Необработанная часть", "№пр", "Применяемое правило"))
    print(" %-19s %-35s" %(mag.str(), chain[i:imax]))
    while i < imax:
        sym = chain[i]
        if sym in manipulateTable["П"] and mag.top() in manipulateTable["П"][sym]:
            mag.push(sym)
            i += 1
        elif sym in manipulateTable["ОП"] and mag.top() in manipulateTable["ОП"][sym]:
            old_mag = mag.str()
            res = identification()
            if type(res) == type(True):
                print(" %-19s" %("▼S"))
                print(" Допустить ")
                break
            elif res == False:
                print(" Отвергнуть ")
                break
            else:    
                print(" %-19s %-35s %3s %20s" %(old_mag, chain[i:imax], res, rules[res]))
                mag.push(rules[res][0])
        else:
            print("Отвергнуть")
            break
                
check("{[a=+(b,b)]a=a}{!(a<a)[a=*(b,b)]}")