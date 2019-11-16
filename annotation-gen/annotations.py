import itertools

types = ["type", "variable", "field"]

lists = {
    "type": ["T0", "T1"],
    "variable": ["V0", "V1", "V2"],
    "field": ["F0", "F1", "F2"]
}

files = {
    "type": [None, "1type"],
    "variable": [None, "1var", "2var"],
    "field": [None, "1field", "2field"]
}

class Opcode:
    def __init__(self, opcode):
        self.opcode = opcode
        self.type = 0
        self.variable = 0
        self.field = 0
        self.rest = ""

    def tostring(self):
        return "syntax Instruction ::= \"{0}\" T{1}V{2}F{3} {4}".format(self.opcode, self.type, self.variable, self.field, self.rest)

opcodes = {}

with open("opcodes") as f:
    for opcode in f.readlines():
        opcodes[opcode.strip()] = Opcode(opcode.strip())

for annotation_type in types:
    for i in range(1, len(files[annotation_type])):
        with open(files[annotation_type][i]) as f:
            for opcode in f.readlines():
                setattr(opcodes[opcode.strip()], annotation_type, i)

with open("remaining") as f:
    for line in f.readlines():
        split = line.split(",")
        opcodes[split[0].strip()].rest = split[1].strip()


def makeSyntaxSubstring(prefix, num):
    return "" if num == 0 else prefix + str(num)

def makeNonterminalsList(type_, variable, field):
    ret = list(filter(lambda x: len(x) > 0, [makeSyntaxSubstring("T", type_), makeSyntaxSubstring("V", variable), makeSyntaxSubstring("F", field)]))
    if len(ret) == 0:
        return ["NoAnnotation"]
    else:
        return ret

made_set = set()

for m_type_ in range(2):
    for m_variable in range(3):
        for m_field in range(3):
            s = "syntax T{0}V{1}F{2} ::= ".format(m_type_, m_variable, m_field)
            results = []
            for type_ in range(m_type_ + 1):
                for variable in range(m_variable + 1):
                    for field in range(m_field + 1):
                        for permutation in itertools.permutations(makeNonterminalsList(type_, variable, field)):
                            results.append(" ".join(permutation))
            if len(results) == 0:
                print("{0}NoAnnotation".format(s))
            else:
                print("{0}{1}".format(s, " | ".join(results)))

#for opcode in opcodes:
#    print(opcodes[opcode].tostring())
