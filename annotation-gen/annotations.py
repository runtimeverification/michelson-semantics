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

def makeSyntaxStrings(type_, variable, field):



for type_ in range(2):
    for variable in range(3):
        for field in range(3):
            for permutation in itertools.permutations(["T" + str(type_), "V" + str(variable), "F" + str(field)]):
                print("syntax T{0}V{1}F{2} ::= {3}".format(str(type_), str(variable), str(field), " ".join(permutation)))

for opcode in opcodes:
    print(opcodes[opcode].tostring())
