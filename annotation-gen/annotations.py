import itertools

types = ["type", "variable", "field"]

lists = {
    "type": ["L0TA", "L1TA"],
    "variable": ["L0VA", "L1VA", "L2VA"],
    "field": ["L0FA", "L1FA", "L2FA"]
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


#for type_ in lists["type"]:
    #for variable in lists["variable"]:
        #for field in lists["field"]:
            #for permutation in itertools.permutations([type_, variable, field]):
                #print("syntax {0}{1}{2} ::= {3}".format(type_, variable, field, " ".join(permutation)))

for opcode in opcodes:
    print(opcodes[opcode].tostring())
