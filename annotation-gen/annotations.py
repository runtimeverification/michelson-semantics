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

def combinedSortString(type_, variable, field):
    return "T{0}V{1}F{2}".format(type_, variable, field)

class Opcode:
    def __init__(self, opcode):
        self.opcode = opcode
        self.type = 0
        self.variable = 0
        self.field = 0
        self.rest = ""
        self.operand_types = []
        self.operand_names = []

    def mysortstring(self):
        return combinedSortString(self.type, self.variable, self.field)

    def setoperandstring(self, rest):
        self.rest = rest.strip()
        self.operand_types = self.rest.split()
        self.operand_names = [self.operand_types[i][:1] + str(i) for i in range(len(self.operand_types))]

    def tostring(self):
        return "syntax Instruction ::= \"{0}\" {1} {2}".format(self.opcode, self.mysortstring(), self.rest)

    def torule(self): 
        return "rule <k> #ExtractAnnotations({0} Annotations:{1} {2}) => #HandleAnnotations(Annotations, {0} {2}) ... </k>".format(self.opcode, self.mysortstring(), " ".join(self.operand_names))

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
        opcodes[split[0].strip()].setoperandstring(split[1].strip())


def makeSyntaxSubstring(prefix, num):
    return "" if num == 0 else prefix + str(num)

def makeNonterminalsList(type_, variable, field):
    ret = list(filter(lambda x: len(x) > 0, [makeSyntaxSubstring("T", type_), makeSyntaxSubstring("V", variable), makeSyntaxSubstring("F", field)]))
    if len(ret) == 0:
        return ["NoAnnotation"]
    else:
        return ret

def removeEmpty(lst):
    return filter(lambda x: len(x) > 0, lst)

#for m_type_ in range(2):
#    for m_variable in range(3):
#        for m_field in range(3):
#            s = "syntax {0} ::= ".format(combinedSortString(m_type_, m_variable, m_field))
#            results = []
#            for type_ in range(m_type_ + 1):
#                for variable in range(m_variable + 1):
#                    for field in range(m_field + 1):
#                        for permutation in itertools.permutations(makeNonterminalsList(type_, variable, field)):
#                            results.append(" ".join(permutation))
#            if len(results) == 0:
#                print("{0}NoAnnotation".format(s))
#            else:
#                print("{0}{1}".format(s, " | ".join(results)))

for opcode in opcodes:
    #print(opcodes[opcode].tostring())
    print(opcodes[opcode].torule())

#def makeRawAnnotationFormString(n, prefix, sort):
#    if n == 0:
#        return "" #"_:NoAnnotation" 
#    else:
#        return " ".join(map(lambda x: "{0}{1}:{2}".format(prefix, x, sort), range(n)))

#def makeFixedAnnotationFormString(n, prefix, sort):
#    if n == 0:
#        return ".{0}List".format(sort)
#    else:
#        return makeRawAnnotationFormString(n, prefix, sort) # LHS = RHS in this case.

#for m_type_ in range(2):
#    for m_variable in range(3):
#        for m_field in range(3):
#            lhs_set = set()
#            combinedSort = combinedSortString(m_type_, m_variable, m_field)
#            for type_ in range(m_type_ + 1):
#                for variable in range(m_variable + 1):
#                    for field in range(m_field + 1):
#                        sorts = [(type_, "T", "TypeAnnotation"), 
#                                 (variable, "V", "VariableAnnotation"),
#                                 (field, "F", "FieldAnnotation")] 
#                        rhs = ", ".join([makeFixedAnnotationFormString(n, p, s) for (n, p, s) in sorts])
#                        for permutation in itertools.permutations(sorts):
#                            lhs = " ".join(removeEmpty([makeRawAnnotationFormString(n, p, s) for (n, p, s) in permutation]))
#                            if type_ == 0 and variable == 0 and field == 0:
#                                lhs = "_:NoAnnotation"
#                            if lhs not in lhs_set:
#                                lhs_set.add(lhs)
#                                print("rule <k> ({0}):{1} => #ExtractedAnnotations({2}) ...  </k>".format(lhs, combinedSort, rhs))
