from sys import argv

def add_parens(typestr):
    if len(typestr.strip().split()) > 1:
        return "(" + typestr + ")"
    return typestr

with open(argv[1], 'r') as types_file, open(argv[2], 'r') as data_file:
    types = types_file.readlines()[1:]
    data = data_file.readlines()[1:]
    assert len(types) == len(data)
    print("real_output {{ {0} }} ;".format(" ; ".join(["Stack_elt {0} {1}".format(add_parens(type_[:-1]), data[:-1].strip()) for (type_, data) in zip(types, data)])))
