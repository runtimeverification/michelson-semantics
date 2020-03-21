values = [-9999999, -1, 0, 1, 9999999]
compares = [ "EQ", "NEQ", "LE", "LT", "GE", "GT"]

results = [[False, False, False, True, True],
           [False, False, True, True, True],
           [True, True, False, False, False],
           [True, True, True, False, False],
           [True, True, False, True, True],
           [False, False, True, False, False]]

results.reverse()

TEMPLATE = """code {{ {0} }} ;
input {{ Stack_elt int {1} }} ;
output {{ Stack_elt bool {2} }} ;
"""

for i in range(len(compares)):
    for j in range(len(values)):
        op = compares[i]
        val = values[j]
        res = results[i][j]
        with open("{0}_0{1}.tzt".format(op.lower(), str(j)), 'w') as f:
            f.write(TEMPLATE.format(op, val, res))

