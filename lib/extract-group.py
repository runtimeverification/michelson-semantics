import json, sys

def strToBool(s):
    s = s.lower()
    if s not in ['true','false']:
        raise ValueError("'{}' is not a valid Bool ('true','false')")
    return s == 'true'

if len(sys.argv) not in [4,5]:
    raise ValueError("usage: <jsonfile> <key> <skip_printing_key> [default_value]")

input_file  = sys.argv[1]
key         = sys.argv[2]
skip_key    = strToBool(sys.argv[3])
default_val = sys.argv[4] if len(sys.argv) == 5 else None

# extract JSON
extracted = None
with open(input_file, 'r') as f:
    extracted = json.load(f)

# get JSON value for key
val = extracted.get(key, "")

# set default value if value is empty and default exists
if default_val != None and val.strip() == "":
    val = default_val

# print out either "value" or "key value ;"
if skip_key:
    if val.startswith('"') and val.endswith('"'):
        val = val[1:-1]
    print(val)
else:
    print("{0} {1} ;".format(key, val))
