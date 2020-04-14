import json, sys
assert len(sys.argv) == 4 or len(sys.argv) == 5
with open(sys.argv[1], 'r') as f:
    extracted = json.load(f)
    key = sys.argv[2]
    skip_key_cmd = sys.argv[3].lower()
    skip_key = skip_key_cmd  == "true"
    assert skip_key or skip_key_cmd == "false"
    val = extracted.get(key, "")
    if len(sys.argv) == 5 and val.strip() == "":
        val = sys.argv[4]
    if skip_key:
        if val.startswith('"') and val.endswith('"'):
            val = val[1:-1]
        print(val)
    else:
        print("{0} {1} ;".format(sys.argv[2], val))
