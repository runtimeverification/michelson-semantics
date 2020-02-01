#!/usr/bin/python3
import os, sys, re

script_dir = os.path.dirname(__file__)
old_tests = os.path.join(script_dir, "old-unit")
new_tests = os.path.join(script_dir, "unit")

files = list(map(lambda s: os.path.join(old_tests, s), os.listdir(old_tests)))

def is_out_file(s):
    return s.endswith(".expected")

expected_files = sorted(filter(is_out_file, files))
input_files = sorted(filter(lambda s: not is_out_file(s), files))

assert len(input_files) == len(expected_files) 

def read_file(name):
    with open(name) as f:
        return f.read()

def extract_input_group(contents):
    input_elements = []
    list_marker = "$Stack"
    idx = contents.find(list_marker) + len(list_marker)
    while idx < len(contents):
        item_marker = "ListItem("
        idx = contents.find(item_marker, idx) 
        if idx < 0:
            break
        idx += len(item_marker)
        start = idx
        depth = 1
        while depth > 0 and idx < len(contents):
            if contents[idx] == "(":
                depth += 1
            elif contents[idx] == ")":
                depth -= 1
            idx += 1
        assert depth == 0
        end = idx - 1
        input_elements.append(contents[start:end])
    return "{{ {0} }}".format("; ".join(input_elements))



group_res = {
    "input": extract_input_group
}
def extract_groups(test_file_contents):
    return [name + group_res[name](test_file_contents) for name in group_res] 

def extract_output(expected_file_contents):
    return ""

for (test_name, expected_name) in zip(input_files, expected_files):
   groups = extract_groups(read_file(test_name))
   groups.append(extract_output(read_file(expected_name)))
   with open(os.path.join(new_tests, os.path.basename(test_name) + "t"), "w") as f:
       print(";\n".join(groups), file=f)

