#!/usr/bin/env python3
"""
Usage: decode-michelson-operation.py <hex-encoded binary string>

Consumes binary string given as the only command line argument and prints its
KORE representation on standard output.

Prerequisites:
1. the Tezos module in this archive should be built via the included Makefile.
2. the `kast` executable should be available on the system path.
"""

import json, sys, subprocess, os, tempfile

BUFFER = ""

def wrap_string(string):
    return '"{0}"'.format(string)

def emit(item):
    global BUFFER
    BUFFER += " {0} ".format(str(item))

list_type = type([])
dict_type = type({})

def emit_expression(obj, suppress_parens=False):
    if type(obj) == list_type:
        if not suppress_parens:
            emit("{")
        for i in range(len(obj)):
            emit_expression(obj[i], True)
            if (i != len(obj) - 1):
                emit(";")
        if not suppress_parens:
            emit("}")
    elif "int" in obj:
        emit(obj["int"])
    elif "contents" in obj:
        emit(wrap_string(obj["contents"]))
    elif "string" in obj:
        emit(wrap_string(obj["string"]))
    elif "bytes" in obj:
        emit("0x" + str(obj["bytes"]))
    elif "prim" in obj:
        if not suppress_parens:
            emit("(")
        emit(obj["prim"])
        if "annots" in obj:
            for ann in obj["annots"]:
                emit(ann)
        if "args" in obj:
            for arg in obj["args"]:
                emit_expression(arg)
        if not suppress_parens:
            emit(")")
    else:
        print(obj, file=sys.stderr)
        assert False

def emit_reveal(obj):
    assert False

def emit_transaction(obj):
    emit("Transfer_tokens")
    if "parameters" in obj:
        emit_expression(obj["parameters"]["value"])
    else:
        emit("Unit")
    emit(obj["amount"])
    emit(wrap_string(obj["destination"]))
    emit(obj["nonce"])

def emit_optional_delegate(obj):
    if "delegate" in obj:
        emit("Some " + str(wrap_string(obj["delegate"])))
    else:
        emit("None")


def emit_origination(obj):
    emit("Create_contract")
    emit("{")
    emit_expression(obj["script"]["code"], True)
    emit("}")
    emit_optional_delegate(obj)
    emit(obj["balance"])
    emit_expression(obj["script"]["storage"])
    emit(obj["nonce"])

def emit_delegation(obj):
    emit("Set_delegate")
    emit_optional_delegate(obj)
    emit(obj["nonce"])

def emit_operation(obj):
    global BUFFER
    BUFFER = ""
    assert type(obj) == dict_type
    assert "kind" in obj
    kind = obj["kind"]
    if kind == "reveal":
        emit_reveal(obj)
    elif kind == "transaction":
        emit_transaction(obj)
    elif kind == "origination":
        emit_origination(obj)
    elif kind == "delegation":
        emit_delegation(obj)
    else:
        assert False
    return BUFFER

# auxiliary operation for calling subprocesses
def run(args):
    proc_ret = subprocess.run(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    if proc_ret.returncode != 0:
        print(proc_ret.stderr)
        exit(1)
    return proc_ret.stdout

if __name__ == "__main__":
  if len(sys.argv) != 2: exit(1)
  # get paths to important pieces
  path = os.path.dirname(os.path.realpath(__file__))
  tezos_codec = os.path.join(path, "..", "ext",    "tezos", "tezos-codec")
  k_def_path  = os.path.join(path, "..", ".build", "defn",  "llvm")
  # run tezos codec to get json encoded string
  json_str = run([tezos_codec, "decode", "005-PsBabyM1.operation.internal", "from", sys.argv[1]])
  # run emit operation to get K encoding of operation
  k_str = emit_operation(json.loads(json_str))
  # run kast to get kore
  with tempfile.NamedTemporaryFile() as f:
      f.write(k_str.encode('utf-8'))
      f.flush()
      kore_str = run(["kast", "-s", "Data", "--directory", k_def_path, "-o", "kore", f.name])
      # print the kore str
      print(kore_str, end="")
