#!/usr/bin/env python3
import json, sys

def wrap_string(string):
    return '"{0}"'.format(string)

def emit(item):
    print(" {0} ".format(str(item)), end="")

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
    emit("(")
    emit(obj["nonce"])
    emit(",")
    if "parameters" in obj:
        emit_expression(obj["parameters"]["value"])
    else:
        emit("Unit")
    emit(",")
    emit(obj["amount"])
    emit(",")
    emit(wrap_string(obj["destination"]))
    emit(")")

def emit_optional_delegate(obj):
    if "delegate" in obj:
        emit("Some " + str(wrap_string(obj["delegate"])))
    else:
        emit("None")


def emit_origination(obj):
    emit("Create_contract")
    emit("(")
    emit(obj["nonce"])
    emit(",")
    emit_expression(obj["script"]["code"], True)
    emit(",")
    emit_optional_delegate(obj)
    emit(",")
    emit(obj["balance"])
    emit(",")
    emit_expression(obj["script"]["storage"])
    emit(")")

def emit_delegation(obj):
    emit("Set_delegate")
    emit("(")
    emit(obj["nonce"])
    emit(",")
    emit_optional_delegate(obj)
    emit(")")

def emit_operation(obj):
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

with open(sys.argv[1], "r") as f:
    emit_operation(json.load(f))
