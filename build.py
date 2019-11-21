#!/usr/bin/env python3

import sys
from os import (path, environ)

repo_dir = path.dirname(__file__)
sys.path.append(path.join(repo_dir, 'ext/'))
from kninja import *

# Project Definition
# ==================

proj = KProject()

files = [
        # "test-michelson.k", covered by main=
        "michelson.k", 
        "michelson-syntax.k", 
        "test-michelson-syntax.k", 
        "michelson-annotation-syntax.k", 
        "michelson-annotation-semantics.k", 
        "michelson-config.k"
]

michelson = proj.definition(alias="michelson", backend="llvm",main="test-michelson.k",runner_script="./michelson.py",other=files)
michelson.tests(inputs = glob("tests/unit/*.tz"))

proj.main()

