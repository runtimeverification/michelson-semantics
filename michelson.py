#!/usr/bin/env python3
extdir = 'ext'
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), extdir))
from kninja.runner import *

proj = KProject(extdir = extdir)
KDefinition( proj
           , alias = 'michelson'
           , backend = 'llvm'
           , directory = proj.builddir('defn/michelson')
           )
KRunner(proj).main()
