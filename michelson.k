require "michelson-syntax.k"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports DOMAINS  

  rule I:Instruction ; Is:InstructionList => I ~> Is [structural]
  rule .InstructionList => .K [structural]
  rule {} => .K [structrual]
  rule { Is:InstructionList } => Is [structural]

  rule <k> { code B:Block ; _:StorageDecl _:ParameterDecl } P:Data S:Data => B </k> // Untyped semantics disregard everything but the code block.
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { code B:Block ; _:ParameterDecl _:StorageDecl } P:Data S:Data => B </k> // Untyped semantics disregard everything but the code block.
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:StorageDecl code B:Block ; _:ParameterDecl } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:ParameterDecl code B:Block ; _:StorageDecl } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:StorageDecl _:ParameterDecl code B:Block ; } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:ParameterDecl _:StorageDecl code B:Block ; } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  syntax StackSort ::= Stack(List)

  configuration <k> $PGM:Pgm </k>
                <stack> Stack(.List) </stack>
endmodule
