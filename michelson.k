require "michelson-syntax.k"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports DOMAINS

  rule <k> { code B:Block ; _:StorageDecl _:ParameterDecl } P:Data S:Data => B </k> // Untyped semantics disregard everything but the code block.
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  syntax StackSort ::= Stack(List)

  configuration <k> $PGM:Pgm </k>
                <stack> Stack(.List) </stack>
endmodule
