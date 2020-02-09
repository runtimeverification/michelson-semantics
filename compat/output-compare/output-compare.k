requires "../../unit-test-syntax.k"
requires "../../michelson-config.k"
requires "../../michelson.k"

module OUTPUT-COMPARE-SYNTAX
  imports UNIT-TEST-SYNTAX

  syntax RealOutputStack ::= "real_output" OutputStack
  syntax Group ::= RealOutputStack
endmodule

module OUTPUT-COMPARE
  imports OUTPUT-COMPARE-SYNTAX
  imports MICHELSON

  syntax OutputStack ::= #FindOutputStack(Groups) [function]
  rule #FindOutputStack(output T) => T
  rule #FindOutputStack(output T ; _) => T
  rule #FindOutputStack(_ ; G) => #FindOutputStack(G) [owise]

  syntax OutputStack ::= #FindRealOutputStack(Groups) [function]
  rule #FindRealOutputStack(real_output T) => T
  rule #FindRealOutputStack(real_output T ; _) => T
  rule #FindRealOutputStack(_ ; G) => #FindRealOutputStack(G) [owise]

  syntax KItem ::= #CheckOutput(OutputStack, OutputStack) // Expected, Actual

  rule <k> #CheckOutput( { } , { } ) => . </k>
       <returncode> _ => 0 </returncode>

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } ) => . </k>
       <returncode> _ => 0 </returncode>
       requires #ConcreteArgToSemantics(ED, ET) ==K #ConcreteArgToSemantics(AD, AT)

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } ) => #CheckOutput( { Es } , { As } ) </k>
       requires #ConcreteArgToSemantics(ED, ET) ==K #ConcreteArgToSemantics(AD, AT)

  rule <k> Gs => #CheckOutput(#FindOutputStack(Gs), #FindRealOutputStack(Gs)) </k>
       <returncode> _ => 1 </returncode>
endmodule
