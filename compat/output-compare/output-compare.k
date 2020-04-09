requires "michelson.k"
requires "common.k"

module OUTPUT-COMPARE-SYNTAX
  imports UNIT-TEST-SYNTAX
  syntax RealOutputGroup ::= "real_output" OutputStack
  syntax Group ::= RealOutputGroup
  syntax Pgm ::= Data
endmodule

module OUTPUT-COMPARE
  imports OUTPUT-COMPARE-SYNTAX
  imports COMPAT-COMMON
  imports K-REFLECTION
  imports MICHELSON

  syntax String ::= #decodeBinaryRepresentation(String) [function, hook(MICHELSON.decode)]

  syntax BlockchainOperation ::= #parseOperation(String) [function]
  rule #parseOperation(S) => #parseKORE(S)

  syntax KItem ::= #CheckOutput(OutputStack, OutputStack) // Expected, Actual

  rule #ConcreteArgToSemantics(B:MBytesLiteral, operation _) => #ConcreteArgToSemantics(#parseOperation(#decodeBinaryRepresentation(#MBytesToString(B))), operation .AnnotationList)

  rule I:Instruction ; => I [anywhere]

  rule <k> #CheckOutput( X:FailedStack , X:FailedStack ) => . </k>
       <returncode> _ => 0 </returncode>

  rule <k> #CheckOutput( { } , { } ) => . </k>
       <returncode> _ => 0 </returncode>

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } ) => . </k>
       <returncode> _ => 0 </returncode>
       requires #Matches(#ConcreteArgToSemantics(ED, ET), #ConcreteArgToSemantics(AD, AT))

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } )  </k>
       <stack> . => #Mismatch(#ConcreteArgToSemantics(ED, ET), #ConcreteArgToSemantics(AD, AT)) </stack> [owise]

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } ) => #CheckOutput( { Es } , { As } ) </k>
       requires #Matches(#ConcreteArgToSemantics(ED, ET), #ConcreteArgToSemantics(AD, AT))

  syntax KItem ::= #Mismatch(Data, Data)

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } )  </k>
       <stack> . => #Mismatch(#ConcreteArgToSemantics(ED, ET), #ConcreteArgToSemantics(AD, AT)) </stack> [owise]

  rule <k> other_contracts M ; Gs => Gs </k>
       <knownaddrs> _ => #OtherContractsMapToKMap(M) </knownaddrs>
       <returncode> _ => 1 </returncode>

  rule <k> real_output AOS ; output EOS ; => #CheckOutput(EOS, AOS) </k>
       <returncode> _ => 1 </returncode>

  rule <k> Gs:Groups </k>
       <returncode> 0 => 1 </returncode> [owise]
endmodule
