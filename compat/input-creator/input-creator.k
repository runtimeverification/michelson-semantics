requires "../../unit-test-syntax.k"
requires "../../michelson-config.k"

module INPUT-CREATOR
  imports UNIT-TEST-SYNTAX
  imports MICHELSON-CONFIG

  syntax Data ::= #DataForType(Type) [function]
  rule #DataForType(int _) => 0
  rule #DataForType(nat _) => 0
  rule #DataForType(string _) => ""
  rule #DataForType(bytes _) => 0x
  rule #DataForType(mutez _) => 0
  rule #DataForType(bool _) => true
  rule #DataForType(key_hash _) => "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"
  rule #DataForType(timestamp _) => 0
  rule #DataForType(address _) => "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"

//  rule #DataForType(key _) 
  rule #DataForType(unit _) => Unit
//  rule #DataForType(signature _) 
//  rule #DataForType(operation _) 
//  rule #DataForType(chain_id _) 

  rule #DataForType(pair _ T1:Type T2:Type) => Pair #DataForType(T1) #DataForType(T2)

  rule #DataForType(option _ _) => None
  rule #DataForType(list _ _) => { }
  rule #DataForType(set _ _) => { }
//  rule #DataForType(contract _ _) => None
  rule #DataForType(or _ T1 _) => #DataForType(T1)
  rule #DataForType(lambda _ _ T2) => { DROP .AnnotationList ; PUSH .AnnotationList T2 #DataForType(T2) }
  rule #DataForType(map _ _ _) => { }
  rule #DataForType(big_map _ _ _) => { }

  syntax Type ::= #FindParamType(Groups) [function]
  rule #FindParamType(parameter T) => T
  rule #FindParamType(parameter T ; Rs) => T
  rule #FindParamType(_:Group) => unit .AnnotationList [owise]
  rule #FindParamType(_ ; Rs) => #FindParamType(Rs) [owise]

  rule <k> G:Groups => #DataForType(#FindParamType(G)) </k>
       <returncode> _ => 0 </returncode>

  rule <k> Gs:Groups ; => Gs </k> [owise]

endmodule
