module MICHELSON-ANNOTATIONS-SORTS
  syntax NoAnnotation ::= "" [klabel(noannotation)]
  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]

  syntax L0TA ::= NoAnnotation
  syntax L1TA ::= TypeAnnotation | NoAnnotation

  syntax L0VA ::= NoAnnotation
  syntax L1VA ::= VariableAnnotation | NoAnnotation
  syntax L2VA ::= VariableAnnotation VariableAnnotation | VariableAnnotation NoAnnotation | NoAnnotation

  syntax L0FA ::= NoAnnotation
  syntax L1FA ::= FieldAnnotation | NoAnnotation
  syntax L2FA ::= FieldAnnotation FieldAnnotation | FieldAnnotation | NoAnnotation

  syntax VariableAnnotationList ::= List{VariableAnnotation, " "}
  syntax FieldAnnotationList ::= List{FieldAnnotation, " "}

  

endmodule
