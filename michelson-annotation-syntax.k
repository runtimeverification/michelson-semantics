module MICHELSON-ANNOTATION-SYNTAX
  syntax NoAnnotation ::= "" [klabel(noannotation)]
  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]

  syntax TypeAnnotationList ::= List{TypeAnnotation, " "}
  syntax VariableAnnotationList ::= List{VariableAnnotation, " "}
  syntax FieldAnnotationList ::= List{FieldAnnotation, " "}
endmodule
