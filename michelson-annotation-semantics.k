requires "michelson-syntax.k"
requires "michelson-config.k"

module MICHELSON-ANNOTATION-SEMANTICS
  imports MICHELSON-SYNTAX
  imports MICHELSON-ANNOTATION-SYNTAX
  imports MICHELSON-CONFIG

  syntax KItem ::= ExtractAnnotations(Instruction)

  syntax RawAnnotations ::= T0V0F0 
                          | T0V0F1
                          | T0V0F2
                          | T0V1F0
                          | T0V1F1
                          | T0V1F2
                          | T0V2F0
                          | T0V2F1
                          | T0V2F2
                          | T1V0F0 
                          | T1V0F1
                          | T1V0F2
                          | T1V1F0
                          | T1V1F1
                          | T1V1F2
                          | T1V2F0
                          | T1V2F1
                          | T1V2F2

  syntax TransformedAnnotations ::= #ExtractedAnnotations(TypeAnnotationList, VariableAnnotationList, FieldAnnotationList)
  syntax KResult ::= TransformedAnnotations
  syntax AnnotationForms ::= RawAnnotations | TransformedAnnotations

  syntax KItem ::= HandleAnnotations(AnnotationForms, Instruction)

  rule <k> (_:NoAnnotation):T0V0F0 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (_:NoAnnotation):T0V0F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T0V0F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T0V0F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T0V0F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation):T0V0F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T0V1F0 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation):T0V1F0 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (_:NoAnnotation):T0V1F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T0V1F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T0V1F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T0V1F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T0V1F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation):T0V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T0V2F0 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation):T0V2F0 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation):T0V2F0 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (_:NoAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T0V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T0V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T1V0F0 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V0F0 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (_:NoAnnotation):T1V0F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T1V0F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V0F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation):T1V0F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation):T1V0F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T1V0F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T1V0F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation):T1V0F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V0F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation):T1V0F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation):T1V0F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V0F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation):T1V0F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T1V1F0 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation):T1V1F0 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V1F0 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation):T1V1F0 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation):T1V1F0 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (_:NoAnnotation):T1V1F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T1V1F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T1V1F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T1V1F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T1V1F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation F0:FieldAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation V0:VariableAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation T0:TypeAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation T0:TypeAnnotation):T1V1F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation F0:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation T0:TypeAnnotation):T1V1F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T1V2F0 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation):T1V2F0 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation):T1V2F0 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V2F0 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation):T1V2F0 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation):T1V2F0 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F0 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation):T1V2F0 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (_:NoAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation V0:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation):T1V2F1 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (_:NoAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(.TypeAnnotationList, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, .VariableAnnotationList, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, .FieldAnnotationList) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation F0:FieldAnnotation F1:FieldAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (V0:VariableAnnotation V1:VariableAnnotation F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation T0:TypeAnnotation V0:VariableAnnotation V1:VariableAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
  rule <k> (F0:FieldAnnotation F1:FieldAnnotation V0:VariableAnnotation V1:VariableAnnotation T0:TypeAnnotation):T1V2F2 => #ExtractedAnnotations(T0:TypeAnnotation, V0:VariableAnnotation V1:VariableAnnotation, F0:FieldAnnotation F1:FieldAnnotation) ...  </k>
endmodule
