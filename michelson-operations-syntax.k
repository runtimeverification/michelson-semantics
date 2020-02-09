module MICHELSON-OPERATIONS-SYNTAX
  imports MICHELSON-SYNTAX

  syntax BlockchainOperation ::= "Create_contract" "(" Int "," Contract "," OptionData "," Mutez "," Data ")"
  syntax BlockchainOperation ::= "Transfer_tokens" "(" Int "," Data "," Mutez "," Address ")"
  syntax BlockchainOperation ::= "Set_delegate" "(" Int "," OptionData ")"

  syntax Data ::= BlockchainOperation
endmodule
