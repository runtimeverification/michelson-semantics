module MICHELSON-INTERNAL-SYNTAX
  imports MICHELSON-SYNTAX

  syntax BlockchainOperation ::= "Create_contract" "(" Int "," Contract "," OptionData "," Mutez "," Data ")"
  syntax BlockchainOperation ::= "Transfer_tokens" "(" Int "," Data "," Mutez "," Address ")"
  syntax BlockchainOperation ::= "Set_delegate" "(" Int "," OptionData ")"

  syntax FailedStack ::= "(" "Failed" Data ")"
  syntax FailedStack ::= "(" "MutezOverflow" Int Int ")"
  syntax FailedStack ::= "(" "MutezUnderflow" Int Int ")"
  syntax FailedStack ::= "(" "GeneralOverflow" Int Int ")"

  syntax Data ::= BlockchainOperation
endmodule
