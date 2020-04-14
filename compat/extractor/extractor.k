requires "unit-test-syntax.k"
requires "michelson-config.k"
requires "common.k"

module EXTRACTOR-SYNTAX
  imports UNIT-TEST-SYNTAX
  imports BOOL-SYNTAX

  syntax GroupId ::= "contract" 
                   | "now" 
                   | "sender" 
                   | "source"
                   | "chain_id"
                   | "self"
                   | "amount"
                   | "balance"
                   | "other_contracts"
                   | "parameter_value"
                   | "storage_value"
                   | "code"
                   | "input"
                   | "output"
                   | "query"
                   | "code_or_contract"

  syntax QueryGroup ::= "query" GroupId Bool
  syntax Group ::= QueryGroup
endmodule

module EXTRACTOR
  imports COMPAT-COMMON
  imports EXTRACTOR-SYNTAX
  imports LIST

  syntax Group ::= "#NoGroup"

  syntax Groups ::= #ReverseGroups(Groups) [function]
  syntax Groups ::= #ReverseGroupsAux(Groups, Groups) [function]
  rule #ReverseGroups(G:Group) => G
  rule #ReverseGroups(G:Group;) => G
  rule #ReverseGroups(G ; Gs) => #ReverseGroupsAux(Gs, G)
  rule #ReverseGroupsAux(G:Group, Gs) => G ; Gs
  rule #ReverseGroupsAux(G:Group;, Gs) => G ; Gs
  rule #ReverseGroupsAux(G ; Gs, Gs2) => #ReverseGroupsAux(Gs, G ; Gs2)

  syntax Group ::= #FindGroup(Groups, GroupId) [function]
  syntax Group ::= #FindGroupAux(Groups, GroupId) [function]

  rule #FindGroup(Gs, I) => #FindGroupAux(#ReverseGroups(#NoGroup ; Gs), I)
  rule #FindGroupAux(#NoGroup, _) => #NoGroup
  rule #FindGroupAux(G:ContractGroup ; _, contract) => G
  rule #FindGroupAux(G:NowGroup ; _, now) => G
  rule #FindGroupAux(G:SenderGroup ; _, sender) => G
  rule #FindGroupAux(G:SourceGroup ; _, source) => G
  rule #FindGroupAux(G:ChainGroup ; _, chain_id) => G
  rule #FindGroupAux(G:SelfGroup ; _, self) => G
  rule #FindGroupAux(G:AmountGroup ; _, amount) => G
  rule #FindGroupAux(G:BalanceGroup ; _, balance) => G
  rule #FindGroupAux(G:ContractsGroup ; _, other_contracts) => G
  rule #FindGroupAux(G:CodeGroup ; _, code) => G
  rule #FindGroupAux(G:InputGroup ; _, input) => G
  rule #FindGroupAux(G:OutputGroup ; _, output) => G
  rule #FindGroupAux(G:QueryGroup ; _, query) => G
  rule #FindGroupAux(G:CodeGroup ; _, code_or_contract) => G
  rule #FindGroupAux(G:ContractGroup ; _, code_or_contract) => G
  rule #FindGroupAux(_ ; Gs, Q) => #FindGroupAux(Gs, Q) [owise]

  rule #doUnparse(#NoGroup, _) => "#NoGroup"

  syntax K ::= #Ungroup(Group) [function]
  rule #Ungroup(#NoGroup) => #NoGroup
  rule #Ungroup(contract { C }) => C
  rule #Ungroup(now I) => I
  rule #Ungroup(sender S) => S
  rule #Ungroup(source S) => S
  rule #Ungroup(chain_id C) => C
  rule #Ungroup(self S) => S
  rule #Ungroup(amount I) => I
  rule #Ungroup(balance I) => I
  rule #Ungroup(other_contracts M) => M
  rule #Ungroup(parameter_value D) => D
  rule #Ungroup(storage_value D) => D
  rule #Ungroup(input LS) => LS
  rule #Ungroup(code C) => C
  rule #Ungroup(output O) => O
  rule #Ungroup(query Q B) => query Q B

  syntax K ::= #DoQuery(Groups, Group) [function]
  rule #DoQuery(Gs, query Q false) => #FindGroup(Gs, Q) ;
  rule #DoQuery(Gs, query Q true) => #Ungroup(#FindGroup(Gs, Q))
  rule #DoQuery(_, #NoGroup) => #NoGroup

  syntax K ::= #Extract(Groups) [function]
  rule #Extract(Gs) => #DoQuery(Gs, #FindGroup(Gs, query)) 

  rule <k> Gs:Groups => . </k>
       <out> ... .List => ListItem(#unparse(#Extract(#ReverseGroups(#NoGroup ; Gs)))) </out>
       <returncode> 1 => 0 </returncode>
endmodule
