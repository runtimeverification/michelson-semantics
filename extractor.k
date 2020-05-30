requires "json.k"
requires "common.k"

module EXTRACTOR-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module EXTRACTOR
  imports COMPAT-COMMON
  imports EXTRACTOR-SYNTAX
  imports JSON

  syntax JSONKey ::= #KeyFromGroup(Group) [function]

  rule #KeyFromGroup(G:ContractGroup) => "contract"
  rule #KeyFromGroup(G:NowGroup) => "now"
  rule #KeyFromGroup(G:SenderGroup) => "sender"
  rule #KeyFromGroup(G:SourceGroup) => "source"
  rule #KeyFromGroup(G:ChainGroup) => "chain_id"
  rule #KeyFromGroup(G:SelfGroup) => "self"
  rule #KeyFromGroup(G:AmountGroup) => "amount"
  rule #KeyFromGroup(G:BalanceGroup) => "balance"
  rule #KeyFromGroup(G:BigMapGroup) => "big_maps"
  rule #KeyFromGroup(G:ContractsGroup) => "other_contracts"
  rule #KeyFromGroup(G:CodeGroup) => "code"
  rule #KeyFromGroup(G:InputGroup) => "input"
  rule #KeyFromGroup(G:OutputGroup) => "output"
  rule #KeyFromGroup(G:ParameterDecl) => "parameter"
  rule #KeyFromGroup(G:ParameterValueGroup) => "parameter_value"
  rule #KeyFromGroup(G:StorageValueGroup) => "storage_value"

  syntax K ::= #GroupContent(Group) [function]
  rule #GroupContent(contract { C }) => C
  rule #GroupContent(now C) => C
  rule #GroupContent(sender C) => C
  rule #GroupContent(source C) => C
  rule #GroupContent(chain_id C) => C
  rule #GroupContent(self C) => C
  rule #GroupContent(amount C) => C
  rule #GroupContent(balance C) => C
  rule #GroupContent(other_contracts C) => C
  rule #GroupContent(code C) => C
  rule #GroupContent(input C) => C
  rule #GroupContent(output C) => C
  rule #GroupContent(parameter C) => C
  rule #GroupContent(big_maps C) => C
  rule #GroupContent(parameter_value C) => C
  rule #GroupContent(storage_value C) => C

  syntax JSON ::= #GroupsToJson(Groups) [function]
  syntax JSONs ::= #GroupsToJsonAux(Groups) [function]
  syntax JSON ::= #GroupToJson(Group) [function]

  rule #GroupsToJson(Gs) => { #GroupsToJsonAux(Gs) }

  rule #GroupsToJsonAux(G ; Gs) => #GroupToJson(G) , #GroupsToJsonAux(Gs)
  rule #GroupsToJsonAux(G) => #GroupToJson(G)
  rule #GroupsToJsonAux(G ;) => #GroupToJson(G)


  rule #GroupToJson(G) => #KeyFromGroup(G) : #unparse(#GroupContent(G))

  rule <k> Gs:Groups => . </k>
       <out> ... .List => ListItem(JSON2String(#GroupsToJson(Gs))) </out>
       <returncode> 1 => 0 </returncode>
endmodule
