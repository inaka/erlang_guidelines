-module(variable_names).

-export([bad/2, good/2]).

bad(Variablename, Another_Variable_Name) ->
  [Variablename, Another_Variable_Name].

good(Variable, VariableName) ->
  [Variable, VariableName].
