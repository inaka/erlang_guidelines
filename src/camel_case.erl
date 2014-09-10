-module(camel_case).

-export([bad/0, good/0]).

bad() ->
  Variable_Name = moduleName:functionName(atomConstant),
  another_ModuleName:another_Function_Name(Variable_Name).

good() ->
  VariableName = module_name:function_name(atom_constant),
  another_module_name:another_function_name(VariableName).
