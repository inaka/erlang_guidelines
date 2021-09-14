-module(dyn_calls).

-export([bad/1, good/1]).

bad(Arg) ->
  Mods = [module_1, module_2, module_3],
  Fun = my_function,
  lists:foreach(
    fun(Mod) ->
      Mod:Fun(Arg)
    end, Mods).

good(Arg) ->
  module_1:my_function(Arg),
  module_2:my_function(Arg),
  module_3:my_function(Arg).
