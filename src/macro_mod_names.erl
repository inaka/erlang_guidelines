-module(macro_mod_names).

-define(SERVER, ?MODULE). % Oh, god! Why??
-define(TM, another_module).

-export([bad/1, good/1]).

bad(Arg) ->
  Parsed = gen_server:call(?SERVER, {parse, Arg}),
  ?TM:handle(Parsed).

good(Arg) ->
  Parsed = gen_server:call(?MODULE, {parse, Arg}),
  another_module:handle(Parsed).
