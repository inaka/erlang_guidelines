-module(specs).

-export([bad/2, good/2]).

bad(InitialValue, Commands) ->
  gen_server:call(?MODULE, {compute, InitialValue, Commands}).

-type command() :: inc | dec.
-spec good(pos_integer(), [command()]) -> pos_integer().
good(InitialValue, Commands) ->
  gen_server:call(?MODULE, {compute, InitialValue, Commands}).
