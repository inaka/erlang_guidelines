-module(validations).

-export([bad/1, good/1]).

bad(X) ->
  gen_server:call(?MODULE, {add, X}).

good(X) when is_integer(X) ->
  gen_server:call(?MODULE, {add, X});
good(X) ->
  throw({invalid_input, X}).
