-module(pattern_matching).

-export([bad/1, good/1]).

bad(L) ->
  case length(L) of
    0 -> error;
    _ -> ok
  end.

good([]) ->
  error;
good(_L) ->
  ok.