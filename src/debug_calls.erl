-module(debug_calls).

-export([bad/1, good/1]).

-spec bad(any()) -> any().
bad(Input) ->
  io:format("About to do something with ~p~n", [Input]),
  R = god:create_user(Input),
  ct:pal("The result was ~p", [R]),
  R.

-spec good(any()) -> any().
good(Input) ->
  god:create_user(Input).
