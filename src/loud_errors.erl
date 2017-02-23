-module(loud_errors).

-export([bad/1, good/1]).

bad(WithThis) ->
  try
    something:that(may, fail, WithThis)
  catch
    _:Error ->
      {error, Error}
  end.

good(WithThis) ->
  try
    something:that(may, fail, WithThis)
  catch
    _:Error ->
      lager:error("Error here: ~p~n"
                  " Arguments: ~p~n"
                  " Stack: ~p", [Error, WithThis, erlang:get_stacktrace()]),
      {error, Error}
  end.
