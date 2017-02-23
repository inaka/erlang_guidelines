-module(loud_errors).

-export([bad1/1, bad2/1, good/1]).

bad1(WithThis) ->
  try
    something:that(may, fail, WithThis)
  catch
    _:Error ->
      {error, Error}
  end.

bad2(WithThis) ->
  try
    something:that(may, fail, WithThis)
  catch
    _:Error ->
      throw({error, Error})
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
