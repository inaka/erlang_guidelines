-module(loud_errors).

-export([bad1/1, bad2/1, good1/1, good2/1]).

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

good1(WithThis) ->
  try
    something:that(may, fail, WithThis)
  catch
    _:Error ->
      lager:error("Error here: ~p~n"
                  " Arguments: ~p~n"
                  " Stack: ~p", [Error, WithThis, erlang:get_stacktrace()]),
      {error, Error}
  end.

good2(WithThis) ->
  try
    something:that(may, fail, WithThis)
  catch
    _:Error ->
      exit({error, Error})
  end.
