-module(nested_try_catch).

-export([bad/0, good1/0, good2/0]).

bad() ->
  try
    maybe:throw(exception1),
    try
      maybe:throw(exception2),
      "We are safe!"
    catch
      _:exception2 ->
        "Oh, no! Exception #2"
    end
  catch
    _:exception1 -> "Bummer! Exception #1"
  end.

good1() ->
  try
    maybe:throw(exception1),
    maybe:throw(exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1";
    _:exception2 ->
      "Oh, no! Exception #2"
  end.

good2() ->
  try
    maybe:throw(exception1),
    a_function:that_deals(with, exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1"
  end.