-module(col_width).

-record(rec, {field1 :: any(), field2 :: any(), field3 :: any()}).

-export([bad/2, good/2]).

%$ @doc too wide
bad([#rec{field1 = FF1, field2 = FF2, field3 = FF3}, #rec{field1 = BF1, field2 = BF2, field3 = BF3} | Rest], Arg2) ->
  other_module:bad(FF1, FF2, FF3, BF1, BF2, BF3, bad(Rest, Arg2)).

%% @doc good (< 80 chars)
good([Foo, Bar | Rest], Arg2) ->
  #rec{field1 = FF1, field2 = FF2, field3 = FF3} = Foo,
  #rec{field1 = BF1, field2 = BF2, field3 = BF3} = Bar,
  other_module:good(FF1, FF2, FF3, BF1, BF2, BF3, good(Rest, Arg2)).
