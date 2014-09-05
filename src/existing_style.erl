-module(existing_style).

-export([bad/0, good/0]).

bad() ->
  % existing code
  List = [ {elem1, 1}
         , {elem2, 2}
  % new code (not respecting the format)
         , {elem3, 3}, {elem4, 4},
           {elem5, 5}
         ],
  other_module:call(List).

good() ->
  % existing code
  List = [ {elem1, 1}
         , {elem2, 2}
  % new code (respecting the format)
         , {elem3, 3}
         , {elem4, 4}
         , {elem5, 5}
         ],
  other_module:call(List).
