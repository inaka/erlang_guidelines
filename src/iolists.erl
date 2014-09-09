-module(iolists).

-export([good/1, bad/1]).

bad(Param) -> "Hello " ++ binary_to_list(Param) ++ "! Have a nice day!".

good(Param) -> ["Hello ", Param, "! Have a nice day!"].
