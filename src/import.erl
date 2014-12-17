-module(import).

-export([good/1, bad/1]).

-import(lists, [map/2]).

bad(L) -> map(fun(X) -> X * 2 end, L).

good(L) -> lists:map(fun(X) -> X * 2 end, L).
