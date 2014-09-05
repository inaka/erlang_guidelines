-module(spaces).

-export([bad/3, good/3]).

% @doc no spaces
bad(_My,_Space,_Bar)->[is,'not',working].

% @doc spaces!!
good(_Hey, _Now, _It) -> ["works " ++ "again, " | [hooray]].
