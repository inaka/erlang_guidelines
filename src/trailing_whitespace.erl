-module(trailing_whitespace).

-export([bad/0, good/0]).

bad() -> "this line has trailing whitespace".       

good() -> "this line has not".
