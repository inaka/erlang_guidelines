-module(type_placement).

-export([good/0, bad/0]).

-type good_type() :: 1..3.

-spec good() -> good_type().
good() -> 2.


-type bad_type() :: 1..3.
-spec bad() -> bad_type().
bad() -> 2.
