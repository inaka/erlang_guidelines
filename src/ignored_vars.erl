-module(ignored_vars).

-export([good/1, bad/1]).

bad(_Number) -> 2 * _Number.

good(Number) -> 2 * Number.
