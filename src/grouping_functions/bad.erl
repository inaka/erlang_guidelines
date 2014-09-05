%%% @doc Mixing priv and public functions
-module(bad).

-export([public1/0, public2/0]).

public1() -> private3(atom1).

private1() -> atom2.

public2() -> private2(private1()).

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
