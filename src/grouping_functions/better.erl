%%% @doc associated functions are closer
-module(better).

-export([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> public1(X);
    _ -> throw(cant_do)
  end.
public1(X) -> private3(X). % This is a private function but it's obviously related just to the one before

public2() -> private2(private1()).

private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
