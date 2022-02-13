%% @doc this is a very very trivial example, DRY has a much wider scope but it's
%%      provided just as an example
-module(dry).

-export([bad/0, good/0]).

bad() ->
  case something:from(other, place) of
    {show, _} ->
      display:nicely(something:from(other, place));
    nothing ->
      display:nothing()
  end.

good() ->
  case something:from(other, place) of
    {show, _} = ThingToShow ->
      display:nicely(ThingToShow);
    dont_show_me ->
      display:nothing()
  end.
