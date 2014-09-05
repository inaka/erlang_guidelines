-module(spaces).

-export ([bad/0, better/0, good/0]).

%% @doc inconsistent 
bad() ->
  try
    ThisBlock = is:indented(with, two, spaces),
    that:is_good(ThisBlock)
  catch
      _:_ ->
          {this, block, is, indented},
          [with, four, spaces]
  end.

%% @doc consistent, but with 4 spaces
better() ->
    receive
        {this, block} -> is:indented(with, four, spaces);
        _That -> is:not_good()
    after 100 ->
        but:at_least(it, is, consistent)
    end.

%% @doc good
good() ->
  case indentation:block() of
    {2, spaces} -> me:gusta();
    {_, _} -> not_sure:if_gusta()
  end.
