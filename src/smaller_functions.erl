-module(smaller_functions).

-export([bad/0, bad/1, good/0, good/1]).

%% @doc function with just a case
bad(Arg) ->
  case Arg of
    this_one -> should:be(a, function, clause);
    and_this_one -> should:be(another, function, clause)
  end.

%% @doc usage of pattern matching
good(this_one) -> is:a(function, clause);
good(and_this_one) -> is:another(function, clause).


%% @doc function with an internal case
bad() ->
  InitialArg = some:initial_arg(),
  InternalResult =
    case InitialArg of
      this_one -> should:be(a, function, clause);
      and_this_one -> should:be(another, function, clause)
    end,
  some:modification(InternalResult).

%% @doc usage of function clauses instead of an internal case
good() ->
  InitialArg = some:initial_arg(),
  InternalResult = good(InitialArg),
  some:modification(InternalResult).
