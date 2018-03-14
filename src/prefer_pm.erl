-module(prefer_pm).

-export ([good/3, bad/3]).

%% @doc Uses equality comparisons (=:=) for everything
-spec bad(T, T, 0|1|2) -> ok.
bad(A, B, 0) ->
  case A =:= B of
    true -> proceed();
    false -> fail(A)
  end;
bad(A, B, 1) ->
  case change(A) =:= B of
    true -> proceed();
    false -> fail(A)
  end;
bad(A, B, 2) ->
  case change(A) =:= change(B) of
    true -> proceed();
    false -> fail(A)
  end.

%% @doc Uses pattern-matching everywhere
-spec good(T, T, 0|1|2) -> ok.
good(A, B, 0) ->
  case A of
    B -> proceed();
    A -> fail(A)
  end;
good(A, B, 1) ->
  case change(A) of
    B -> proceed();
    C -> fail(C)
  end;
good(A, B, 2) ->
  case {change(A), change(B)} of
    {C, C} -> proceed();
    {D, _} -> fail(D)
  end.

change(X) -> {changed, X}.
proceed() -> ok.
fail(E) -> exit({error, E}).
