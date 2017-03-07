-module(case_catch).


bad(List) ->
  case catch hd(List) of
    {'EXIT', {badarg, Reason}} ->
      {badarg, Reason};
    Hd ->
      Hd
  end.

good(List) ->
  try hd(List) of
    Hd ->
      Hd
  catch
    badarg:T ->
      {badarg,T}
  end.
