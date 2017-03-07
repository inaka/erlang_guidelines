-module(nesting).

-export([bad/0, good/0]).

bad() ->
  case this:function() of
    has ->
      try too:much() of
        nested ->
          receive
            structures ->
              it:should_be(refactored);
            into ->
              several:other(functions)
          end
      catch
        _:_ ->
          dont:you("think?")
      end;
    _ ->
      i:do()
  end.

good() ->
  case this:function() of
    calls ->
      other:functions();
    that ->
      internal_work()
  end.

internal_work() ->
  try do:the(internal, parts) of
    what ->
      was:done(in)
  catch
    _:the ->
      previous:example()
  end.
