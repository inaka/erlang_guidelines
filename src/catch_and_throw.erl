-module(catch_and_throw).

-export([bad/2, good/2]).

%% We use catch and throw to find the first element that matches a predicate on a list
bad(Pred, List) ->
    catch lists:foreach(
        fun(Elem) ->
            case Pred(Elem) of
                true -> throw(Elem);
                _ -> noop
            end
        end, List).

%% We use recursion to find the first element that matches a predicate on a list
good(_Pred, []) -> false;
good(Pred, [Elem|Elems]) ->
    case Pred(Elem) of
        true -> Elem;
        _ -> good(Pred, Elems)
    end.
