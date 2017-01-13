-module(recursion).

-export([recurse/1, fold/1, comprehension/1]).

%%
%% Example:
%% Different functions to capitalize a string
%%

%% BAD: makes unnecessary use of manual recursion
recurse(S) ->
    lists:reverse(recurse(S, [])).

recurse([], Acc) ->
    Acc;
recurse([H | T], Acc) ->
    NewAcc = [string:to_upper(H) | Acc],
    recurse(T, NewAcc).

%% GOOD: uses a fold instead to achieve the same result,
%% but this time more safely, and with fewer lines of code
fold(S) ->
    Result = lists:foldl(fun fold_fun/2, [], S),
    lists:reverse(Result).

fold_fun(C, Acc) ->
    [string:to_upper(C) | Acc].

%% BEST: in this case, a list comprehension yields the
%% simplest implementation (assuming we ignore the fact
%% that string:to_upper can also be used directly on strings)
comprehension(S) ->
    [string:to_upper(C) || C <- S].
