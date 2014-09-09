-module(good).

-callback function1(binary(), State) -> {ok, State}.
