-module(sleepy).

-export([bad/1, good/1]).

bad(_Config) ->
    something:that(kicks, off, a, {background, task}),
    timer:sleep(1000),
    done = that_task:status().

good(_Config) ->
    something:that(kicks, off, a, {background, task}),
    ktn_task:wait_for(fun() -> that_task:status() end, done).
