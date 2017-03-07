-module(message_formatting).

bad(Pid) ->
    %% These are error-prone and confusing, as there's no indication of what the message
    %% is supposed to be used for on the receiving end:
    Pid ! -1,
    gen_server:cast(Pid, self()),
    %% These are better, but still less readable, as first tuple element
    %% is not being used to tag the message:
    gen_server:call(Pid, {123, set_count}),
    gen_server:call(Pid, {make_ref(), notify, <<"something">>}).

good(Pid) ->
    %% These are examples of well-formatted messages:
    gen_server:cast(Pid, reload_config),
    gen_server:call(Pid, {set_count, 123}),
    gen_server:call(Pid, get_count),
    Pid ! {notify, make_ref(), <<"hello world">>}.
