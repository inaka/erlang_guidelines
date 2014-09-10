-module(record_spec).

-record(state, {field1, field2}).

-opaque state() :: #state{}.

-export_type([state/0]).

-export([bad/1, good/1]).

-spec bad(#state{}) -> {any(), #state{}}.
bad(State) -> {State#state.field1, State}.

-spec good(state()) -> {any(), state()}.
good(State) -> {State#state.field1, State}.
