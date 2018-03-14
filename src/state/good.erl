-module(good).

-behaviour(gen_server).

-export([start/1, increment/0, retrieve/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-record(good_state, {value :: pos_integer()}).

-type state() :: #good_state{}.

-spec start(pos_integer()) -> {ok, pid()}.
start(InitialValue) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, InitialValue, []).

-spec retrieve() -> pos_integer().
retrieve() -> gen_server:call(?MODULE, retrieve).

-spec increment() -> ok.
increment() -> gen_server:cast(?MODULE, increment).


-spec init(pos_integer()) -> {'ok', state()}.
init(InitialValue) -> {ok, #good_state{value = InitialValue}}.

-spec handle_call(retrieve, {pid(), term()}, state()) ->
        {'reply', pos_integer(), state()}.
handle_call(retrieve, _From, State) ->
  {reply, State#good_state.value, State}.

-spec handle_cast(increment, state()) -> {'noreply', state()}.
handle_cast(increment, State) ->
  {noreply, State#good_state{value = State#good_state.value + 1}}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(
        normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.
terminate(_Reason, _State) -> ok.

-spec code_change(term() | {down, term()}, state(), term()) -> {'ok', state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
