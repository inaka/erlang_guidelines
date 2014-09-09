-module(bad).

-behaviour(gen_server).

-export([start/1, increment/0, retrieve/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-spec start(pos_integer()) -> {ok, pid()}.
start(InitialValue) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, InitialValue, []).

-spec retrieve() -> pos_integer().
retrieve() -> gen_server:call(?MODULE, retrieve).

-spec increment() -> ok.
increment() -> gen_server:cast(?MODULE, increment).


-spec init(pos_integer()) -> {'ok', pos_integer()}.
init(InitialValue) -> {ok, InitialValue}.

-spec handle_call(retrieve, {pid(), term()}, pos_integer()) ->
        {'reply', pos_integer(), pos_integer()}.
handle_call(retrieve, _From, Value) ->
  {reply, Value, Value}.

-spec handle_cast(increment, pos_integer()) -> {'noreply', pos_integer()}.
handle_cast(increment, Value) ->
  {noreply, Value + 1}.

-spec handle_info(any(), pos_integer()) -> {'noreply', pos_integer()}.
handle_info(_Msg, Value) -> {noreply, Value}.

-spec terminate(
        normal | shutdown | {shutdown, term()} | term(), pos_integer()) -> 'ok'.
terminate(_Reason, _Value) -> ok.

-spec code_change(term() | {down, term()}, pos_integer(), term()) ->
        {'ok', pos_integer()}.
code_change(_OldVersion, Value, _Extra) -> {ok, Value}.
