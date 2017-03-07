-module(otp_encapsulation).

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([good/0, bad/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

good() ->
  %% Good, because this function is an API call that encapsulates our gen_server implementation:
  gen_server:call(?MODULE, do_good).

bad() ->
  %% Bad, because we're sending an event to some other process whose implementation is defined
  %% in another module. This breaks encapsulation:
  gen_fsm:send_all_state_event(some_fsm, make_everyone_sad).

%% gen_server implementation

init(nil) ->
  {ok, nostate}.

handle_call(do_good, _From, State) ->
  {reply, yay, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
