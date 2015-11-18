-module(pin_server).
-include_lib("../src/pin_record.hrl").

-behaviour(gen_server).

-import(pin_library, [read_pin_state/1, initialize_pins/1]).
-export([start_link/4, init/1, terminate/2, handle_call/3, handle_info/2, code_change/3, handle_cast/2, check_pin/1]).

start_link(PinServerId, Pin, QuietSeconds, Client) ->
  gen_server:start_link({local, PinServerId}, ?MODULE, [PinServerId, Pin, QuietSeconds, Client], []).

init([PinServerId, Pin, QuietSeconds, Client]) ->
  gen_server:cast(PinServerId, {start, PinServerId, Pin, QuietSeconds, Client}),
  {ok, running}.

% Check the pin state every second. When there's an event, quiesce for QuietSeconds.
handle_cast({start, PinServerId, Pin, QuietSeconds, Client}, _State) ->
  {ok, PinCheckIntervalInSeconds} = application:get_env(mousetrap, pin_check_interval_seconds),
  initialize_pins([Pin]),
  PinState = read_pin_state(Pin),
  notification_library:notify(format_message("Start ~p watching pin ~p (now ~p) and notifying ~p", [PinServerId, Pin, PinState, Client])),
  {ok, _TimerRef} = timer:apply_interval(PinCheckIntervalInSeconds * 1000, ?MODULE, check_pin, [PinServerId]),
  {noreply, [Pin, QuietSeconds, Client, PinState, 0]};

%Quiet period ending, re-read pin state.
handle_cast(check_pin, [Pin, QuietSeconds, Client, _, 1]) ->
  {noreply, [Pin, QuietSeconds, Client, read_pin_state(Pin), 0]};

%Not in a quiet period, check for state changes
handle_cast(check_pin, [Pin, QuietSeconds, Client, PinState, 0]) ->
  NewPinState = read_pin_state(Pin),
  NewQuietSecondsLeft = compare_pin_states(Client, Pin, PinState, NewPinState, QuietSeconds),
  {noreply, [Pin, QuietSeconds, Client, NewPinState, NewQuietSecondsLeft]};

%In a quiet period, decrement count
handle_cast(check_pin, [Pin, QuietSeconds, Client, PinState, QuietSecondsLeft]) -> {noreply, [Pin, QuietSeconds, Client, PinState, QuietSecondsLeft - 1]}.

terminate(_Reason, _State) ->
  ok.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

check_pin(PinServerId) -> gen_server:cast(PinServerId, check_pin).

%If there's a state change, set the countdown to QuietSeconds
compare_pin_states(_, _, PinState, PinState, _) -> 0;
compare_pin_states(Client, Pin, open, closed = NewState, QuietSeconds) -> handle_transition(Client, Pin, NewState), QuietSeconds;
compare_pin_states(Client, Pin, closed, open = NewState, QuietSeconds) -> handle_transition(Client, Pin, NewState), QuietSeconds.

handle_transition(Client, Pin, NewState) ->
  gen_server:cast(Client, {NewState, Pin}).

format_message(Str, Args) ->
  lists:flatten(io_lib:format(Str, Args)).
