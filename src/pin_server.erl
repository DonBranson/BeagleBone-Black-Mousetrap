-module(pin_server).
-include_lib("../src/pin_record.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_info/2, code_change/3, handle_cast/2]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_Arguments) ->
  {ok, running}.

handle_cast({start, PinServerId, Pin, QuietSeconds, Client}, _State) ->
  {ok, PinCheckIntervalInSeconds} = application:get_env(mousetrap, pin_check_interval_seconds),
  notification_library:notify(format_message("Start ~p watching pin ~p and notifying ~p", [PinServerId, Pin, Client])),
  pin_library:initialize_pins([Pin]),
  timer:apply_interval(PinCheckIntervalInSeconds * 1000, ?MODULE, check_pin, [PinServerId]),
  {noreply, [Pin, QuietSeconds, Client, pin_library:read_pin_state(Pin), 0]}.

terminate(_Reason, _State) ->
  ok.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_message(Str, Args) ->
  lists:flatten(io_lib:format(Str, Args)).
