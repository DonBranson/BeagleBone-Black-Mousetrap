-module(mousetrap_server).

-include_lib("../src/pin_record.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_info/2, code_change/3, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Arguments) ->
  {ok, running}.

handle_cast({open, Pin}, State) ->
  notification_library:notify(format_message("Mousetrap '~s' has caught a mouse.", Pin)),
  {noreply, State};
handle_cast({closed, Pin}, State) ->
  notification_library:notify(format_message("Mousetrap '~s' has been reset.", Pin)),
  {noreply, State}.

format_message(Str, Pin) ->
  lists:flatten(io_lib:format(Str, [Pin#pin.description])).

terminate(_Reason, _State) ->
  ok.

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
