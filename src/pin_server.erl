-module(pin_server).
-include_lib("../src/pin_record.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_info/2, code_change/3, handle_cast/2]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_Arguments) ->
	{ok, running}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
