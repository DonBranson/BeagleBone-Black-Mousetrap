-module(mousetrap_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  io:format("pins_export_file: ~p~n", [application:get_env(pins_export_file)]),
	mousetrap_sup:start_link().

stop(_State) ->
	ok.
