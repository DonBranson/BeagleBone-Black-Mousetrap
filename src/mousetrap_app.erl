-module(mousetrap_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	notification_library:notify("Mousetrap starting"),
	mousetrap_sup:start_link().

stop(_State) ->
	ok.