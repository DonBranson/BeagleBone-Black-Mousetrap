-module(mousetrap_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	inets:start(),
	ssl:start(),
	notification_library:notify("@channel Mousetrap starting"),
	mousetrap_sup:start_link().

stop(_State) ->
	notification_library:notify("@here Mousetrap stopping"),
  mousetrap_sup:stop(),
	ok.