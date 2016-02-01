-module(mousetrap_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	notification_library:notify("@channel Mousetrap " ++ get_application_version() ++ " starting"),
	mousetrap_sup:start_link().

stop(_State) ->
	notification_library:notify("@channel Mousetrap stopping"),
  mousetrap_sup:stop(),
	ok.

get_application_version() ->
	[{loaded, LoadedModuleList}, _, _, _, _, _] = application:info(),
	{mousetrap, _Description, Version} = lists:keyfind(mousetrap, 1, LoadedModuleList),
	Version.
