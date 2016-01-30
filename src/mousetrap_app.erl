-module(mousetrap_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  [{loaded,LoadedModuleList}, _, _, _, _, _ ] = application:info(),
  {mousetrap, _Description, Version} = lists:keyfind(mousetrap, 1, LoadedModuleList),
  notification_library:notify("@channel Mousetrap " ++ Version ++ " starting"),
	mousetrap_sup:start_link().

stop(_State) ->
	notification_library:notify("@channel Mousetrap stopping"),
  mousetrap_sup:stop(),
	ok.
