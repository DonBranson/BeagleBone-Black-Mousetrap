-module(mousetrap_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  ok.

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
