-module(mousetrap_sup_tests).

-include_lib("eunit/include/eunit.hrl").

fixture_startup_test() ->
  application:set_env(mousetrap, quiet_minutes, 5),
  make_pin_list(),
  ok.

init_returns_ok_test() ->
  {Actual, {_, _}} = mousetrap_sup:init([]),
  ?assertEqual(ok, Actual).

init_sets_restart_strategy_test() ->
  {_, {{Actual, _, _}, _}} = mousetrap_sup:init([]),
  ?assertEqual(one_for_one, Actual).

init_sets_restart_parameters_test() ->
  {_, {{_, MaxRestart, MaxTime}, _}} = mousetrap_sup:init([]),
  ?assertEqual(1, MaxRestart),
  ?assertEqual(1, MaxTime).

init_fails_if_no_pins_to_watch_test() ->
  application:set_env(mousetrap, pins, []),
  Actual = mousetrap_sup:init([]),
  ?assertEqual({error, no_pins_to_watch}, Actual).

init_returns_a_list_of_pin_server_specs_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [_M|_P]}} = mousetrap_sup:init([]).

init_returns_mousetrap_server_spec_first_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [{MousetrapServerTag, _, _, _, _, _} | _ServerSpecs]}} = mousetrap_sup:init([]),
  ?assertEqual(mousetrap_server, MousetrapServerTag).

init_specifies_the_mousetrap_server_start_function_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [{_, {MouseTrapServerModule, MouseTrapServerStartFunction, []}, _, _, _, _} | _ServerSpecs]}} = mousetrap_sup:init([]),
  ?assertEqual(mousetrap_server, MouseTrapServerModule),
  ?assertEqual(start_link, MouseTrapServerStartFunction).

init_declares_the_mousetrap_server_restart_strategy_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [{_, {_, _, []}, RestartStrategy, _, _, _} | _ServerSpecs]}} = mousetrap_sup:init([]),
  ?assertEqual(permanent, RestartStrategy).

init_declares_the_mousetrap_server_shutdown_strategy_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [{_, {_, _, []}, _, ShutdownStrategy, _, _} | _ServerSpecs]}} = mousetrap_sup:init([]),
  ?assertEqual(brutal_kill, ShutdownStrategy).

init_declares_the_mousetrap_server_is_a_worker_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [{_, {_, _, []}, _, _, Type, _} | _ServerSpecs]}} = mousetrap_sup:init([]),
  ?assertEqual(worker, Type).

init_declares_the_mousetrap_dependencies_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [{_, {_, _, []}, _, _, _, Dependencies} | _ServerSpecs]}} = mousetrap_sup:init([]),
  ?assertEqual([mousetrap_server], Dependencies).

init_declares_a_pin_server_for_each_watched_pin_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [_ | PinServers]}} = mousetrap_sup:init([]),
  PinserverTags = [pin_server_5, pin_server_48, pin_server_31, pin_server_30],
  validate_tags(PinserverTags, PinServers).

validate_tags([], []) -> ok;
validate_tags([ExpectedPinServerTag | PinServerTags], [PinServer | PinServers]) ->
  {ActualTag, _, _, _, _, _} = PinServer,
  ?assertEqual(ExpectedPinServerTag, ActualTag),
  validate_tags(PinServerTags, PinServers).

init_specifies_the_pin_server_start_function_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [_ | PinServers]}} = mousetrap_sup:init([]),
  validate_start_function(PinServers).

validate_start_function([]) -> ok;
validate_start_function([PinServer | PinServers]) ->
  {_, {ActualModule, ActualStartFunction, ActualArgs}, _, _, _, _} = PinServer,
  ?assertEqual(pin_server, ActualModule),
  ?assertEqual(start_link, ActualStartFunction),
  ?assertEqual([], ActualArgs),
  validate_start_function(PinServers).

init_declares_the_pin_server_restart_strategy_test() ->
  make_pin_list(),
  {_, {{_, _, _}, [_ | PinServers]}} = mousetrap_sup:init([]),
  validate_restart_strategy(PinServers).

validate_restart_strategy([]) -> ok;
validate_restart_strategy([PinServer | PinServers]) ->
  {_, _, ActualRestartStrategy, _, _, _} = PinServer,
  ?assertEqual(permanent, ActualRestartStrategy),
  validate_restart_strategy(PinServers).

fixture_teardown_test() ->
  meck:unload().

make_pin_list() ->
  application:set_env(mousetrap, pins,
    [
      {gpio0, 30, "1 (over workshop door)"},
      {gpio0, 31, "2 (by basement freezer)"},
      {gpio1, 16, "3 (in the kitchen pantry)"},
      {gpio0, 5, "4 (Not yet wired)"}
    ]
  ).