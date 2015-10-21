-module(mousetrap_sup_tests).

-include_lib("eunit/include/eunit.hrl").

fixture_startup_test() ->
  application:set_env(mousetrap, quiet_minutes, 5),
  make_pin_list(),
  ok.

init_returns_ok_test() ->
  {Actual, _, _} = mousetrap_sup:init([]),
  ?assertEqual(ok, Actual).

init_sets_restart_strategy_test() ->
  {_, {Actual, _, _}, _} = mousetrap_sup:init([]),
  ?assertEqual(one_for_one, Actual).

init_sets_restart_parameters_test() ->
  {_, {_, MaxRestart, MaxTime}, _} = mousetrap_sup:init([]),
  ?assertEqual(1, MaxRestart),
  ?assertEqual(1, MaxTime).

init_fails_if_no_pins_to_watch_test() ->
  application:set_env(mousetrap, pins, []),
  Actual = mousetrap_sup:init([]),
  ?assertEqual({error, no_pins_to_watch}, Actual).

init_returns_a_list_of_pin_server_specs_test() ->
  make_pin_list(),
  {_, {_, _, _}, _ServerSpecs = []} = mousetrap_sup:init([]).

fixture_teardown_test() ->
  meck:unload().

make_pin_list() ->
  application:set_env(mousetrap, pins,
    [
      {gpio0, 30, "1 (over workshop door)"},
      {gpio0, 31, "2 (by basement freezer)"},
      {gpio0, 48, "3 (in the kitchen pantry)"},
      {gpio0, 5, "4 (Not yet wired)"}
    ]
  ).