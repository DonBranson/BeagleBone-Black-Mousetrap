-module(pin_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pin_record.hrl").

fixture_startup_test() ->
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}).

init_returns_ok_test() ->
  Actual = pin_server:init(argument),
  ?assertEqual({ok, running}, Actual).

start_link_delegates_test() ->
  {ok, Actual} = pin_server:start_link(),
  ?assert(is_pid(Actual)).

terminate_returns_ok_test() ->
  Actual = pin_server:terminate(reason, state),
  ?assertEqual(ok, Actual).

fixture_teardown_test() ->
  meck:unload().
