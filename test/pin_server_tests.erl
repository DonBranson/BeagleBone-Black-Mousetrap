-module(pin_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pin_record.hrl").

fixture_startup_test() ->
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}).

init_returns_ok_test() ->
  Actual = pin_server:init(argument),
  ?assertEqual({ok, running}, Actual).

fixture_teardown_test() ->
  meck:unload().
