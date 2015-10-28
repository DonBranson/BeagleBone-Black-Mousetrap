-module(pin_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pin_record.hrl").

fixture_startup_test() ->
  meck:expect(pin_library, initialize_pins, 1, ok),
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}).

init_returns_ok_test() ->
  Actual = pin_server:init(argument),
  ?assertEqual({ok, running}, Actual).

start_link_delegates_test() ->
  {ok, Actual} = pin_server:start_link(),
  ?assert(is_pid(Actual)).

handle_cast_sends_notification_test() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  {noreply, _LoopData} = pin_server:handle_cast({start, pin_server_5, Pin, 60, pin_server_tests}, []),
  ?assert(meck:called(notification_library, notify, ["Start pin_server_5 watching pin {pin,gpio0,5,undefined} and notifying pin_server_tests"])).

handle_cast_calls_initialize_pins_test() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  pin_server:handle_cast({start, pin_server_5, Pin, 60, pin_server_tests}, []),
  ?assert(meck:called(pin_library, initialize_pins, [[Pin]])).

terminate_returns_ok_test() ->
  Actual = pin_server:terminate(reason, state),
  ?assertEqual(ok, Actual).

fixture_teardown_test() ->
  meck:unload().
