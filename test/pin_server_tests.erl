-module(pin_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pin_record.hrl").

fixture_startup_test() ->
  application:set_env(mousetrap, pin_check_interval_seconds, 1),
  meck:new([timer], [unstick]),
  meck:expect(pin_library, initialize_pins, 1, ok),
  meck:expect(pin_library, read_pin_state, 1, open_test),
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}),
  meck:expect(timer, apply_interval, 4, {ok, timer_ref}).

init_returns_ok_test() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  Actual = pin_server:init([pin_server_5, Pin, 10, pin_server_tests]),
  ?assertEqual({ok, running}, Actual).

start_link_delegates_test() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  {ok, Actual} = pin_server:start_link(pin_server_5, Pin, 10, pin_server_tests),
  ?assert(is_pid(Actual)).

handle_cast_sends_notification_test() ->
  fire_pin_serverhandle_cast(),
  ?assert(meck:called(notification_library, notify, ["Start pin_server_5 watching pin {pin,gpio0,5,undefined} and notifying pin_server_tests"])).

handle_cast_calls_initialize_pins_test() ->
  Pin = fire_pin_serverhandle_cast(),
  ?assert(meck:called(pin_library, initialize_pins, [[Pin]])).

handle_cast_starts_timer_test() ->
  fire_pin_serverhandle_cast(),
  ?assert(meck:called(timer, apply_interval, [1000, pin_server, check_pin, [pin_server_5]])).

handle_cast_start_returns_state_test() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  Actual = pin_server:handle_cast({start, pin_server_5, Pin, 63, pin_server_tests}, []),
  ?assertEqual({noreply, [Pin, 63, pin_server_tests, open_test, 0]}, Actual).

handle_cast_rereads_pin_state_at_end_of_quiescent_period_test() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  Actual = pin_server:handle_cast(check_pin, [Pin, 63, pin_server_tests, open, 1]),
  ?assertEqual({noreply, [Pin, 63, pin_server_tests, open_test, 0]}, Actual).

handle_cast_recognizes_opening_trap_outside_quiescent_period_test() ->
  meck:expect(pin_library, read_pin_state, 1, open),
  Pin = #pin{bank = gpio0, bank_pin = 5},
  Actual = pin_server:handle_cast(check_pin, [Pin, 63, pin_server_tests, closed, 0]),
  ?assertEqual({noreply, [Pin, 63, pin_server_tests, open, 63]}, Actual).

handle_cast_recognizes_closing_trap_outside_quiescent_period_test() ->
  meck:expect(pin_library, read_pin_state, 1, closed),
  Pin = #pin{bank = gpio0, bank_pin = 5},
  Actual = pin_server:handle_cast(check_pin, [Pin, 63, pin_server_tests, open, 0]),
  ?assertEqual({noreply, [Pin, 63, pin_server_tests, closed, 63]}, Actual).

terminate_returns_ok_test() ->
  Actual = pin_server:terminate(reason, state),
  ?assertEqual(ok, Actual).

fixture_teardown_test() ->
  meck:unload().

fire_pin_serverhandle_cast() ->
  Pin = #pin{bank = gpio0, bank_pin = 5},
  {noreply, _LoopData} = pin_server:handle_cast({start, pin_server_5, Pin, 60, pin_server_tests}, []),
  Pin.
