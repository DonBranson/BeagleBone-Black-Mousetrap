-module(mousetrap_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pin_record.hrl").

fixture_startup_test() ->
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}).

init_returns_ok_test() ->
  Actual = mousetrap_server:init(argument),
  ?assertEqual({ok, running}, Actual).

start_link_delegates_test() ->
  {ok, Actual} = mousetrap_server:start_link(),
  ?assert(is_pid(Actual)).

terminate_returns_ok_test() ->
  Actual = mousetrap_server:terminate(reason, state),
  ?assertEqual(ok, Actual).

handle_cast_sends_notification_when_a_mouse_is_caught_test() ->
  {noreply, _LoopData} = mousetrap_server:handle_cast({open, #pin{description = "test pin"}}, state),
  ?assert(meck:called(notification_library, notify, ["@channel Mousetrap test pin has caught a mouse."])).

handle_cast_sends_notification_when_trap_is_reset_test() ->
  {noreply, _LoopData} = mousetrap_server:handle_cast({closed, #pin{description = "test pin"}}, state),
  ?assert(meck:called(notification_library, notify, ["Mousetrap test pin has been reset."])).

fixture_teardown_test() ->
  meck:unload().