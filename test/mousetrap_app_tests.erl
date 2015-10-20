-module(mousetrap_app_tests).

-include_lib("eunit/include/eunit.hrl").

fixture_startup_test() ->
  application:set_env(mousetrap, slack_token, "test_token"),
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}),
  meck:expect(mousetrap_sup, start_link, 0, {ok, pid}).

start_sends_notification_test() ->
  mousetrap_app:start("", ""),
  ?assert(meck:called(notification_library, notify, ["Mousetrap starting"])).

start_starts_mousetrap_supervisor_test() ->
  ?assert(meck:called(mousetrap_sup, start_link, [])).

fixture_teardown_test() ->
  meck:unload().