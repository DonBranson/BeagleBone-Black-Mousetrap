-module(mousetrap_app_tests).

-include_lib("eunit/include/eunit.hrl").

fixture_startup_test() ->
  application:set_env(mousetrap, slack_token, "test_token"),
  meck:expect(notification_library, notify, 1, {ok, 200, [long_list]}).

startup_message_sent_test() ->
  mousetrap_app:start("", ""),
  ?assert(meck:called(notification_library, notify, ["Mousetrap starting"])).