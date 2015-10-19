-module(notification_library_tests).

-include_lib("eunit/include/eunit.hrl").

fixture_startup_test() ->
  application:set_env(mousetrap, slack_token, test_token),
  meck:new(slacker, [unstick]),
  meck:expect(slacker_chat, post_message, 4, {ok, [long_list]}).

notify_forwards_to_slacker_test() ->
  TestMessage = <<"Test Message">>,
  notification_library:notify(TestMessage),
  ?assert(meck:called(slacker_chat, post_message, [test_token, <<"#mousetrap">>, TestMessage, <<"mousetrap">>])).