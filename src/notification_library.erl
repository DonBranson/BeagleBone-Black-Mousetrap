-module(notification_library).

-export([notify/1]).

notify(Message) ->
  {ok, Token} = application:get_env(mousetrap, slack_token),
  slacker_chat:post_message(Token, <<"#mousetrap">>, Message, <<"mousetrap">>).
