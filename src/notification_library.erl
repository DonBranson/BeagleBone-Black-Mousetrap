-module(notification_library).

-import(http_uri, [encode/1]).
-export([notify/1]).

notify(Message) ->
  {ok, UserName} = application:get_env(mousetrap, slack_user),
  {ok, SlackChannel} = application:get_env(mousetrap, slack_channel),
  {ok, Token} = application:get_env(mousetrap, slack_token),
  URL = lists:flatten(io_lib:format("https://slack.com/api/chat.postMessage?token=~s&channel=~s&username=~s&text=~s", [
    encode(Token), encode(SlackChannel), encode(UserName), encode(Message)
  ])),
  httpc:request(URL).