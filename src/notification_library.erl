-module(notification_library).

-export([notify/1]).

notify(Message) ->
  {ok, UserName} = application:get_env(mousetrap, slack_user),
  {ok, SlackChannel} = application:get_env(mousetrap, slack_channel),
  {ok, Token} = application:get_env(mousetrap, slack_token),
  URL = lists:flatten(io_lib:format("https://slack.com/api/chat.postMessage?token=~s&channel=~s&username=~s&text=~s", [
    http_uri:encode(Token), http_uri:encode(SlackChannel), http_uri:encode(UserName), http_uri:encode(Message)
  ])),
  httpc:request(URL).