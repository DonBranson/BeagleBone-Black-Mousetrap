-module(mousetrap_app_tests).

-include_lib("eunit/include/eunit.hrl").

export_file_set_test() ->
  mousetrap_app:start("", "").