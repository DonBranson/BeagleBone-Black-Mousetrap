-module(pin_library).
-include_lib("pin_record.hrl").

-export([initialize_pins/1]).

initialize_pins([]) -> ok;
initialize_pins([#pin{} = Pin|_T]) ->
  export_pin(Pin),
  set_pin_for_input(Pin).

export_pin(Pin) ->
  {ok, ExportFile} = application:get_env(mousetrap, pins_export_file),
  file:write_file(ExportFile, get_software_pin(Pin)).

set_pin_for_input(Pin) ->
  {ok, PinsRootDirectory} = application:get_env(mousetrap, pins_root_directory),
  io:format("aaa ~p~n", [PinsRootDirectory ++ get_software_pin(Pin) ++ "/directory"]),
  file:write_file(PinsRootDirectory ++ get_software_pin(Pin) ++ "/direction", "in").

get_software_pin(#pin{bank=gpio0, bank_pin=Pin}) -> get_software_pin(0 + Pin);
get_software_pin(#pin{bank=gpio1, bank_pin=Pin}) -> get_software_pin(30 + Pin);
get_software_pin(#pin{bank=gpio2, bank_pin=Pin}) -> get_software_pin(60 + Pin);
get_software_pin(#pin{bank=gpio3, bank_pin=Pin}) -> get_software_pin(90 + Pin);
get_software_pin(SoftwarePin) -> integer_to_list(SoftwarePin).