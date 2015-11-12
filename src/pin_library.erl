-module(pin_library).
-include_lib("pin_record.hrl").

-export([initialize_pins/1, read_pin_state/1, get_software_pin/1]).

initialize_pins([]) -> ok;
initialize_pins([#pin{} = Pin|T]) ->
  ok = export_pin(Pin),
  ok = set_pin_for_input(Pin),
  initialize_pins(T).

export_pin(Pin) ->
  {ok, ExportFile} = application:get_env(mousetrap, pins_export_file),
  file:write_file(ExportFile, get_software_pin(Pin)).

set_pin_for_input(Pin) ->
  {ok, PinsRootDirectory} = application:get_env(mousetrap, pins_root_directory),
  file:write_file(PinsRootDirectory ++ get_software_pin(Pin) ++ "/direction", "in").

read_pin_state(#pin{} = Pin) ->
  {ok, PinsRootDirectory} = application:get_env(mousetrap, pins_root_directory),
  {ok, IoDevice} = file:open(PinsRootDirectory ++ get_software_pin(Pin) ++ "/value", [read, raw]),
  State = transform_state(file:read(IoDevice, 1)),
  ok = file:close(IoDevice),
  State.

transform_state({ok, "1"}) -> open;
transform_state({ok, _}) -> closed.

get_software_pin(#pin{bank=gpio0, bank_pin=Pin}) -> get_software_pin(0 * 32 + Pin);
get_software_pin(#pin{bank=gpio1, bank_pin=Pin}) -> get_software_pin(1 * 32 + Pin);
get_software_pin(#pin{bank=gpio2, bank_pin=Pin}) -> get_software_pin(2 * 32 + Pin);
get_software_pin(#pin{bank=gpio3, bank_pin=Pin}) -> get_software_pin(3 * 32 + Pin);
get_software_pin(SoftwarePin) -> integer_to_list(SoftwarePin).