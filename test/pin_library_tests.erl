-module(pin_library_tests).

-include_lib("../src/pin_record.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ExportFile, "some_file").

fixture_startup_test() ->
  application:set_env(mousetrap, pins_export_file, ?ExportFile),
  meck:new(file, [unstick, passthrough]),
  meck:expect(file, write_file, 2, ok).

initialize_pins_exports_pins_empty_test() ->
  Actual = pin_library:initialize_pins([]),

  ?assertEqual(ok, Actual),
  ?assertNot(meck:called(file, write_file, [])).

initialize_pins_exports_bank0_pins_test() ->
  validate_pin_export("0", 5, "5").

initialize_pins_exports_pins_bank1_test() ->
  validate_pin_export("1", 5, "35").

initialize_pins_exports_pins_bank2_test() ->
  validate_pin_export("2", 5, "65").

initialize_pins_exports_pins_bank3_test() ->
  validate_pin_export("3", 5, "95").

validate_pin_export(Bank, BankPin, SoftwarePin) ->
  Actual = pin_library:initialize_pins([#pin{bank = list_to_atom("gpio" ++ Bank), bank_pin = BankPin}]),

  ?assertEqual(ok, Actual),
  ?assert(meck:called(file, write_file, [?ExportFile, SoftwarePin])).

fixture_teardown_test() ->
  meck:unload().