-module(pin_library_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/pin_record.hrl").

-define(ExportFile, "export_file").
-define(PinsRootDir, "pins_root").

fixture_startup_test() ->
  application:set_env(mousetrap, pins_export_file, ?ExportFile),
  application:set_env(mousetrap, pins_root_directory, ?PinsRootDir),
  meck:new(file, [unstick, passthrough]),
  meck:expect(file, write_file, 2, ok),
  meck:expect(file, close, 1, ok).

initialize_pins_exports_pins_empty_test() ->
  Actual = pin_library:initialize_pins([]),

  ?assertEqual(ok, Actual),
  ?assertNot(meck:called(file, write_file, [])).

initialize_pins_exports_bank0_pins_test() ->
  validate_pin_export("0", 5, "5").

initialize_pins_exports_pins_bank1_test() ->
  validate_pin_export("1", 5, "37").

initialize_pins_exports_pins_bank2_test() ->
  validate_pin_export("2", 5, "69").

initialize_pins_exports_pins_bank3_test() ->
  validate_pin_export("3", 5, "101").

initalize_sets_pins_to_input_mode_test() ->
  meck:reset(file),
  pin_library:initialize_pins([
    #pin{bank = gpio0, bank_pin = 5},
    #pin{bank = gpio1, bank_pin = 5}
  ]),

  ?assert(meck:called(file, write_file, [?PinsRootDir ++ "5/direction", "in"])),
  ?assert(meck:called(file, write_file, [?PinsRootDir ++ "37/direction", "in"])).

read_pin_state_converts_one_to_open_test() ->
  pin_read_setup(),
  Actual = pin_library:read_pin_state(#pin{bank=gpio0, bank_pin=5}),
  ?assertEqual(open, Actual).

read_pin_state_converts_non_one_to_close_test() ->
  pin_read_setup(),
  Actual = pin_library:read_pin_state(#pin{bank=gpio1, bank_pin=5}),
  ?assertEqual(closed, Actual).

read_pin_state_closes_file_test() ->
  pin_read_setup(),
  pin_library:read_pin_state(#pin{bank=gpio0, bank_pin=5}),
  ?assert(meck:called(file, close, [iodevice1])).

validate_pin_export(Bank, BankPin, SoftwarePin) ->
  meck:reset(file),
  Actual = pin_library:initialize_pins([#pin{bank = list_to_atom("gpio" ++ Bank), bank_pin = BankPin}]),

  ?assertEqual(ok, Actual),
  ?assert(meck:called(file, write_file, [?ExportFile, SoftwarePin])).

pin_read_setup() ->
  meck:reset(file),
  meck:expect(file, open,
    fun(ValueFile, [read, raw]) ->
      case ValueFile of
        ?PinsRootDir ++ "5/value" -> {ok, iodevice1};
        _ -> {ok, iodevice2}
      end
    end),
  meck:expect(file, read,
    fun(IoDevice, 1) ->
      case IoDevice of
        iodevice1 -> {ok, "1"};
        _ -> {ok, "0"}
      end
    end).

fixture_teardown_test() ->
  meck:unload().