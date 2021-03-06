-module(mousetrap_sup).
-behaviour(supervisor).

-include_lib("pin_record.hrl").
-import(pin_library, [get_software_pin/1]).
-export([init/1, start_link/0, stop/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  exit(whereis(mousetrap_sup), kill).

init([]) ->
  {ok, Pins} = application:get_env(mousetrap, pins),
  {ok, QuietMinutes} = application:get_env(mousetrap, quiet_minutes),
  notification_library:notify(lists:flatten(io_lib:format("Quiet period set to ~p minutes.", [QuietMinutes]))),
  QuietSeconds = QuietMinutes * 60,
  case Pins of
    [] ->
      {error, no_pins_to_watch};
    _ ->
      MousetrapServer = {mousetrap_server, {mousetrap_server, start_link, []}, permanent, brutal_kill, worker, [mousetrap_server]},
      PinServers = build_list_of_pinserver_specs(Pins, [], QuietSeconds),
      {ok, {{one_for_one, _MaxRestart = 1, _MaxTime = 1}, [MousetrapServer | PinServers]}}
  end.

build_list_of_pinserver_specs([], PinServers, _QuietSeconds) -> PinServers;
build_list_of_pinserver_specs([{Bank, BankPin, Description}|T], PinServers, QuietSeconds) ->
  PinServerId = make_pin_server_id(pin_server, #pin{bank = Bank, bank_pin = BankPin, description = Description}),
  PinChild = {PinServerId,
    {pin_server, start_link, [PinServerId, (#pin{bank = Bank, bank_pin = BankPin, description = Description}), QuietSeconds, mousetrap_server]},
    permanent, brutal_kill, worker, [pin_server, pin_library]
  },
  build_list_of_pinserver_specs(T, [PinChild | PinServers], QuietSeconds).

make_pin_server_id(Atom, Pin) -> list_to_atom(atom_to_list(Atom) ++ "_" ++ get_software_pin(Pin)).