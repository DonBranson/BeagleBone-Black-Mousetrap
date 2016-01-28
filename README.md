Mousetrap
===

This is an HA Erlang mousetrap application that runs on the BeagleBone Black. It monitors mousetraps and notifies via Slack when a mouse is caught. This is a re-work of the original Erlang mousetrap application I built in 2013.

Requirements
===

* [A BeagleBone Black](http://beagleboard.org/BLACK)
* Erlang runtime system. I used [kerl](https://github.com/yrashk/kerl) to install 17.4 at /home/root/erlangs/17.4
* A [Slack account](http://slack.com) with an [authentication token](https://api.slack.com/web#basics) and a channel for posting

Sample Config
===

    [
      {mousetrap, [
        {pins_export_file, "/sys/class/gpio/export"},
        {pins_root_directory, "/sys/class/gpio/gpio"},
        {pins, [
          {gpio0, 30, "1 (over workshop door)"},
          {gpio0, 31, "2 (by basement freezer)"},
          {gpio1, 16, "3 (in the kitchen pantry)"},
          {gpio0, 5, "4 (Not yet wired)"}
        ]},
        {quiet_minutes, 30},
        {pin_check_interval_seconds, 1}
      ]},
      {slack, [
        {slack_user, "mousetrap"},
        {slack_channel, "#mousetrap"},
        {slack_token, "<slack token here>"}
      ]}
    ].


Sample Slack Messages
===

    mousetrap BOT [7:40 PM]
    Mousetrap starting
    Start pin_server_5 watching pin {pin,gpio0,5,"4 (Not yet wired)"} (now closed) and notifying mousetrap_server
    Start pin_server_48 watching pin {pin,gpio1,16,"3 (in the kitchen pantry)"} (now closed) and notifying mousetrap_server
    Start pin_server_31 watching pin {pin,gpio0,31,"2 (by basement freezer)"} (now closed) and notifying mousetrap_server
    Start pin_server_30 watching pin {pin,gpio0,30,"1 (over workshop door)"} (now closed) and notifying mousetrap_server

    mousetrap BOT [12:44 AM]
    Mousetrap '3 (in the kitchen pantry)' has caught a mouse.

    mousetrap BOT [7:46 AM]
    Mousetrap '3 (in the kitchen pantry)' has been reset.

Systemd Config
===

mousetrap.service:

    [Unit]
    Description=Mousetrap service
    Requires=ntpd.service
    After=ntpd.service

    [Service]
    ExecStart=/home/root/runner foreground
    ExecStop=/home/root/runner stop
    SyslogIdentifier=mousetrap
    Restart=on-failure
    RestartSec=10
    TimeoutStartSec=60

    [Install]
    WantedBy=multi-user.target

runner:

    #!/bin/sh

    export HOME=/home/root
    export PATH=/usr/local/bin:/usr/bin:/bin
    . /home/root/erlangs/17.4/activate
    /home/root/mousetrap/bin/mousetrap $*
