PROJECT = mousetrap
PROJECT_DESCRIPTION =  App for the BeagleBone Black to monitor mousetraps and alert on activity.
PROJECT_VERSION = 1.0.4
PROJECT_REGISTERED = mousetrap

TEST_DEPS = meck
DEPS = slack

include erlang.mk

observe:
	erl -sname observer -hidden -setcookie mousetrap -run observer

deploy: rel
	rsync -a _rel/mousetrap/ bbb01:mousetrap/
