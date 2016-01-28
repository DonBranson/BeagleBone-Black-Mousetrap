PROJECT = mousetrap
PROJECT_DESCRIPTION =  App for the BeagleBone Black to monitor mousetraps and alert on activity.
PROJECT_VERSION = 1.0.3
PROJECT_REGISTERED = mousetrap

TEST_DEPS = meck
DEPS = slack
dep_slack = git git@github.com:DonBranson/slack.git

include erlang.mk

observe:
	erl -sname observer -hidden -setcookie mousetrap -run observer
