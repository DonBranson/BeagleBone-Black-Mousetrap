PROJECT = mousetrap
TEST_DEPS = meck
DEPS = slacker
dep_slacker = git git@github.com:julienXX/slacker.git master

include erlang.mk

run:
	erl -pa ebin deps/*/ebin -eval "application:ensure_all_started(mousetrap, permanent)." \
		-setcookie mousetrap \
		-sname mousetrap \
		-config rel/files/sys.config

observe:
	erl -sname observer -hidden -setcookie mousetrap -run observer
