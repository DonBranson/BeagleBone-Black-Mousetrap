PROJECT = mousetrap
TEST_DEPS = meck
DEPS = slacker
dep_slacker = git git@github.com:julienXX/slacker.git master

include erlang.mk

run: clean tests
	erl -pa ebin deps/*/ebin -eval "application:ensure_all_started(mousetrap, permanent)." -config ~/mousetrap.config
