PROJECT = mousetrap
DEPS = meck
include erlang.mk

run: clean tests
	erl -pa ebin deps/*/ebin -eval "application:ensure_all_started(mousetrap, permanent)."
