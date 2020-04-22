.PHONY: all compile clean test

all: compile

deps:
	@rebar3 get-deps

compile:
	@rebar3 compile

clean:
	@rebar3 clean -a

test:
	@rebar3 test
