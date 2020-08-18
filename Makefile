.PHONY: all deps compile xref test clean distclean build dialyzer

all: distclean deps compile xref dialyzer test

deps:
	@./rebar3 deps
	@./rebar3 get-deps

compile:
	@./rebar3 compile

xref:
	@./rebar3 xref

test: compile
	@./rebar3 eunit

clean:
	@./rebar3 clean

distclean:
	@./rebar3 clean --all

build:
	@./rebar3 compile
	@./rebar3 xref

dialyzer:
	@./rebar3 as test dialyzer
