REBAR := "./rebar3"

.PHONY: build test deps rel

all: deps build

build:
	$(REBAR) compile

test:
	$(REBAR) ct

deps:
	$(REBAR) get-deps

rel: test
	$(REBAR) release
