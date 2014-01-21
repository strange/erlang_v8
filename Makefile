make:
	rebar compile

test:
	rebar eunit skip_deps=true

.PHONY: test
