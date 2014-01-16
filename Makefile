make:
	rebar get-deps compile

test:
	rebar eunit skip_deps=true

.PHONY: test
