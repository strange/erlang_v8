SHELL = /bin/bash
PROJECT = erlang_v8

DEPS = jsx

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

CT_SUITES = port

include erlang.mk
