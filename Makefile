SHELL = /bin/bash
PROJECT = erlang_v8

DEPS = jsx
dep_jsx = https://github.com/talentdeficit/jsx.git master

TEST_DEPS = ct_helper
dep_ct_helper = https://github.com/extend/ct_helper.git master

CT_SUITES = port

include erlang.mk
