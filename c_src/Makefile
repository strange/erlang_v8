ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LIB_DIR := $(ROOT_DIR)/../lib
PRIV_DIR := $(ROOT_DIR)/../priv

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc report.cc vm.cc

V8_DIR := /usr/include/nodejs/deps/v8

PATH := $(PATH)
SHELL := /bin/bash

.PHONY: all v8 clean distclean local-clean local-distclean

all:: v8

clean:: local-clean

distclean:: local-distclean

local-clean:
	rm -rf $(TARGET_BIN)

local-distclean: local-clean
	rm -rf $(LIB_DIR)

v8: $(TARGET_BIN)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC)
ifeq ($(OS),Darwin)
	# We need to link libstdc++ as XCode defaults to libc++, and use slightly
	# different flags, on OS X. The following assumes Mavericks, XCode and
	# default compiler (clang).
	# TODO: This will not build.
	g++ -I$(V8_DIR)  \
		$(TARGET_SRC) \
		-stdlib=libstdc++ \
		-std=c++0x \
		-o $(TARGET_BIN) \
		-lv8 -lpthread
else
	g++ \
		-I$(V8_DIR)/include \
		$(TARGET_SRC) \
		-o $(TARGET_BIN) \
		-Wno-unused-variable \
		-lv8 \
		-lrt \
		-ldl \
		-pthread \
		-std=c++11
endif
