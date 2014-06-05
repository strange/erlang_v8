SHELL = /bin/bash
PROJECT = erlang_v8

DEPS = jsx
dep_jsx = pkg://jsx master

TEST_DEPS = ct_helper
dep_ct_helper = https://github.com/extend/ct_helper.git master

CT_SUITES = port

ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

V8_REF := 49744859536225e7ac3b726e5b019dd99e127e6f
V8_DIR := lib/v8-$(V8_REF)
V8_LIB := $(V8_DIR)/out/$(BUILD_ARCH).release
V8_URL := https://github.com/v8/v8/archive/$(V8_REF).tar.gz

TARGET_BIN := priv/erlang_v8
TARGET_SRC := c_src/erlang_v8.cc

include erlang.mk

.PHONY: v8 local-clean local-clean-all

app: v8

clean: local-clean

clean-all: local-clean-all

local-clean:
	rm -rf $(TARGET_BIN)

local-clean-all:: 
	rm -rf $(V8_DIR)

v8: $(TARGET_BIN)

lib:
	mkdir -p lib

$(V8_DIR): lib
	curl -L $(V8_URL) | tar xvz -C lib

$(V8_DIR)/build/gyp: $(V8_DIR)
	@cd $(V8_DIR) && make dependencies
	@touch $@

$(V8_LIB)/libv8_base.$(BUILD_ARCH).a: $(V8_DIR)/build/gyp
	@cd $(V8_DIR) && make $(BUILD_ARCH).release werror=no
	@touch $@
	@cp $(V8_LIB)/obj.target/tools/gyp/*.a $(V8_LIB) 2> /dev/null || :
	@cp $(V8_LIB)/obj.target/third_party/icu/*.a $(V8_LIB) 2> /dev/null || :

$(TARGET_SRC): $(V8_LIB)/libv8_base.$(BUILD_ARCH).a
	@:

$(TARGET_BIN): $(TARGET_SRC)
	@mkdir -p priv
ifeq ($(OS),Darwin)
	# We need to link libstdc++ as XCode defaults to libc++, and use slightly
	# different flags, on OS X. The following assumes Mavericks, XCode and
	# default compiler (clang).
	g++ -Iinclude $(TARGET_SRC) \
		-stdlib=libstdc++ \
		-o $(TARGET_BIN) \
		$(V8_LIB)/libv8_{base.$(BUILD_ARCH),snapshot}.a \
		$(V8_LIB)/libicu{uc,i18n,data}.a \
		-I $(V8_DIR)/include \
		-lpthread \
		-v
else
	g++ -Iinclude $(TARGET_SRC) \
		-o $(TARGET_BIN) \
		-Wl,--start-group \
		$(V8_LIB)/libv8_{base.$(BUILD_ARCH),snapshot}.a \
		$(V8_LIB)/libicu{uc,i18n,data}.a \
		-Wl,--end-group \
		-I $(V8_DIR)/include \
		-lrt \
		-lpthread \
		-v
endif
