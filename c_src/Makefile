SHELL = /bin/bash

ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

LIB_DIR := ../lib
PRIV_DIR := ../priv

V8_REF := 49744859536225e7ac3b726e5b019dd99e127e6f
V8_DIR := $(LIB_DIR)/v8-$(V8_REF)
V8_LIB := $(V8_DIR)/out/$(BUILD_ARCH).release
V8_URL := https://codeload.github.com/v8/v8/tar.gz/$(V8_REF)

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc

.PHONY: all v8 local-clean local-clean-all

all:: v8 

clean:: local-clean

clean-all: local-clean-all

local-clean:
	rm -rf $(TARGET_BIN)

local-clean-all::
	rm -rf $(V8_DIR)

v8: $(TARGET_BIN)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(V8_DIR): $(LIB_DIR)
	curl -L $(V8_URL) | tar xvz -C $(LIB_DIR) 

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

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC) 
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
