SHELL = /bin/bash

ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LIB_DIR := $(ROOT_DIR)/../lib
PRIV_DIR := $(ROOT_DIR)/../priv

DEPOT_REF := c6787971b576148c94d8dc11dc3d28e88c971995
DEPOT_DIR := $(LIB_DIR)/depot_tools

V8_REF := 07234f240d2c346ee00e6a1c5a57b6ae861be6b3
V8_DIR := $(LIB_DIR)/v8
V8_LIB := $(V8_DIR)/out/$(BUILD_ARCH).release

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc report.cc vm.cc

GYPFLAGS := -Dv8_use_external_startup_data=0

export PATH := $(PATH):$(DEPOT_DIR)

.PHONY: all v8 local-clean local-clean-all

all:: v8 

clean:: local-clean

distclean:: local-clean-all

local-clean:
	rm -rf $(TARGET_BIN)

local-clean-all:
	rm -rf $(TARGET_BIN)
	rm -rf $(LIB_DIR)

v8: $(TARGET_BIN)

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(DEPOT_DIR): $(LIB_DIR)
ifeq ($(wildcard $(DEPOT_DIR)),)
	cd $(LIB_DIR) && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git;
	cd $(DEPOT_DIR) && git reset --hard $(DEPOT_REF)
endif

$(V8_DIR): $(LIB_DIR) $(DEPOT_DIR)
ifeq ($(wildcard $(V8_DIR)),)
	cd $(LIB_DIR) && $(DEPOT_DIR)/fetch --no-history v8 
	cd $(V8_DIR) && git fetch origin $(V8_REF)
	cd $(V8_DIR) && git reset --hard $(V8_REF)
endif

GYPFLAGS ?=
GYPFLAGS += -Dlinux_use_bundled_gold=0
GYPFLAGS += -Dlinux_use_bundled_binutils=0
GYPFLAGS += -Dv8_use_external_startup_data=0

$(V8_LIB)/obj.target/src/libv8_base.a: $(V8_DIR)
	@cd $(V8_DIR) && ./tools/dev/v8gen.py $(BUILD_ARCH).release
	@cd $(V8_DIR) && make $(BUILD_ARCH).release -j4 werror=no GYPFLAGS="$(GYPFLAGS)"
	@touch $@
	# ninja -C $(V8_DIR)/out.gn/$(BUILD_ARCH).release

# g++ -I. hello_world.cpp -o hello_world -Wl,--start-group out/x64.release/obj.target/{tools/gyp/libv8_{base,libbase,external_snapshot,libplatform},third_party/icu/libicu{uc,i18n,data}}.a -Wl,--end-group -lrt -ldl -pthread -std=c++0x

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC) $(V8_LIB)/obj.target/src/libv8_base.a
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
		$(V8_LIB)/libv8_{base,libbase,snapshot,libplatform}.a \
		$(V8_LIB)/libicu{uc,i18n,data}.a \
		-lpthread
else
	g++ -I$(V8_DIR) -I$(V8_DIR)/include \
		$(TARGET_SRC) \
		-o $(TARGET_BIN) \
		-Wno-unused-variable \
		-Wl,--start-group \
		$(V8_LIB)/obj.target/{src/libv8_{base,libbase,external_snapshot,libplatform,libsampler},third_party/icu/libicu{uc,i18n}}.a \
		-Wl,--end-group \
		-lrt \
		-ldl \
		-pthread \
		-std=c++0x
endif
