ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LIB_DIR := $(ROOT_DIR)/../lib
PRIV_DIR := $(ROOT_DIR)/../priv

DEPOT_DIR := $(LIB_DIR)/depot_tools

V8_VERSION := "7.9.317.25"
V8_DIR := $(LIB_DIR)/v8
V8_LIB := $(V8_DIR)/out.gn/$(BUILD_ARCH).release/obj

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc report.cc vm.cc

PATH := $(PATH):$(DEPOT_DIR)
SHELL := /bin/bash

.PHONY: all v8 clean distclean local-clean local-distclean

all:: v8

clean:: local-clean

distclean:: local-distclean

local-clean:
	rm -rf $(TARGET_BIN)

local-distclean: local-clean
	rm -rf $(LIB_DIR)

v8: $(TARGET_BIN) $(PRIV_DIR)/natives_blob.bin $(PRIV_DIR)/snapshot_blob.bin $(PRIV_DIR)/icudtl.dat

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(DEPOT_DIR): $(LIB_DIR)
ifeq ($(wildcard $(DEPOT_DIR)),)
	cd $(LIB_DIR) && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git;
endif

$(V8_DIR): $(LIB_DIR) $(DEPOT_DIR)
ifeq ($(wildcard $(V8_DIR)),)
	@echo $(LIB_DIR)
	@cd $(LIB_DIR) && $(DEPOT_DIR)/fetch v8
endif

$(V8_LIB)/libv8_base.a: $(V8_DIR)
	@echo "ROOT dir $(ROOT_DIR)"
	cd $(LIB_DIR) && gclient sync --revision $(V8_VERSION)
	cd $(V8_DIR) && tools/dev/v8gen.py -vv $(BUILD_ARCH).release
	cd $(V8_DIR) && gn gen "--args=is_component_build = false is_debug = false use_custom_libcxx = false use_sysroot = false use_gold = false v8_static_library = true target_cpu = \"$(BUILD_ARCH)\"" out.gn/$(BUILD_ARCH).release
	cd $(V8_DIR) && $(DEPOT_DIR)/ninja -C out.gn/$(BUILD_ARCH).release
	#@touch $@

$(PRIV_DIR)/natives_blob.bin: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../natives_blob.bin $(PRIV_DIR)

$(PRIV_DIR)/snapshot_blob.bin: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../snapshot_blob.bin $(PRIV_DIR)

$(PRIV_DIR)/icudtl.dat: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../icudtl.dat $(PRIV_DIR)

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC) $(V8_LIB)/libv8_base.a
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
		$(V8_LIB)/libv8_{base,libbase,external_snapshot,libplatform,libsampler}.a \
		$(V8_LIB)/libicu{uc,i18n}.a \
		-lpthread
else
	g++ \
		-I$(V8_DIR)/include \
		$(TARGET_SRC) \
		-o $(TARGET_BIN) \
		-Wno-unused-variable \
		-Wl,--start-group \
		$(V8_LIB)/{libv8_{base,libbase,external_snapshot,libplatform,libsampler,init,initializers,compiler,compiler_opt,base_without_compiler},libtorque*,libwasm*,libwee8,third_party/icu/libicu{uc,i18n},src/inspector/libinspector}.a \
		$(V8_LIB)/buildtools/third_party/libc++/libc++/*.o \
		$(V8_LIB)/buildtools/third_party/libc++abi/libc++abi/*.o \
		-Wl,--end-group \
		-lrt \
		-ldl \
		-pthread \
		-fpermissive \
		-std=c++11
endif
