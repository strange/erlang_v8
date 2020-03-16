ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LIB_DIR := $(ROOT_DIR)/../lib
PRIV_DIR := $(ROOT_DIR)/../priv

DEPOT_DIR := $(LIB_DIR)/depot_tools

V8_VERSION := "8.0.426.26"
V8_DIR := $(LIB_DIR)/v8
V8_LIB := $(V8_DIR)/out.gn/$(BUILD_ARCH).release/obj
CURRENT_VERSION := $(shell cd $(V8_DIR); git describe --tags)

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc report.cc vm.cc

PATH := $(PATH):$(DEPOT_DIR)
SHELL := /bin/bash

.PHONY: all v8 clean distclean local-clean local-distclean

all:: v8

clean:: local-clean

distclean:: local-distclean

local-clean:
	cd $(V8_DIR) && $(DEPOT_DIR)/ninja -C out.gn/$(BUILD_ARCH).release -t clean erlang_v8
	rm $(V8_LIB)/libv8_base.a
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
	@echo "Current $(CURRENT_VERSION) Need $(V8_VERSION)"
	cd $(V8_DIR) && git stash
	if [ $(CURRENT_VERSION) != $(V8_VERSION) ]; then \
	cd $(LIB_DIR) && gclient sync --revision $(V8_VERSION); \
	fi
	cd $(V8_DIR) && git stash pop || true
	if [ ! -f $(V8_DIR)/.gn_patched ]; then \
			cd $(V8_DIR) && patch -p1 -i ../../gn.patch && touch .gn_patched; \
  fi
	cd $(V8_DIR) && tools/dev/v8gen.py -vv $(BUILD_ARCH).release
	cd $(V8_DIR) && gn gen "--args=is_component_build = false is_debug = false v8_static_library = true target_cpu = \"$(BUILD_ARCH)\"" out.gn/$(BUILD_ARCH).release
	cd $(V8_DIR) && $(DEPOT_DIR)/ninja -C out.gn/$(BUILD_ARCH).release erlang_v8

$(PRIV_DIR)/natives_blob.bin: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../natives_blob.bin $(PRIV_DIR)

$(PRIV_DIR)/snapshot_blob.bin: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../snapshot_blob.bin $(PRIV_DIR)

$(PRIV_DIR)/icudtl.dat: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../icudtl.dat $(PRIV_DIR)

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC) $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../erlang_v8 $(PRIV_DIR)
