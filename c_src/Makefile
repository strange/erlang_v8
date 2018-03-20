ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x64
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

LIB_DIR := $(ROOT_DIR)/../build
PRIV_DIR := $(ROOT_DIR)/../priv

<<<<<<< HEAD
DEPOT_DIR := $(LIB_DIR)/depot_tools

V8_VERSION := "5.9.211.33"
V8_DIR := $(LIB_DIR)/v8
V8_LIB := $(V8_DIR)/out.gn/$(BUILD_ARCH).release/obj

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc report.cc vm.cc

PATH := $(PATH):$(DEPOT_DIR)
SHELL := /bin/bash
=======
DEPOT_REF := 85adaa343e5e9f499e59bd0cb98d02cc2fc9d2d5
DEPOT_DIR := $(LIB_DIR)/depot_tools

TARGET_BIN := $(PRIV_DIR)/erlang_v8
TARGET_SRC := erlang_v8.cc report.cc vm.cc

V8_REF := 7c79736019c9a0b9ef03f64ab26e46bb98692abf
V8_DIR := $(LIB_DIR)/v8
V8_LIB := $(V8_DIR)/out.gn/$(BUILD_ARCH).release
V8_LIB_RPATH := $(realpath --relative-to=$(TARGET_BIN) $(V8_LIB))

V8_LIB_RPATH := '$$ORIGIN'/.

export PATH := $(PATH):$(DEPOT_DIR)
>>>>>>> master

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
<<<<<<< HEAD
	cd $(LIB_DIR) && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git;
=======
	cd $(LIB_DIR) && git clone \
		https://chromium.googlesource.com/chromium/tools/depot_tools.git;
	cd $(DEPOT_DIR) && git reset --hard $(DEPOT_REF)
>>>>>>> master
endif

$(V8_DIR): $(LIB_DIR) $(DEPOT_DIR)
ifeq ($(wildcard $(V8_DIR)),)
<<<<<<< HEAD
	@echo $(LIB_DIR)
	@cd $(LIB_DIR) && $(DEPOT_DIR)/fetch v8
endif

$(V8_LIB)/libv8_base.a: $(V8_DIR)
	cd $(LIB_DIR) && gclient sync --revision $(V8_VERSION)
	cd $(V8_DIR) && tools/dev/v8gen.py -vv $(BUILD_ARCH).release
	cd $(V8_DIR) && gn gen "--args=is_component_build = false is_debug = false v8_static_library = true target_cpu = \"$(BUILD_ARCH)\"" out.gn/$(BUILD_ARCH).release
	cd $(V8_DIR) && $(DEPOT_DIR)/ninja -C out.gn/$(BUILD_ARCH).release
	@touch $@

$(PRIV_DIR)/natives_blob.bin: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../natives_blob.bin $(PRIV_DIR)

$(PRIV_DIR)/snapshot_blob.bin: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../snapshot_blob.bin $(PRIV_DIR)

$(PRIV_DIR)/icudtl.dat: $(V8_LIB)/libv8_base.a
	cp $(V8_LIB)/../icudtl.dat $(PRIV_DIR)

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC) $(V8_LIB)/libv8_base.a
=======
	cd $(LIB_DIR) && $(DEPOT_DIR)/fetch v8 
	cd $(V8_DIR) && git reset --hard $(V8_REF)
endif

$(PRIV_DIR)/libv8_libbase.so: $(V8_DIR)
	@cd $(V8_DIR) && ./tools/dev/v8gen.py $(BUILD_ARCH).release
	@cd $(V8_DIR) && gn gen out.gn/$(BUILD_ARCH).release --args='target_cpu="$(BUILD_ARCH)" v8_use_snapshot=true is_component_build=true is_debug=false v8_enable_inspector=false v8_enable_i18n_support=true v8_use_external_startup_data=false'
	@cd $(V8_DIR) && ninja -C out.gn/$(BUILD_ARCH).release d8
	@cp $(V8_LIB)/*.so $(PRIV_DIR)
	@touch $@

$(TARGET_BIN): $(PRIV_DIR) $(TARGET_SRC) $(PRIV_DIR)/libv8_libbase.so
>>>>>>> master
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
<<<<<<< HEAD
		-Wno-unused-variable \
		-Wl,--start-group \
		$(V8_LIB)/{libv8_{base,libbase,external_snapshot,libplatform,libsampler},third_party/icu/libicu{uc,i18n},src/inspector/libinspector}.a \
		-Wl,--end-group \
=======
		-Wl,$(PRIV_DIR)/libv8{,_libbase,_libplatform}.so \
		-Wl,$(PRIV_DIR)/libicu{uc,i18n}.so \
		-Wl,-rpath,$(V8_LIB_RPATH) \
>>>>>>> master
		-lrt \
		-ldl \
		-pthread \
		-std=c++11
endif
