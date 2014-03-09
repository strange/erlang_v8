ARCH := $(shell getconf LONG_BIT)
OS := $(shell uname)

BUILD_ARCH_32 := ia32
BUILD_ARCH_64 := x86
BUILD_ARCH := $(BUILD_ARCH_$(ARCH))

V8_REF := 49744859536225e7ac3b726e5b019dd99e127e6f
V8_DIR := lib/v8-$(V8_REF)
V8_LIB := $(V8_DIR)/out/$(BUILD_ARCH).release
V8_URL := https://github.com/v8/v8/archive/$(V8_REF).tar.gz

.PHONY: all v8 clean distclean test

all: v8 deps
	rebar compile

v8: priv/erlang_v8
	@:

clean:
	rm -rf priv/erlang_v8

distclean: clean
	rm -rf $(V8_DIR)

test:
	rebar eunit skip_deps=true

lib:
	mkdir -p lib

$(V8_DIR): lib
	curl -L $(V8_URL) | tar xvz -C lib

$(V8_DIR)/build/gyp: $(V8_DIR)
	cd $(V8_DIR) && make dependencies
	@touch $(V8_DIR)/build/gyp

$(V8_LIB)/libv8_base.$(BUILD_ARCH).a: $(V8_DIR)/build/gyp
	cd $(V8_DIR) && make $(BUILD_ARCH).release werror=no
	cp $(V8_LIB)/obj.target/tools/gyp/libv8_{base.$(BUILD_ARCH),snapshot}.a \
		$(V8_LIB) || :
	cp $(V8_LIB)/obj.target/third_party/icu/libicu{uc,i18n,data}.a \
		$(V8_LIB) || :

c_src/erlang_v8.cc: $(V8_LIB)/libv8_base.$(BUILD_ARCH).a
	@:

priv/erlang_v8: c_src/erlang_v8.cc
	mkdir -p priv
ifeq ($(OS),Darwin)
	# We need to link libstdc++ as XCode defaults to libc++. This assumes
	# latest OS X, XCode and default compiler (clang).
	g++ -Iinclude c_src/erlang_v8.cc \
		-stdlib=libstdc++ \
		-o priv/erlang_v8 \
		$(V8_LIB)/libv8_{base.$(BUILD_ARCH),snapshot}.a \
		$(V8_LIB)/libicu{uc,i18n,data}.a \
		-I $(V8_DIR)/include \
		-lpthread \
		-v
else
	g++ -Iinclude c_src/erlang_v8.cc \
		-o priv/erlang_v8 \
		-Wl,--start-group \
		$(V8_LIB)/libv8_{base.$(BUILD_ARCH),snapshot}.a \
		$(V8_LIB)/libicu{uc,i18n,data}.a \
		-Wl,--end-group \
		-I $(V8_DIR)/include \
		-lrt \
		-lpthread \
		-v
endif
