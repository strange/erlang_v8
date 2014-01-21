#!/bin/bash
ROOT=$(pwd)

LIB_DIR=$ROOT/libs
V8_DIR=$ROOT/libs/v8
DIST_DIR=$ROOT/libs/dist
SOURCE_DIR=$ROOT/c_src

ARCH=`getconf LONG_BIT`

if [ "$ARCH" == "32" ]; then
    BUILD_ARCH="ia${ARCH}"
else
    BUILD_ARCH="x${ARCH}"
fi

if [ "$(uname)" == "Darwin" ]; then
    V8_FLAGS="werror=no"
else
    V8_FLAGS=""
fi

V8_SHA="49744859536225e7ac3b726e5b019dd99e127e6f"

checkout() {
    mkdir -p "$LIB_DIR"
    if [ -d "$V8_DIR" ]; then
        cd $V8_DIR
        git pull origin master
        git reset --hard $V8_SHA
    else
        cd $LIB_DIR
        git clone git://github.com/v8/v8.git v8
        git reset --hard $V8_SHA
    fi
}

v8_deps() {
	cd $V8_DIR
    make dependencies 
}

build_v8() {
    pyver=`python -c 'import sys; print(sys.version_info[0])'`
    if [ "$pyver" != "2" ]; then
        echo "Python 2.6 or 2.7 is required by GYP."
        exit 1;
    fi
	cd $V8_DIR
    make $BUILD_ARCH.release $V8_FLAGS
}

build_dist() {
    mkdir -p $DIST_DIR

    RELEASE_DIR=$V8_DIR/out/$BUILD_ARCH.release
    if [ "$(uname)" == "Darwin" ]; then
        # OS X uses clang, and the static archives are located directly in the
        # release directory. We also need to link libstdc++ as XCode defaults
        # to libc++. This assumes latest OS X, XCode and default compiler
        # (clang).
        g++ -Iinclude $SOURCE_DIR/erlang_v8.cc \
            -stdlib=libstdc++ \
            -o $DIST_DIR/erlang_v8 \
            $RELEASE_DIR/libv8_{base.$BUILD_ARCH,snapshot}.a \
            $RELEASE_DIR/libicu{uc,i18n,data}.a \
            -I $V8_DIR/include \
            -lpthread \
            -v
    else
        g++ -Iinclude $SOURCE_DIR/erlang_v8.cc \
            -o $DIST_DIR/erlang_v8 \
            -Wl,--start-group \
            $RELEASE_DIR/obj.target/{tools/gyp/libv8_{base.$BUILD_ARCH,snapshot},third_party/icu/libicu{uc,i18n,data}}.a \
            -Wl,--end-group \
            -I $V8_DIR/include \
            -lpthread \
            -v
    fi
}

clean() {
    rm -rf $DIST_DIR
    cd $V8_DIR
    make clean
}

distclean() {
    rm -rf $LIB_DIR
}

case "$1" in
    checkout)
        checkout
        v8_deps
        ;;

    update)
        checkout
        ;;

    build)
        build_v8
        build_dist
        ;;

    build-dist)
        build_dist
        ;;

    clean)
        clean
        ;;

    distclean)
        distclean
        ;;
esac
