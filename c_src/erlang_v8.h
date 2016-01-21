#ifndef ERLANG_V8_H
#define ERLANG_V8_H

#include "vm.h"

struct TimeoutHandlerArgs {
    v8::Platform* platform;
    v8::Isolate* isolate;
    VM& vm;
    long timeout;
};

v8::Handle<v8::Value> JSONStringify(v8::Isolate* isolate, v8::Handle<v8::Value> obj);
v8::Handle<v8::Value> WrapError(v8::Isolate* isolate, v8::Handle<v8::Value> value);

#endif
