#ifndef ERLANG_V8_H
#define ERLANG_V8_H

#include "vm.h"

v8::Local<v8::Value> JSONStringify(v8::Isolate* isolate, v8::Local<v8::Value> obj);
v8::Local<v8::Value> WrapError(v8::Isolate* isolate, v8::Local<v8::Value> value);

#endif
