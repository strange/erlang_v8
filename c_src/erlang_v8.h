#ifndef ERLANG_V8_H
#define ERLANG_V8_H

#include "vm.h"

const uint8_t OP_OK = 0;
const uint8_t OP_ERROR = 1;
const uint8_t OP_TIMEOUT = 2;

const uint8_t OP_EVAL = 1;
const uint8_t OP_CALL = 2;
const uint8_t OP_CREATE_CONTEXT = 3;
const uint8_t OP_DESTROY_CONTEXT = 4;
const uint8_t OP_RESET_VM = 5;

struct Packet {
    uint8_t op;
    uint32_t ref;
    std::string data;
};

struct TimeoutHandlerArgs {
    v8::Platform* platform;
    v8::Isolate* isolate;
    VM& vm;
    long timeout;
};

v8::Handle<v8::Value> JSONStringify(v8::Isolate* isolate, v8::Handle<v8::Value> obj);
v8::Handle<v8::Value> WrapError(v8::Isolate* isolate, v8::Handle<v8::Value> value);

#endif
