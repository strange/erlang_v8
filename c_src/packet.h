#ifndef ERLANG_V8_PACKET_H
#define ERLANG_V8_PACKET_H

const uint8_t OP_OK = 0;
const uint8_t OP_ERROR = 1;
const uint8_t OP_TIMEOUT = 2;
const uint8_t OP_INVALID_CONTEXT = 3;

const uint8_t OP_EVAL = 1;
const uint8_t OP_CALL = 2;
const uint8_t OP_CREATE_CONTEXT = 3;
const uint8_t OP_DESTROY_CONTEXT = 4;
const uint8_t OP_RESET_VM = 5;
const uint8_t OP_COMPILE_MODULE = 6;

struct Packet {
    uint8_t op;
    uint32_t ref;
    std::string data;
};

#endif
