#ifndef ERLANG_V8_VM_H
#define ERLANG_V8_VM_H

#include <map>
#include <vector>

#include "packet.h"

class VM {
    private:
        v8::Isolate* isolate;
        v8::Platform* platform;

        std::map<uint32_t, v8::Persistent<v8::Context, v8::CopyablePersistentTraits<v8::Context>>> contexts;

    public:
        VM(v8::Platform* platform_, v8::Isolate* isolate_);

        v8::Isolate* GetIsolate();

        void Eval(Packet *packet);
        void Call(Packet *packet);

        void PumpMessageLoop();
        void TerminateExecution();

        bool CreateContext(uint32_t ref);
        bool DestroyContext(uint32_t ref);

        int Size();
};

#endif
