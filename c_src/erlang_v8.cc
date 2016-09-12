#include <assert.h>
#include <string.h>
#include <iostream>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"

#include "vm.h"
#include "erlang_v8.h"
#include "debug.h"
#include "packet.h"

using namespace v8;
using namespace std;

class ArrayBufferAllocator : public v8::ArrayBuffer::Allocator {
    public:
        virtual void* Allocate(size_t length) {
            void* data = AllocateUninitialized(length);
            return data == NULL ? data : memset(data, 0, length);
        }
        virtual void* AllocateUninitialized(size_t length) {
            return malloc(length);
        }
        virtual void Free(void* data, size_t) {
            free(data);
        }
};

size_t PacketLength() {
    char bytes[4];
    size_t len;

    if (!cin.get(bytes[0]) || !cin.get(bytes[1]) ||
            !cin.get(bytes[2]) || !cin.get(bytes[3])) {
        return 0;
    }

    len = (((uint8_t)bytes[0] << 24) | ((uint8_t)bytes[1] << 16) |
            ((uint8_t)bytes[2] << 8) | (uint8_t)bytes[3]);

    return len;
}

bool NextPacket(Packet* packet) {
    size_t len = PacketLength();
    uint32_t ref = 0;

    if (len == 0) {
        return false;
    }

    string buf;
    buf.resize(len);

    for (int bytes_read = 0; bytes_read < len;) {                                                  
        if (!cin.read(&buf[bytes_read], len - bytes_read)) {
            return false;
        }
        bytes_read += cin.gcount();                                                                  
    }

    // extract the one-byte op code from the message and erase it from the
    // buffer.
    uint8_t op = buf.at(0);
    buf.erase(0, 1);

    ref = (((uint8_t)buf[0] << 24) | ((uint8_t)buf[1] << 16) |
            ((uint8_t)buf[2] << 8) | (uint8_t)buf[3]);

    buf.erase(0, 4);

    packet->op = op;
    packet->ref = ref;
    packet->data = buf;

    return true;
}

bool CommandLoop(VM& vm) {
    HandleScope handle_scope(vm.GetIsolate());

    bool reset = false;
    Packet packet;

    while (!reset && NextPacket(&packet)) {
        TRACE("In command loop!\n");
        vm.Size();

        switch(packet.op) {
            case OP_EVAL:
                FTRACE("Eval in context: %i\n", packet.ref);
                vm.Eval(&packet);
                break;
            case OP_CALL:
                FTRACE("Call in context: %i\n", packet.ref);
                vm.Call(&packet);
                break;
            case OP_CREATE_CONTEXT:
                FTRACE("Creating context: %i\n", packet.ref);
                vm.CreateContext(packet.ref);
                break;
            case OP_DESTROY_CONTEXT:
                FTRACE("Destroying context: %i\n", packet.ref);
                vm.DestroyContext(packet.ref);
                break;
            case OP_RESET_VM:
                FTRACE("Ignoring reset: %i\n", packet.ref);
                // reset = true;
                break;
        }
        vm.PumpMessageLoop();
        packet = (const Packet){ 0 };
    }
    Isolate::GetCurrent()->ContextDisposedNotification(); 
    return reset;
}

int main(int argc, char* argv[]) {
    ios_base::sync_with_stdio(false);

    V8::InitializeICU();

    Platform* platform = v8::platform::CreateDefaultPlatform();

    V8::InitializeICUDefaultLocation(argv[0]);                                    
    V8::InitializeExternalStartupData(argv[0]); 

    V8::InitializePlatform(platform);
    V8::Initialize();

    V8::SetFlagsFromCommandLine(&argc, argv, true);

    Isolate::CreateParams params;

    char* source;
    if (argc == 2) {
        source = argv[1];
    } else {
        source = NULL;
    }

    StartupData snapshot = V8::CreateSnapshotDataBlob(source);

    ArrayBufferAllocator allocator;
    params.snapshot_blob = &snapshot;
    params.array_buffer_allocator = &allocator;

    Isolate* isolate = Isolate::New(params);

    VM vm(platform, isolate);
    FTRACE("Initial VM: %p\n", &vm);
    {
        Isolate::Scope isolate_scope(isolate);

        while (CommandLoop(vm));
    }

    isolate->Dispose();
    V8::Dispose();
    V8::ShutdownPlatform();
    delete platform;

    return 0;
}
