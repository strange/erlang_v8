#include <assert.h>
#include <string.h>
#include <iostream>

#include "libplatform/libplatform.h"
#include "v8.h"

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

    for (size_t bytes_read = 0; bytes_read < len;) {
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
        packet.ref = 0;
        //packet = (const Packet){ 0 };
    }
    Isolate::GetCurrent()->ContextDisposedNotification(); 
    return reset;
}

/*
 * The implementations of the run_extra_code(), create_snapshot_data_blob() and
 * warm_up_snapshot_data_blob() functions have been derived from V8's test suite.
 */
// see also https://github.com/rubyjs/mini_racer/commit/a22348f73ee32b7860d7ba5b26bfed6c80a52e56
bool run_extra_code(Isolate *isolate, Local<v8::Context> context,
                    const char *utf8_source, const char *name) {
    Context::Scope context_scope(context);
    TryCatch try_catch(isolate);
    Local<String> source_string;
    if (!String::NewFromUtf8(isolate, utf8_source,
                             NewStringType::kNormal)
             .ToLocal(&source_string)) {
        return false;
    }
    Local<v8::String> resource_name =
        String::NewFromUtf8(isolate, name, NewStringType::kNormal)
            .ToLocalChecked();
    ScriptOrigin origin(resource_name);
    ScriptCompiler::Source source(source_string, origin);
    Local<Script> script;
    if (!ScriptCompiler::Compile(context, &source).ToLocal(&script)) {
        TRACE("snapshot compilation failed\n");
        return false;
    }
    if (script->Run(context).IsEmpty()) {
        TRACE("snapshot result empty\n");
        return false;
    }
    // CHECK(!try_catch.HasCaught());
    return true;
}

StartupData
create_snapshot_data_blob(const char *embedded_source = nullptr) {
    // Create a new isolate and a new context from scratch, optionally run
    // a script to embed, and serialize to create a snapshot blob.
    StartupData result = {nullptr, 0};
    {
        SnapshotCreator snapshot_creator;
        Isolate *isolate = snapshot_creator.GetIsolate();
        {
            HandleScope scope(isolate);
            Local<Context> context = Context::New(isolate);
            if (embedded_source != nullptr &&
                !run_extra_code(isolate, context, embedded_source,
                                "<embedded>")) {
                TRACE("run_extra code false\n");
                return result;
            }
            TRACE("path \n");
            snapshot_creator.SetDefaultContext(context);
        }
        result = snapshot_creator.CreateBlob(
            SnapshotCreator::FunctionCodeHandling::kClear);
    }
    return result;
}

int main(int argc, char* argv[]) {
    ios_base::sync_with_stdio(false);

    V8::InitializeICU();
    V8::InitializeICUDefaultLocation(argv[0]);
    v8::V8::InitializeExternalStartupData(argv[0]);

    std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
    V8::InitializePlatform(platform.get());
    V8::Initialize();

    V8::SetFlagsFromCommandLine(&argc, argv, true);

    Isolate::CreateParams params;

    char* source;
    if (argc == 2 && strlen(argv[1]) > 0) {
        // this seems to cause debug builds to be angry
        source = argv[1];
        FTRACE("got snapshot \"%s\"\n", source);
        StartupData snapshot = create_snapshot_data_blob(source);
        params.snapshot_blob = &snapshot;
    } else {
        source = NULL;
    }

    params.array_buffer_allocator =
        v8::ArrayBuffer::Allocator::NewDefaultAllocator();

    Isolate* isolate = Isolate::New(params);

    VM vm(platform.get(), isolate);
    FTRACE("Initial VM: %p\n", &vm);
    {
        Isolate::Scope isolate_scope(isolate);

        while (CommandLoop(vm));
    }

    isolate->Dispose();
    V8::Dispose();
    V8::ShutdownPlatform();
    //delete platform;

    return 0;
}
