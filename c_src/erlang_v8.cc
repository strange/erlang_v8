#include <iostream>
#include <sstream>
#include <fstream>
#include <assert.h>
#include <map>
#include <thread>
#include <unistd.h>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"

#define DEBUG 1
#define TRACE(fmt, ...) \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, \
            __LINE__, __func__, __VA_ARGS__); } while (0)

using namespace v8;
using namespace std;

const uint8_t OP_OK = 0;
const uint8_t OP_ERROR = 1;
const uint8_t OP_TIMEOUT = 2;

const uint8_t OP_EVAL = 1;
const uint8_t OP_CALL = 2;
const uint8_t OP_DESTROY_CONTEXT = 3;
const uint8_t OP_CREATE_CONTEXT = 4;

struct Packet {
    uint8_t op;
    uint32_t ref;
    string data;
};

struct TimeoutHandlerArgs {
    v8::Isolate *isolate;
    long timeout;
};

Handle<Value> JSONStringify(Isolate* isolate, Handle<Value> obj);
Handle<Value> WrapError(Isolate* isolate, Handle<Value> value);

const char* ToCString(const v8::String::Utf8Value& value) {
  return *value ? *value : "<string conversion failed>";
}

void Report(Isolate* isolate, Handle<Value> response, uint8_t op) {
    uint32_t ref = 0;

    Handle<Value> input;

    if (response->IsUndefined()) {
        input = String::NewFromUtf8(isolate, "undefined");
    } else {
        input = JSONStringify(isolate, response);
    }

    String::Utf8Value utf8 (input);
    uint32_t len = utf8.length() + 1 + 4;

    cout << (uint8_t)((len >> 24) & 0xff);
    cout << (uint8_t)((len >> 16) & 0xff);
    cout << (uint8_t)((len >> 8) & 0xff);
    cout << (uint8_t)(len & 0xff);

    cout << op;

    cout << (uint8_t)((ref >> 24) & 0xff);
    cout << (uint8_t)((ref >> 16) & 0xff);
    cout << (uint8_t)((ref >> 8) & 0xff);
    cout << (uint8_t)(ref & 0xff);

    cout << *utf8;
    cout.flush();
}

void ReportOK(Isolate* isolate, Handle<Value> response) {
    Report(isolate, response, OP_OK);
}

void ReportError(Isolate* isolate, Handle<Value> response) {
    Report(isolate, WrapError(isolate, response), OP_ERROR);
}

void ReportException(Isolate* isolate, TryCatch* try_catch) {
    TRACE("Here: %s\n", "1");
    HandleScope handle_scope(isolate);
    TRACE("Here: %s\n", "2");

    Handle<Value> stack_trace = try_catch->StackTrace();
    TRACE("Here: %s\n", "3");

    if (stack_trace.IsEmpty()) {
    TRACE("Here: %s\n", "4");
        ReportError(isolate, try_catch->Exception());
    TRACE("Here: %s\n", "5");
    } else {
    TRACE("Here: %s\n", "6");
        const char* st = ToCString(String::Utf8Value(try_catch->StackTrace()));
    TRACE("Here: %s\n", "7");
        TRACE("Stack: %s\n", st);
        ReportError(isolate, try_catch->StackTrace());
    }
}

Handle<Value> WrapError(Isolate* isolate, Handle<Value> exception) {
    EscapableHandleScope handle_scope(isolate);

    Local<Object> obj = Object::New(isolate);

    String::Utf8Value exception_string(exception);
    std::string from = std::string(*exception_string);

    obj->Set(String::NewFromUtf8(isolate, "error"),
             exception->ToString());

    // add line number and other fancy details ...

    return handle_scope.Escape(obj);
}

Handle<Value> JSONStringify(Isolate* isolate, Handle<Value> obj) {
    Handle<Context> context = isolate->GetCurrentContext();
    Handle<Object> global = context->Global();
    EscapableHandleScope handle_scope(isolate);

    Handle<Value> JSONValue = global->Get(String::NewFromUtf8(isolate, "JSON"));
    Handle<Object> JSON = JSONValue->ToObject();
    Handle<Function> stringify = Handle<Function>::Cast(
            JSON->Get(String::NewFromUtf8(isolate, "stringify")));

    Handle<Value> args[] = { obj };
    Local<Value> result = stringify->Call(JSON, 1, args);

    return handle_scope.Escape(result);
}

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

Handle<Context> CreateContext(Isolate* isolate) {
    v8::Handle<v8::ObjectTemplate> g = v8::ObjectTemplate::New(isolate);
    return v8::Context::New(isolate, NULL, g);
}

void* TimeoutHandler(void *arg) {
    TRACE("Thread started: %i\n", 10);
    struct TimeoutHandlerArgs *args = (struct TimeoutHandlerArgs*)arg;
    usleep(1000000);
    TRACE("After sleep: %i\n", 10);
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, 0x00);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, 0x00);
    V8::TerminateExecution(args->isolate);
    TRACE("Isolate terminated: %i\n", 10);
    return NULL;
}


void Eval(Isolate* isolate, std::map<uint32_t,Handle<Context>> &contexts,
        Packet* packet) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    string input = packet->data;

    Handle<Context> context = contexts[packet->ref];
    Context::Scope context_scope(context);

    Handle<String> json_data = String::NewFromUtf8(isolate, input.c_str());
    Local<Object> instructions = JSON::Parse(json_data)->ToObject();

    Local<String> source_key = String::NewFromUtf8(isolate, "source");
    Local<String> source = instructions->Get(source_key)->ToString();

    Handle<Script> script = Script::Compile(source);

    if (script.IsEmpty()) {
        assert(try_catch.HasCaught());
        ReportException(isolate, &try_catch);
    } else {
        TRACE("Thread starting: %i\n", 10);

        pthread_t t;
        struct TimeoutHandlerArgs args;
        void *res;

        args.isolate = isolate;
        args.timeout = (long)1;

        pthread_create(&t, NULL, TimeoutHandler, &args);

        Handle<Value> result = script->Run();
        std::cerr << "IsExecutionTerminating " << V8::IsExecutionTerminating() << std::endl;
        std::cerr << "TryCatch.Exception.IsEmpty() " << try_catch.Exception().IsEmpty() << std::endl;
        std::cerr << "TryCatch.Message.IsEmpty() " << try_catch.Message().IsEmpty() << std::endl;
        std::cerr << "TryCatch.StackTrace.IsEmpty() " << try_catch.StackTrace().IsEmpty() << std::endl;

        pthread_cancel(t);
        pthread_join(t, &res);

        std::cerr << "join" << res << std::endl;

        // th.terminate();
        // th.join();

        TRACE("WERE OUT!%i\n", 10);

        if (result.IsEmpty()) {
            assert(try_catch.HasCaught());
            if (try_catch.Message().IsEmpty() && try_catch.StackTrace().IsEmpty()) {
                TRACE("It's a timeout! 1%i\n", 10);
                Handle<String> tt = String::NewFromUtf8(isolate, "timeout");
                contexts.erase(packet->ref);
                Report(isolate, tt, OP_TIMEOUT);
            } else {
                TRACE("It's a regular error. 1%i\n", 10);
                ReportException(isolate, &try_catch);
            }
        } else {
            TRACE("ALL IS WELL%i\n", 10);
            ReportOK(isolate, result);
        }
    }
}

void Call(Isolate* isolate, std::map<uint32_t,Handle<Context>> &contexts,
        Packet* packet) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    string input = packet->data;

    Handle<Context> context = contexts[packet->ref];
    Context::Scope context_scope(context);

    Handle<String> json_data = String::NewFromUtf8(isolate, input.c_str());
    Local<Object> instructions = JSON::Parse(json_data)->ToObject();

    // Handle<Context> context = isolate->GetCurrentContext();
    Handle<Object> global = context->Global();

    Local<String> function_key = String::NewFromUtf8(isolate, "function");
    Local<String> function_name = instructions->Get(function_key)->ToString();

    Local<String> args_key = String::NewFromUtf8(isolate, "args");
    Local<Value> args_value = instructions->Get(args_key);
    Local<Array> raw_args = Local<Array>::Cast(args_value);

    int len = raw_args->Length();
    Handle<Value> *args = new Handle<Value>[len];

    for (int i = 0; i < len; i++) { 
        args[i] = raw_args->Get(i);
    }

    // we cannot simply retrieve the function from the global scope as the
    // name can be something like `lol.flop['hi']`. wrapping the call in a
    // temporary function is much simpler than attempting to split the name
    // and check all the individual parts.
    Handle<String> prefix = String::NewFromUtf8(isolate,
            "function __call() { return ");
    Handle<String> suffix = String::NewFromUtf8(isolate,
            ".apply(null, arguments); }");
    Handle<String> source = String::Concat(String::Concat(prefix,
                function_name), suffix);
    Handle<Script> script = Script::Compile(source);
    Handle<Value> eval_result = script->Run();

    if (eval_result.IsEmpty()) {
        assert(try_catch.HasCaught());
        ReportException(isolate, &try_catch);
    } else {
        Local<String> fn = String::NewFromUtf8(isolate, "__call");
        Handle<Function> function = Handle<Function>::Cast(global->Get(fn));
        Handle<Value> result = function->Call(global, len, args);

        if (result.IsEmpty()) {
            assert(try_catch.HasCaught());
            ReportException(isolate, &try_catch);
        } else {
            ReportOK(isolate, result);
        }
    }
}

bool CommandLoop(int scriptc, char* scriptv[]) {
    std::map<uint32_t,Handle<Context>> contexts;

    Isolate* isolate = Isolate::New();
    HandleScope handle_scope(isolate);

    // Initializing scripts for every new (reset) request. This is a temporary
    // solution.
    for (int i = 1; i < scriptc; i++) {
        // we need to catch errors here and inform the user.
        Handle<String> source = String::NewFromUtf8(isolate, scriptv[i]);
        Handle<Script> script = Script::Compile(source);
        Handle<Value> result = script->Run();
    }

    bool reset = false;
    Packet packet;
    while (!reset && NextPacket(&packet)) {
        switch(packet.op) {
            case OP_EVAL:
                TRACE("Eval in context: %i\n", packet.ref);
                Eval(isolate, contexts, &packet);
                break;
            case OP_CALL:
                TRACE("Call in context: %i\n", packet.ref);
                fflush(stderr);
                Call(isolate, contexts, &packet);
                break;
            case OP_CREATE_CONTEXT:
                TRACE("Creating context: %i\n", packet.ref);
                contexts[packet.ref] = CreateContext(isolate);
                break;
            case OP_DESTROY_CONTEXT:
                TRACE("Destroying context: %i\n", packet.ref);
                contexts.erase(packet.ref);
                // reset = true;
                break;
        }
        packet = (const Packet){ 0 };
    }
    Isolate::GetCurrent()->ContextDisposedNotification(); 
    return reset;
}

int main(int argc, char* argv[]) {
    ios_base::sync_with_stdio(false);

    V8::InitializeICU();
    Platform* platform = v8::platform::CreateDefaultPlatform();
    V8::InitializePlatform(platform);
    V8::Initialize();
    V8::SetFlagsFromCommandLine(&argc, argv, true);

    Isolate* isolate = Isolate::New();
    {
        v8::Isolate::Scope isolate_scope(isolate);
        v8::HandleScope handle_scope(isolate);
        while (CommandLoop(argc, argv));
    }

    return 0;
}
