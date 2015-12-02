#include <iostream>
#include <sstream>
#include <fstream>
#include <assert.h>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"

#define DEBUG 0
#define TRACE(fmt, ...) \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, \
            __LINE__, __func__, __VA_ARGS__); } while (0)

using namespace v8;
using namespace std;

const uint8_t OP_OK = 0;
const uint8_t OP_ERROR = 1;

const uint8_t OP_EVAL = 1;
const uint8_t OP_CALL = 2;
const uint8_t OP_RESET_VM = 3;

struct Packet {
    uint8_t op;
    string data;
};

Handle<Value> JSONStringify(Isolate* isolate, Handle<Value> obj);
Handle<Value> WrapError(Isolate* isolate, Handle<Value> value);

const char* ToCString(const v8::String::Utf8Value& value) {
  return *value ? *value : "<string conversion failed>";
}

void Report(Isolate* isolate, Handle<Value> response, uint8_t op) {
    Handle<Value> input;

    if (response->IsUndefined()) {
        input = String::NewFromUtf8(isolate, "undefined");
    } else {
        input = JSONStringify(isolate, response);
    }

    String::Utf8Value utf8 (input);
    uint32_t len = utf8.length() + 1;

    cout << (uint8_t)((len >> 24) & 0xff);
    cout << (uint8_t)((len >> 16) & 0xff);
    cout << (uint8_t)((len >> 8) & 0xff);
    cout << (uint8_t)(len & 0xff);

    cout << op;

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
    HandleScope handle_scope(isolate);

    Handle<Value> stack_trace = try_catch->StackTrace();

    if (stack_trace.IsEmpty()) {
        ReportError(isolate, try_catch->Exception());
    } else {
        const char* st = ToCString(String::Utf8Value(try_catch->StackTrace()));
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

size_t packet_length() {
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

bool next_packet(Packet* packet) {
    size_t len = packet_length();

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

    packet->op = op;
    packet->data = buf;

    return true;
}

void Eval(Isolate* isolate, string input) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    Handle<String> source = String::NewFromUtf8(isolate, input.c_str());
    Handle<Script> script = Script::Compile(source);

    if (script.IsEmpty()) {
        assert(try_catch.HasCaught());
        ReportException(isolate, &try_catch);
    } else {
        Handle<Value> result = script->Run();

        if (result.IsEmpty()) {
            assert(try_catch.HasCaught());
            ReportException(isolate, &try_catch);
        } else {
            ReportOK(isolate, result);
        }
    }
}

void Call(Isolate* isolate, string input) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    Handle<String> json_data = String::NewFromUtf8(isolate, input.c_str());
    Local<Object> instructions = JSON::Parse(json_data)->ToObject();

    Handle<Context> context = isolate->GetCurrentContext();
    Handle<Object> global = context->Global();

    Local<String> function_key = String::NewFromUtf8(isolate, "function");
    Local<String> function_name = instructions->Get(function_key)->ToString();

    Local<String> args_key = String::NewFromUtf8(isolate, "args");
    Local<Value> args_value = instructions->Get(args_key);
    Local<Array> raw_args = Local<Array>::Cast(args_value);

    int len = raw_args->Length();
    Handle<Value> *args = new Handle<Value>[len];

    // debug(static_cast<ostringstream*>( &(ostringstream() << len) )->str());
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
    Isolate* isolate = Isolate::New();

    HandleScope handle_scope(isolate);

    Handle<Context> context = Context::New(isolate, NULL);
    Context::Scope context_scope(context);
    
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
    while (!reset && next_packet(&packet)) {
        switch(packet.op) {
            case OP_EVAL:
                Eval(isolate, packet.data);
                break;
            case OP_CALL:
                Call(isolate, packet.data);
                break;
            case OP_RESET_VM:
                reset = true;
                break;
        }
        packet = (const struct Packet){ 0 };
    }
    Isolate::GetCurrent()->ContextDisposedNotification(); 
    return reset;
}

Handle<Context> CreateContext(Isolate* isolate) {
    v8::Handle<v8::ObjectTemplate> g = v8::ObjectTemplate::New(isolate);
    return v8::Context::New(isolate, NULL, g);
}

int main(int argc, char* argv[]) {
    ios_base::sync_with_stdio(false);

    V8::InitializeICU();
    Platform* platform = v8::platform::CreateDefaultPlatform();
    V8::InitializePlatform(platform);
    V8::Initialize();
    V8::SetFlagsFromCommandLine(&argc, argv, true);
    Isolate* isolate = v8::Isolate::New();

    {
        v8::Isolate::Scope isolate_scope(isolate);
        v8::HandleScope handle_scope(isolate);
        //
        Handle<Context> context = CreateContext(isolate);
        //
        Context::Scope context_scope(context);

        while (CommandLoop(argc, argv));
    }

    return 0;
}
