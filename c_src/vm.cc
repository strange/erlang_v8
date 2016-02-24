#include <assert.h>
#include <map>
#include <vector>
#include <string>
#include <pthread.h>
#include <unistd.h>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"

#include "report.h"
#include "debug.h"
#include "packet.h"
#include "vm.h"

using namespace v8;
using namespace std;


struct TimeoutHandlerArgs {
    VM* vm;
    long timeout;
};


void* TimeoutHandler(void *arg) {
    struct TimeoutHandlerArgs *args = (struct TimeoutHandlerArgs*)arg;
    TRACE("Timeout started.\n");
    usleep(1000000);
    TRACE("After sleep,\n");

    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, 0x00);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, 0x00);

    args->vm->TerminateExecution();
    args->vm->PumpMessageLoop();

    return NULL;
}

VM::VM(Platform* platform_, Isolate* isolate_, int scriptc, char* scriptv[]) {
    isolate = isolate_;
    platform = platform_;

    scripts = vector<string>(scriptv + 1, scriptv + scriptc);
}

bool VM::CreateContext(uint32_t ref) {
    HandleScope handle_scope(isolate);

    Local<v8::ObjectTemplate> global = ObjectTemplate::New(isolate);
    Local<Context> context = Context::New(isolate, NULL, global);

    Persistent<Context, CopyablePersistentTraits<Context>> pcontext(isolate, context);

    Context::Scope context_scope(context);

    // Initializing scripts for every new context. This is a
    // temporary solution.
    for (auto script : scripts) {
        Local<String> source = String::NewFromUtf8(isolate,
                script.c_str());
        Local<Script> compiled = Script::Compile(source);
        Local<Value> result = compiled->Run();
    }

    contexts[ref] = pcontext;
    return true;
}

bool VM::DestroyContext(uint32_t ref) {
    Persistent<Context> pcontext (isolate, contexts[ref]);
    if (pcontext.IsEmpty()) {
        Local<String> tt = String::NewFromUtf8(isolate, "empty contextz");
        Report(isolate, tt, OP_INVALID_CONTEXT);
        return false;
    } else {
        pcontext.Reset();
        contexts.erase(ref);
        return true;
    }
}

Isolate* VM::GetIsolate() {
    return isolate;
}

void VM::PumpMessageLoop() {
    while (v8::platform::PumpMessageLoop(platform, isolate)) continue;
}

void VM::TerminateExecution() {
    V8::TerminateExecution(isolate);
    FTRACE("Isolate terminated: %i\n", 10);
}

int VM::Size() {
    FTRACE("Context size: %zd\n", contexts.size());
    return contexts.size();
}

void VM::Eval(Packet* packet) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    Local<Context> context = Local<Context>::New(isolate,
            contexts[packet->ref]);

    if (context.IsEmpty()) {
        Local<String> tt = String::NewFromUtf8(isolate, "empty contextz");
        Report(isolate, tt, OP_INVALID_CONTEXT);
    } else {
        Context::Scope context_scope(context);

        string input = packet->data;

        Local<String> json_data = String::NewFromUtf8(isolate, input.c_str());
        Local<Object> instructions = JSON::Parse(json_data)->ToObject();

        Local<String> source_key = String::NewFromUtf8(isolate, "source");
        Local<String> source = instructions->Get(source_key)->ToString();

        Local<Script> script = Script::Compile(source);

        if (script.IsEmpty()) {
            assert(try_catch.HasCaught());
            ReportException(isolate, &try_catch);
        } else {
            pthread_t t;
            void *res;
            struct TimeoutHandlerArgs args = {
                this,
                (long)1
            };

            pthread_create(&t, NULL, TimeoutHandler, &args);

            Local<Value> result = script->Run();

            pthread_cancel(t);
            pthread_join(t, &res);

            // FTRACE("Join: %x\n", res);

            if (result.IsEmpty()) {
                assert(try_catch.HasCaught());
                if (try_catch.Message().IsEmpty() && try_catch.StackTrace().IsEmpty()) {
                    TRACE("It's a timeout!\n");
                    Local<String> tt = String::NewFromUtf8(isolate, "timeout");
                    Report(isolate, tt, OP_TIMEOUT);
                } else {
                    TRACE("It's a regular error\n");
                    ReportException(isolate, &try_catch);
                }
                FTRACE("Replacing context: %i\n", packet->ref);
                // vm.CreateContext(packet->ref);
            } else {
                ReportOK(isolate, result);
            }
        }
    }
}

void VM::Call(Packet* packet) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    string input = packet->data;

    Local<Context> context = Local<Context>::New(isolate,
            contexts[packet->ref]);

    if (context.IsEmpty()) {
        Local<Value> tt = String::NewFromUtf8(isolate, "empty contextz");
        Report(isolate, tt, OP_INVALID_CONTEXT);
    } else {
        Context::Scope context_scope(context);

        Local<String> json_data = String::NewFromUtf8(isolate, input.c_str());
        Local<Object> instructions = JSON::Parse(json_data)->ToObject();

        Local<Object> global = context->Global();

        Local<String> function_key = String::NewFromUtf8(isolate, "function");
        Local<String> function_name = instructions->Get(function_key)->ToString();

        Local<String> args_key = String::NewFromUtf8(isolate, "args");
        Local<Value> args_value = instructions->Get(args_key);
        Local<Array> raw_args = Local<Array>::Cast(args_value);

        int len = raw_args->Length();
        Local<Value> *args = new Local<Value>[len];

        for (int i = 0; i < len; i++) { 
            args[i] = raw_args->Get(i);
        }

        // we cannot simply retrieve the function from the global scope as the
        // name can be something like `lol.flop['hi']`. wrapping the call in a
        // temporary function is much simpler than attempting to split the name
        // and check all the individual parts.
        Local<String> prefix = String::NewFromUtf8(isolate,
                "function __call() { return ");
        Local<String> suffix = String::NewFromUtf8(isolate,
                ".apply(null, arguments); }");
        Local<String> source = String::Concat(String::Concat(prefix,
                    function_name), suffix);
        Local<Script> script = Script::Compile(source);
        Local<Value> eval_result = script->Run();

        if (eval_result.IsEmpty()) {
            assert(try_catch.HasCaught());
            ReportException(isolate, &try_catch);
        } else {
            Local<String> fn = String::NewFromUtf8(isolate, "__call");
            Local<Function> function = Local<Function>::Cast(global->Get(fn));
            Local<Value> result = function->Call(global, len, args);

            if (result.IsEmpty()) {
                assert(try_catch.HasCaught());
                ReportException(isolate, &try_catch);
            } else {
                ReportOK(isolate, result);
            }
        }
    }
}
