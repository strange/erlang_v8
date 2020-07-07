#include <assert.h>
#include <map>
#include <vector>
#include <string>
#include <pthread.h>
#include <unistd.h>

#include "libplatform/libplatform.h"
#include "v8.h"

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

    FTRACE("Timeout handler started: %li\n", args->timeout);
    usleep(args->timeout * 1000);
    TRACE("Timeout expired. Terminating execution.\n");

    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, 0x00);
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, 0x00);

    args->vm->TerminateExecution();

    return NULL;
}

VM::VM(Platform* platform_, Isolate* isolate_) {
    isolate = isolate_;
    platform = platform_;
}

bool VM::CreateContext(uint32_t ref) {
    HandleScope handle_scope(isolate);

    Local<v8::ObjectTemplate> global = ObjectTemplate::New(isolate);
    Local<Context> context = Context::New(isolate, NULL, global);

    Persistent<Context, CopyablePersistentTraits<Context>> pcontext(isolate, context);

    Context::Scope context_scope(context);

    contexts[ref] = pcontext;

    Local<String> tt = String::NewFromUtf8(isolate, "context created");
    Report(isolate, tt, OP_OK);

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
        Local<String> tt = String::NewFromUtf8(isolate, "context destroyed");
        Report(isolate, tt, OP_OK);
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
    // v8::V8::TerminateExecution(isolate); Was deprecated, is now removed, the isolate
    //                                      member function does the same.
    isolate->TerminateExecution();
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
        Local<String> tt = String::NewFromUtf8(isolate, "empty context");
        Report(isolate, tt, OP_INVALID_CONTEXT);
    } else {
        Context::Scope context_scope(context);

        string input = packet->data;

        Local<String> json_data = String::NewFromUtf8(isolate, input.c_str());
        Local<Object> instructions = Local<Object>::Cast(
            JSON::Parse(context, json_data).ToLocalChecked()
        );

        Local<String> timeout_key = String::NewFromUtf8(isolate, "timeout");
        Local<String> source_key = String::NewFromUtf8(isolate, "source");

        Local<String> source = instructions->Get(source_key)->ToString();
        Local<Integer> timeout = instructions->Get(timeout_key)->ToInteger();

        Local<Script> script = Script::Compile(source);

        if (script.IsEmpty()) {
            assert(try_catch.HasCaught());
            ReportException(isolate, &try_catch);
        } else {
            pthread_t t;
            void *res;
            struct TimeoutHandlerArgs timeout_handler_args = {
                this,
                (long)timeout->Int32Value()
            };

            pthread_create(&t, NULL, TimeoutHandler, &timeout_handler_args);

            Local<Value> result = script->Run();

            pthread_cancel(t);
            pthread_join(t, &res);

            // FTRACE("Join: %x\n", res);

            if (result.IsEmpty()) {
                assert(try_catch.HasCaught());
                if (try_catch.Message().IsEmpty() && try_catch.StackTrace().IsEmpty()) {
                    TRACE("Execution timed out.\n");
                    Local<String> tt = String::NewFromUtf8(isolate, "timeout");
                    Report(isolate, tt, OP_TIMEOUT);
                } else {
                    TRACE("Regular error\n");
                    ReportException(isolate, &try_catch);
                }
                // vm.CreateContext(packet->ref);
            } else {
                ReportOK(isolate, result);
            }
        }
    }
}

ScriptOrigin ModuleOrigin(Local<v8::Value> resource_name, Isolate* isolate) {
  ScriptOrigin origin(resource_name, Local<v8::Integer>(), Local<v8::Integer>(),
                      Local<v8::Boolean>(), Local<v8::Integer>(),
                      Local<v8::Value>(), Local<v8::Boolean>(),
                      Local<v8::Boolean>(), True(isolate));
  return origin;
}

MaybeLocal<Module> ResolveCallback(Local<Context> context,
                                   Local<String> specifier,
                                   Local<Module> referrer) {
//  if (specifier->StrictEquals(v8_str("./dep1.js"))) {
//    return dep1;
//  } else {
    TRACE("Resolve callback called back!\n");
    Isolate *isolate = context->GetIsolate();
    isolate->ThrowException(String::NewFromUtf8(isolate, "boom"));
    return MaybeLocal<Module>();
//  }
}

const char *to_c(Isolate *i, Local<Value> s) {
    TryCatch try_catch(i);
    v8::String::Utf8Value uv(i, s);
    if (uv.length() == 0) {
	TRACE("Could not convert");
	if (try_catch.HasCaught()) {
	    TRACE("Exception caught");
	    Local<String> msg = try_catch.Message()->Get();
	    TRACE("TODO stringify msg if you hit this...");
	}
	return "<oops>";
    } else {
	std::string stds = std::string(*uv);
	FTRACE("to_c(...) -> len %d, v <%s>\n", uv.length(), stds.c_str());
	return stds.c_str();
    }
}

void VM::CompileModule(Packet* packet) {
    HandleScope handle_scope(isolate);
    TryCatch try_catch(isolate);

    Local<Context> context = Local<Context>::New(isolate,
            contexts[packet->ref]);

    if (context.IsEmpty()) {
        Local<String> tt = String::NewFromUtf8(isolate, "empty context");
	FTRACE("Empty context %i\n", packet->ref);
        Report(isolate, tt, OP_INVALID_CONTEXT);
    } else {
        string input = packet->data;

	FTRACE("Unpacking args from [%s]\n", input.c_str());
        Local<String> json_data = String::NewFromUtf8(isolate, input.c_str());
	FTRACE("Click 1 from [%s]\n", to_c(isolate, json_data));
        Local<Object> instructions = Local<Object>::Cast(
            JSON::Parse(context, json_data).ToLocalChecked()
        );
	FTRACE("Click 2 %s\n", to_c(isolate, instructions->ToString(context).ToLocalChecked()));

	Local<String> name_key = String::NewFromUtf8(isolate, "name");
	FTRACE("Click 3, name_key = [%s]\n", to_c(isolate, name_key));
        Local<String> source_key = String::NewFromUtf8(isolate, "source");
	FTRACE("Click 4, source_key = [%s]\n", to_c(isolate, source_key));

	Local<String> name = instructions->Get(name_key)->ToString();
	FTRACE("Click 5, name is [%s]\n", to_c(isolate, name));
        //Local<String> source_code = instructions->Get(source_key)->ToString();
	Local<String> source_code = String::NewFromUtf8(isolate, "export function sum(x, y) { return x + y }");
	FTRACE("Click 6, code is [%s]\n", to_c(isolate, source_code));
	FTRACE("Click 6, code is [%s]\n", to_c(isolate, source_code->ToDetailString(context).ToLocalChecked()));

	ScriptOrigin origin = ModuleOrigin(name, isolate);
	ScriptCompiler::Source source(source_code, origin);
	TRACE("Compile module\n");
	MaybeLocal<Module> maybe_module = ScriptCompiler::CompileModule(isolate, &source);
	TRACE("Checking status\n");

	if (maybe_module.IsEmpty()) {
	    TRACE("Module is empty\n");
            assert(try_catch.HasCaught());
            ReportException(isolate, &try_catch);
        } else {
	    Local<Module> module = maybe_module.ToLocalChecked();
	    if (module->GetStatus() == v8::Module::kErrored) {
		TRACE("Bad status\n");
		assert(try_catch.HasCaught());
		ReportException(isolate, &try_catch);
	    } else {
		TRACE("Good status, instantiating module\n");
		Maybe<bool> result = module->InstantiateModule(context, ResolveCallback);
		if (result.IsNothing() || module->GetStatus() == v8::Module::kErrored) {
		    TRACE("Instantiate no workies\n");
		    assert(try_catch.HasCaught());
		    ReportException(isolate, &try_catch);
		} else {
		    TRACE("Instantiate workies\n");
		    ReportOK(isolate, name);
		}
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
        Local<Value> tt = String::NewFromUtf8(isolate, "empty context");
        Report(isolate, tt, OP_INVALID_CONTEXT);
    } else {
        Context::Scope context_scope(context);

        Local<String> json_data = String::NewFromUtf8(isolate, input.c_str());
        Local<Object> instructions = Local<Object>::Cast(
                JSON::Parse(context, json_data).ToLocalChecked()
        );

        Local<Object> global = context->Global();

        Local<String> function_key = String::NewFromUtf8(isolate, "function");
        Local<String> function_name = instructions->Get(function_key)->ToString();

        Local<String> timeout_key = String::NewFromUtf8(isolate, "timeout");
        Local<String> args_key = String::NewFromUtf8(isolate, "args");
        Local<Value> args_value = instructions->Get(args_key);
        Local<Array> raw_args = Local<Array>::Cast(args_value);
        Local<Integer> timeout = instructions->Get(timeout_key)->ToInteger();

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

        pthread_t t;
        void *res;
        struct TimeoutHandlerArgs timeout_handler_args = {
            this,
            (long)timeout->Int32Value()
        };

        pthread_create(&t, NULL, TimeoutHandler, &timeout_handler_args);

        Local<Value> eval_result = script->Run();

        pthread_cancel(t);
        pthread_join(t, &res);

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
