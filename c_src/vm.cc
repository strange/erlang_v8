#include <map>
#include <vector>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"

#include "debug.h"
#include "vm.h"

using namespace v8;
using namespace std;

VM::VM(Platform* platform_, Isolate* isolate_, int scriptc, char* scriptv[]) {
    isolate = isolate_;
    platform = platform_;

    scripts = vector<string>(scriptv + 1, scriptv + scriptc);
}

Handle<Context> VM::GetContext(uint32_t ref) {
    return contexts[ref];
}

Isolate* VM::GetIsolate() {
    return isolate;
}

Platform* VM::GetPlatform() {
    return platform;
}

void VM::PumpMessageLoop() {
    while (v8::platform::PumpMessageLoop(platform, isolate)) continue;
}

void VM::TerminateExecution() {
    V8::TerminateExecution(isolate);
    TRACE("Isolate terminated: %i\n", 10);
}

bool VM::CreateContext(uint32_t ref) {
    Handle<v8::ObjectTemplate> global = ObjectTemplate::New(isolate);
    Handle<Context> context = Context::New(isolate, NULL, global);
    Context::Scope context_scope(context);
    // Initializing scripts for every new context. This is a
    // temporary solution.
    for (auto script : scripts) {
        Handle<String> source = String::NewFromUtf8(isolate,
                script.c_str());
        Handle<Script> compiled = Script::Compile(source);
        Handle<Value> result = compiled->Run();
    }
    contexts[ref] = context;
    return true;
}

bool VM::DestroyContext(uint32_t ref) {
    contexts.erase(ref);
    return true;
}

int VM::Size() {
    TRACE("Context size: %i\n", contexts.size());
}
