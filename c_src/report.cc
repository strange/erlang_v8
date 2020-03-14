#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <iostream>

#include "libplatform/libplatform.h"
#include "v8.h"

#include "debug.h"
#include "packet.h"
#include "report.h"

using namespace v8;
using namespace std;

const char* ToCString(const v8::String::Utf8Value& value) {
  return *value ? *value : "<string conversion failed>";
}

void Report(Isolate* isolate, Local<Value> response, uint8_t op) {
    uint32_t ref = 0;

    Local<Context> context = isolate->GetCurrentContext();
    Local<String> input;

    if (response->IsUndefined()) {
        TRACE("Undefined");
        input = String::NewFromUtf8(isolate, "undefined").ToLocalChecked();
    } else if (response->IsString()) {
        input = response->ToString(context).ToLocalChecked();
    } else {
        input = JSONStringify(isolate, response)->ToString(context).ToLocalChecked();
    }

    String::Utf8Value utf8 (isolate, input);
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

    TRACE("Flushed");
}

void ReportOK(Isolate* isolate, Local<Value> response) {
    Report(isolate, response, OP_OK);
}

void ReportError(Isolate* isolate, Local<Value> response) {
    Report(isolate, WrapError(isolate, response), OP_ERROR);
}

void ReportException(Isolate* isolate, TryCatch* try_catch) {
    HandleScope handle_scope(isolate);
    TRACE("CTX\n");
    Local<Context> context = isolate->GetCurrentContext();
    TRACE("STACK\n");
    MaybeLocal<Value> stack_trace = try_catch->StackTrace(context);

    if (stack_trace.IsEmpty()) {
        ReportError(isolate, try_catch->Exception());
    } else {
        TRACE("CSTR\n");
        const char* st = ToCString(String::Utf8Value(isolate, try_catch->StackTrace(context).ToLocalChecked()));
        FTRACE("Stack: %s\n", st);
        ReportError(isolate, try_catch->StackTrace(context).ToLocalChecked());
    }
}

Local<Value> WrapError(Isolate* isolate, Local<Value> exception) {
    EscapableHandleScope handle_scope(isolate);
    Local<Context> context = isolate->GetCurrentContext();

    Local<Object> obj = Object::New(isolate);

    String::Utf8Value exception_string(isolate, exception);
    std::string from = std::string(*exception_string);

    Maybe<bool> ignored __attribute((unused)) = obj->Set(context, String::NewFromUtf8(isolate, "error").ToLocalChecked(),
             exception);

    // add line number and other fancy details ...

    return handle_scope.Escape(obj);
}

Local<Value> JSONStringify(Isolate* isolate, Local<Value> obj) {
    Local<Context> context = isolate->GetCurrentContext();
    Local<Object> global = context->Global();
    EscapableHandleScope handle_scope(isolate);

    MaybeLocal<Value> JSONValue = global->Get(context, String::NewFromUtf8(isolate, "JSON").ToLocalChecked());
    Local<Object> JSON = JSONValue.ToLocalChecked()->ToObject(context).ToLocalChecked();
    Local<Function> stringify = Local<Function>::Cast(
            JSON->Get(context, String::NewFromUtf8(isolate, "stringify").ToLocalChecked()).ToLocalChecked());

    Local<Value> args[] = { obj };
    Local<Value> result = stringify->Call(context, JSON, 1, args).ToLocalChecked();

    return handle_scope.Escape(result);
}
