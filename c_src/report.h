#ifndef ERLANG_V8_REPORT_H
#define ERLANG_V8_REPORT_H

v8::Local<v8::Value> JSONStringify(v8::Isolate* isolate, v8::Local<v8::Value> obj);
v8::Local<v8::Value> WrapError(v8::Isolate* isolate, v8::Local<v8::Value> exception);
void Report(v8::Isolate* isolate, v8::Local<v8::Value> response, uint8_t op);
void ReportOK(v8::Isolate* isolate, v8::Local<v8::Value> response);
void ReportError(v8::Isolate* isolate, v8::Local<v8::Value> response);
void ReportException(v8::Isolate* isolate, v8::TryCatch* try_catch);

#endif
