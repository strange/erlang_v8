#include <iostream>
#include <sstream>
#include <fstream>
#include <v8.h>

using namespace v8;
using namespace std;

const uint8_t OK_R = 0;
const uint8_t ERROR_R = 1;
const uint8_t ERROR_WITH_LINENO_R = 1;

const uint8_t EVAL_R = 0;
const uint8_t RESET_VM_R = 2;

struct Packet {
    uint8_t op;
    string data;
};

Handle<Value> json_stringify(Isolate* isolate, Handle<Value> obj);
void resp(Isolate* isolate, Handle<Value> response, int op);
void ok(Isolate* isolate, Handle<Value> response);
void error(Isolate* isolate, Handle<Value> response);
Handle<Value> wrap_error(Isolate* isolate, Handle<Value> value);
size_t packet_length();
bool next_packet(Packet* packet);
void eval(Isolate* isolate, string input);

void debug(string s) {
    ofstream debug;
    debug.open("/tmp/debug.txt", ios::app);
    debug << s << endl;
    debug.close();
}

void resp(Isolate* isolate, Handle<Value> response, int op) {
    String::Utf8Value utf8(json_stringify(isolate, response));
    uint16_t len = utf8.length() + 1;

    cout << (char)((len >> 8) & 0xff) << (char)(len & 0xff);
    cout << (char)op;
    cout << *utf8;
}

void ok(Isolate* isolate, Handle<Value> response) {
    resp(isolate, response, OK_R);
}

void error(Isolate* isolate, Handle<Value> response) {
    resp(isolate, wrap_error(isolate, response), ERROR_R);
}

Handle<Value> wrap_error(Isolate* isolate, Handle<Value> exception) {
    EscapableHandleScope handle_scope(isolate);

    String::Utf8Value exception_str(exception);

    Local<Object> obj = Object::New(isolate);
    obj->Set(String::NewFromUtf8(isolate, "error"),
            String::NewFromUtf8(isolate, *exception_str));
    // add line number and other fancy details ...

    return handle_scope.Escape(obj);
}

Handle<Value> json_stringify(Isolate* isolate, Handle<Value> obj) {
    Handle<Context> context = isolate->GetCurrentContext();
    Handle<Object> global = context->Global();
    EscapableHandleScope handle_scope(isolate);

    Handle<Object> JSON = global->Get(String::NewFromUtf8(isolate, "JSON"))
                               ->ToObject();
    Handle<Function> stringify = Handle<Function>::Cast(
            JSON->Get(String::NewFromUtf8(isolate, "stringify")));

    Handle<Value> args[] = { obj };
    Local<Value> result = stringify->Call(JSON, 1, args);

    return handle_scope.Escape(result);
}

size_t packet_length() {
    char byte1, byte2;

    if (!cin.get(byte1) || !cin.get(byte2)) {
        return 0;
    }

    return ((uint8_t)byte1 << 8) | (uint8_t)byte2;
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

void eval(Isolate* isolate, string input) {
    HandleScope handle_scope(isolate);
    TryCatch trycatch;

    Handle<String> source = String::NewFromUtf8(isolate, input.c_str());
    Handle<Script> script = Script::Compile(source);
    Handle<Value> result = script->Run();

    if (result.IsEmpty()) {
        Handle<Value> exception = trycatch.Exception();
        error(isolate, exception);
    } else {
        ok(isolate, result);
    }
}

int main(int argc, char* argv[]) {
    ios_base::sync_with_stdio(false);

    Isolate* isolate = Isolate::GetCurrent();
    HandleScope handle_scope(isolate);
    Handle<Context> context = Context::New(isolate);
    Context::Scope context_scope(context);

    Packet packet;
    while (next_packet(&packet)) {
        switch(packet.op) {
            case EVAL_R:
                eval(isolate, packet.data);
                break;
        }

        packet = (const struct Packet){ 0 };
    }

    V8::TerminateExecution(isolate);

    return 0;
}
