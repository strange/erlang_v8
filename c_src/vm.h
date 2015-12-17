#ifndef ERLANG_V8_VM_H
#define ERLANG_V8_VM_H

#include <map>
#include <vector>

class VM {
    private:
        v8::Isolate* isolate;
        v8::Platform* platform;
        std::map<uint32_t,v8::Handle<v8::Context>> contexts;
        std::vector<std::string> scripts;

    public:
        VM(v8::Platform* platform_, v8::Isolate* isolate_, int scriptc, char* scriptv[]); 

        v8::Handle<v8::Context> GetContext(uint32_t ref);
        v8::Isolate* GetIsolate();
        v8::Platform* GetPlatform();

        void PumpMessageLoop();
        void TerminateExecution();

        bool CreateContext(uint32_t ref);
        bool DestroyContext(uint32_t ref);

        int Size();
};

#endif
