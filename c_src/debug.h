#ifndef DEBUG_H
#define DEBUG_H

#define DEBUG 0

#define FTRACE(fmt, ...) \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, \
            __LINE__, __func__, __VA_ARGS__); } while (0)

#define TRACE(s) \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " s, __FILE__, \
            __LINE__, __func__); } while (0)

#endif
