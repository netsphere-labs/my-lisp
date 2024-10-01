
#ifndef INCLUDE_DEBUG_H
#define INCLUDE_DEBUG_H

#ifndef NDEBUG
  // Debug environment /////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h> // va_start()

// 不要のときはコメントアウト
//#define DEBUG_OBJECT_LIFETIMES         1
//#define DEBUG_ENV_LIFETIMES            1
#define DEBUG_TRACE_FILE    stderr


#ifdef _WIN32
#ifndef __func__
  #define __func__ __FUNCTION__
#endif
#endif // _WIN32

#define TRACE(...)   _TRACE2(__VA_ARGS__, "")
#define _TRACE2(format, ...)  fprintf(DEBUG_TRACE_FILE, format, __VA_ARGS__)

inline void _ASSERT_FAIL(const char* file, int line, const char* func,
                         const char* condition,
                         const char* format, ...)     {
    fprintf(DEBUG_TRACE_FILE, "Assertion `%s` failed at %s(%d) %s: ", condition, file, line, func);
    va_list args;
    va_start(args, format);
    vfprintf(DEBUG_TRACE_FILE, format, args);
    va_end(args);

    abort();
}

#define ASSERT(condition, ...) \
    (static_cast<bool>(condition) ? (void) 0 : \
     _ASSERT_FAIL(__FILE__, __LINE__, __func__, #condition, __VA_ARGS__, "") )

#ifndef VERIFY
// VERIFY()マクロは, リリースビルドでも, 式を評価します.
  #define VERIFY     ASSERT
#endif


#else // NDEBUG
  // release environment ////////////////////////////////////////////////////

#define TRACE(...) ((void) 0)

#define ASSERT(condition, ...) ((void) 0)

#ifndef VERIFY
// VERIFY()マクロは, リリースビルドでも, 式を評価します.
  #define VERIFY(check)   (static_cast<void>(check))
#endif


#endif // !NDEBUG


#if DEBUG_OBJECT_LIFETIMES
    #define TRACE_OBJECT TRACE
#else
    #define TRACE_OBJECT(...) ((void) 0)
#endif

#if DEBUG_ENV_LIFETIMES
    #define TRACE_ENV TRACE
#else
    #define TRACE_ENV(...) ((void) 0)
#endif


#endif // INCLUDE_DEBUG_H
