#ifndef SPIO_DEBUG_H_INCLUDED
#define SPIO_DEBUG_H_INCLUDED
#include "spio_types.h"

enum spio_t_assertions_level {
  SPIO_ASSERTION_DISABLE = 0,
  SPIO_ASSERTION_REPORT,        /* report assertion violation but continue */
  SPIO_ASSERTION_DEBUG,         /* report and break into debugger. */
  SPIO_ASSERTION_ABORT          /* report and abort() (dump core) */
};

extern enum spio_t_assertions_level spio_assertions;

extern int spio_trace_line(char const *function, char const *file, int line, char const *msg, spio_t_intptr l, char const *string, int level);
extern int spio_trace_line_p(char const *function, char const *file, int line, char const *msg, void *p, char const *string, int level);
extern int spio_trace_line_lls(char const *function, char const *file, int line, char const *msg, spio_t_intptr l1, spio_t_intptr l2, char const *string, int level);

#define SPIO_DEBUG_BREAK_FUNCTION_NEEDED 1
extern int spio_debug_break(char const *file, int line);

#if SPIO_DEBUG

#ifndef SPIO_DEBUG_BREAK_
#define SPIO_DEBUG_BREAK_ spio_debug_break
#endif  /* SPIO_DEBUG_BREAK_ */
#ifndef SPIO_ERROR_NAME_
#define SPIO_ERROR_NAME_ spio_error_name
#endif  /* SPIO_ERROR_NAME_ */
#ifndef SPIO_TRACE_LINE_
#define SPIO_TRACE_LINE_ spio_trace_line
#endif  /* SPIO_TRACE_LINE_ */

#if defined(_MSC_VER) && SPIO_INCLUDE_OS_TYPES
#define SPIO_DEBUG_BREAK_FL(F,L) do{(void)(L); (void)(F); DebugBreak();}while(0)
#else  /* !_MSC_VER */
#define SPIO_DEBUG_BREAK_FL(F,L) SPIO_DEBUG_BREAK_((F), (L))
#endif /* !_MSC_VER */

#ifndef SPIO_TRACE_LEVEL
#define SPIO_TRACE_LEVEL 1
#endif  /* SPIO_TRACE_LEVEL */

#define SPIO_TRACE_LINE_ls(MSG, LONG, STRING) SPIO_TRACE_LINE_(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (spio_t_intptr)(LONG), (STRING), SPIO_TRACE_LEVEL)
#define SPIO_TRACE_LINE_lls(MSG, LONG1, LONG2, STRING) spio_trace_line_lls(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (spio_t_intptr)(LONG1), (spio_t_intptr)(LONG2), (STRING), SPIO_TRACE_LEVEL)
#define SPIO_TRACE_LINE_l(MSG, LONG) SPIO_TRACE_LINE_(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (spio_t_intptr)(LONG), NULL, SPIO_TRACE_LEVEL)
#define SPIO_TRACE_LINE_p(MSG, PTR) spio_trace_line_p(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (void*)(PTR), NULL, SPIO_TRACE_LEVEL)

#else  /* !SPIO_DEBUG */

#define SPIO_TRACE_LINE_l(MSG, LONG) do {;}while(0)
#define SPIO_TRACE_LINE_p(MSG, PTR) do {;}while(0)
#define SPIO_TRACE_LINE_ls(MSG, LONG, STRING) do {;}while(0)
#define SPIO_TRACE_LINE_lls(MSG, LONG1, LONG2, STRING) do {;}while(0)

#define SPIO_DEBUG_BREAK_FL(F,L) do {;}while(0)


#endif  /* !SPIO_DEBUG */

#define SPIO_DEBUG_BREAK() SPIO_DEBUG_BREAK_FL(__FILE__, __LINE__)

#if !defined SPIO_ANALYZING
#if defined __clang_analyzer__
#define SPIO_ANALYZING 1
#endif /* defined __clang_analyzer__ */
#endif /* !defined SPIO_ANALYZING */

#if !defined SPIO_ANALYZING
#if _MSC_VER >= 1400 && defined _PREFAST_                /* [PM] static analysis with cl.exe /analyze */
#define SPIO_ANALYZING 1
#endif  /* _PREFAST_ */
#endif /* !defined SPIO_ANALYZING */

#if !defined SPIO_ANALYZING
#define SPIO_ANALYZING 0 /* Assume we are compiled for real, not just for static analysis */
#endif /* !defined SPIO_ANALYZING */

#if SPIO_ANALYZING
/* [PM] 4.2.2 When analyzing we do not care if TEST is evaluated more
   than once. Also we may implement SPIO_ANALYZER_ASSUME as a
   conditional call to a function that the compiler/analyzer knows
   will not return. All of this is prohibited if we are actually
   compiling for real, i.e. generating code. */
#define SPIO_ASSERTION_ASSUME(TEST) SPIO_ANALYZER_ASSUME((TEST))
#else  /* !SPIO_ANALYZING */
/* [PM] 4.2.2 When compiling for real, SPIO_ANALYZER_ASSUME(TEST) must
   not evaluate the TEST at all. It is expected to expand to something
   like the MSVC __assume(TEST) */
#define SPIO_ASSERTION_ASSUME(TEST) SPIO_COMPILER_ASSUME((TEST))
#endif  /* !SPIO_ANALYZING */

#if !defined SPIO_ANALYZER_NORETURN
#if defined __clang_analyzer__
#define SPIO_ANALYZER_NORETURN __attribute__((analyzer_noreturn))
#endif /* defined __clang_analyzer__ */
#endif /* !defined SPIO_ANALYZER_NORETURN */


#if SPIO_DEBUG
#ifndef SPIO_ASSERTIONS
#define SPIO_ASSERTIONS 1
#endif  /* !SPIO_ASSERTIONS */
#endif  /* SPIO_DEBUG */

/* MSVC complains about "C4555 ... expression has no effect" if this expands to empty when !SPIO_DEBUG*/
/* gcc complains about "warning: statement with no effect" if this expands to empty when SPIO_DEBUG */
#define SPIO_ASSERT_CAST_TO_VOID_ (void)

#if SPIO_ASSERTIONS

#ifndef SPIO_ASSERT_FAILURE_
#define SPIO_ASSERT_FAILURE_ spio_assert_failure
#endif  /* SPIO_ASSERT_FAILURE_ */

#define SPIO_ASSERT2(ENABLE,TEST)  do { (SPIO_ASSERT_CAST_TO_VOID_ ( (ENABLE) ? ( SPIO_UNLIKELY(!(TEST)) ? SPIO_ASSERT_FAILURE_(SPIO__FUNCTION__, __FILE__,__LINE__) : 1 ) : 1 )); SPIO_ASSERTION_ASSUME((TEST)); } while(0)
#define SPIO_ASSERT_VALUE2(ENABLE, TEST) ( (ENABLE) ? ( SPIO_UNLIKELY(!(TEST)) ? SPIO_ASSERT_FAILURE_(SPIO__FUNCTION__, __FILE__,__LINE__) : 1 ) : 1 ) /* Always returns 1 */
#else  /* !SPIO_ASSERTIONS */
#define SPIO_ASSERT2(ENABLE,TEST)    (SPIO_ASSERT_CAST_TO_VOID_  1)
#define SPIO_ASSERT_VALUE2(ENABLE, TEST) ( 1 ) /* Always returns 1 */

#endif  /* !SPIO_ASSERTIONS */

#if !defined SPIO_ANALYZER_ASSUME
#if _MSC_VER >= 1400 && defined _PREFAST_                /* [PM] static analysis with cl.exe /analyze */
#include <CodeAnalysis/sourceannotations.h>
#define SPIO_ANALYZER_ASSUME(CONDITION) __analysis_assume((CONDITION))
#endif /* _PREFAST_ */
#endif  /* !defined SPIO_ANALYZER_ASSUME */

#if !defined SPIO_COMPILER_ASSUME
#if _MSC_VER >= 1400                /* [PM] static analysis with cl.exe /analyze */
#if 0                           /* FIXME: consider enabling this. It could help speed things up. */
#define SPIO_COMPILER_ASSUME(CONDITION) __assume((CONDITION))
#endif  /* 0 */
#endif /* _MSC_VER >= 1400 */
#endif  /* !defined SPIO_COMPILER_ASSUME */

#ifndef SPIO_COMPILER_ASSUME
#define SPIO_COMPILER_ASSUME(CONDITION)  /* empty */
#endif  /* !SPIO_COMPILER_ASSUME */


#if !defined SPIO_ANALYZER_ASSUME
#if defined SPIO_ANALYZER_NORETURN
/* We have a non-default, i.e. effective, definition of
   SPIO_ANALYZER_NORETURN, so it will tell the analyzer that
   spio_analyzer_failure() never returns. */

/* Dummy, does nothing and returns (but tell analyzer that it does not
   return). Used for stopping the analyzed code path for static analyzer. */
extern void spio_analyzer_failure(void) SPIO_ANALYZER_NORETURN;
#define SPIO_ANALYZER_FAILURE_NEEDED 1

#define SPIO_ANALYZER_ASSUME(CONDITION)  do{if (!(CONDITION)) { spio_analyzer_failure(); } } while(0);
#endif  /* defined SPIO_ANALYZER_NORETURN */
#endif  /* !define SPIO_ANALYZER_ASSUME */



#ifndef SPIO_ANALYZER_ASSUME
/* If it is good enough for the compiler then it is safe for the analyzer to use it too. */
#define SPIO_ANALYZER_ASSUME(CONDITION)  SPIO_COMPILER_ASSUME((CONDITION))
#endif  /* !SPIO_ANALYZER_ASSUME */


/* Both SPIO_ASSERT and SPIO_ASSERT1 (and thus SP_ASSERT) tells VC8 PreFast to assume the TEST is true. */
#define SPIO_ASSERT1(ENABLE,TEST) do { SPIO_ASSERT2((ENABLE), (TEST)); SPIO_ASSERTION_ASSUME((TEST)); }while(0)
/* This does not tell PreFast anything about TEST. */
#define SPIO_ASSERT_VALUE1(ENABLE,VALUE,TEST) ( ( /* The ?: tries to avoid warning about "expression result unused" */ SPIO_ASSERT_VALUE2((ENABLE), (TEST))) ? (VALUE) : (VALUE) )

#ifndef SPIO_SOFT_ASSERTIONS
#if SPIO_DEBUG
#define SPIO_SOFT_ASSERTIONS 1
#endif  /* SPIO_DEBUG */
#endif  /* SPIO_SOFT_ASSERTIONS */

#if SPIO_SOFT_ASSERTIONS
/* An assertion for unexpected conditions that are not errors. Soft
   assertions should therefore always be disabled in non-debug code
   (whereas ordinary assertions could (and perhaps should) be enabled
   where it not for their speed impact)

   Also, soft assertions should NEVER let the compiler, or a a static
   analyzer (like PreFast), assume that the condition holds
 */
#define SPIO_SOFT_ASSERT1(ENABLE,TEST) SPIO_ASSERT2((ENABLE), (TEST))
#else  /* !SPIO_SOFT_ASSERTIONS */
#define SPIO_SOFT_ASSERT1(ENABLE,TEST) /* empty */
#endif  /* !SPIO_SOFT_ASSERTIONS */

/* Represents the run-time test for whether assertions should be
   verified. If already defined, e.g. as 1, then we avoid the
   reference to the external symbol "spio_assertions", a symbol that
   is not always available in separate DSOs.' */
#if !defined(SPIO_ASSERTIONS_ENABLED)
#define SPIO_ASSERTIONS_ENABLED (spio_assertions!=SPIO_ASSERTION_DISABLE)
#endif	/* !defined(SPIO_ASSERTIONS_ENABLED) */

/* SPIO_ASSERT and SPIO_ASSERT1 become just SPIO_ASSERTION_ASSUME(TEST) if
   !SPIO_ASSERTIONS

   Thus, eventually, we may use this to tell the compiler about what
   it can assume.
 */
#define SPIO_ASSERT(TEST)      SPIO_ASSERT1(SPIO_ASSERTIONS_ENABLED, (TEST))
/* Like SPIO_ASSERT(TEST) but always returns VALUE, also when !SPIO_ASSERTIONS */
#define SPIO_ASSERT_VALUE(VALUE, TEST) SPIO_ASSERT_VALUE1(SPIO_ASSERTIONS_ENABLED, (VALUE), (TEST))

/* SPIO_SOFT_ASSERT and SPIO_SOFT_ASSERT1 become no-ops if
   !SPIO_SOFT_ASSERTIONS (or if !SPIO_ASSERTIONS)
*/
#define SPIO_SOFT_ASSERT(TEST) SPIO_SOFT_ASSERT1(SPIO_ASSERTIONS_ENABLED, (TEST))

#if SPIO_DEBUG
#define SPIO_ALLOCATOR_DEBUG_ARGS_DECL , char const *file, int line
#define SPIO_ALLOCATOR_DEBUG_ARGS_PASS_ON , file, line
#define SPIO_ALLOCATOR_DEBUG_ARGS , __FILE__, __LINE__
#else  /* !SPIO_DEBUG */
#define SPIO_ALLOCATOR_DEBUG_ARGS_DECL /* empty */
#define SPIO_ALLOCATOR_DEBUG_ARGS_PASS_ON  /* empty */
#define SPIO_ALLOCATOR_DEBUG_ARGS /* empty */
#endif

/* Always executes (EXPR) and also asserts that it succeeds (not allowed to set 'code') */
#define SPIO_SUCCEEDS(EXPR) do {                        \
   spio_t_error_code spio_succeeds_tmp = (EXPR);        \
   SPIO_ASSERT(!SPIO_FAILED(spio_succeeds_tmp));        \
   (void)spio_succeeds_tmp;                             \
} while(0)

/* use this for guarding against (allowed cases) null s or *s */
#define SPIO_VALID(S) (((S) != NULL) && (*(S) != NULL))
#define SPIO_PAD_MAGIC_INITED 4711
/* use this more extensive test in assertions */
#define SPIO_VALIDATE(S) (SPIO_VALID(S) && ((*(S))->funcs != NULL) && ((*(S))->pad == SPIO_PAD_MAGIC_INITED))

#ifndef SPIO_BARF_HOOK
/* define SPIO_TRACKED_ERROR_CODE to, e.g. SPIO_E_INTERRUPTED to get a debug break when such an error happens anywhere */
#ifdef SPIO_TRACKED_ERROR_CODE
#define SPIO_TRACKED_CODE_ && code != SPIO_TRACKED_ERROR_CODE
#else  /* !SPIO_TRACKED_ERROR_CODE */
#define SPIO_TRACKED_CODE_      /* empty */
#endif /* !SPIO_TRACKED_ERROR_CODE */
/* [PM] 4.2 added SPIO_E_INVALID_STATE to default tracked errors */
#define SPIO_BARF_HOOK(CODE) if (code != SPIO_E_IMPOSSIBLE_ERROR && code != SPIO_E_INTERNAL_ERROR && code != SPIO_E_INVALID_STATE SPIO_TRACKED_CODE_) {} else { SPIO_DEBUG_BREAK(); }
#endif  /* SPIO_BARF_HOOK */

#define SPIO_BARF(CODE) do { code = (CODE); SPIO_TRACE_LINE_ls("barf", code, SPIO_ERROR_NAME_(code)); SPIO_BARF_HOOK(code); goto SPIO_BARF_LABEL; } while (0)
/* [PM] 4.1.2 Ensure gcc does not warn even if EXPR is something like (code = foo()) */
#define SPIO_CHECK(EXPR) do{                            \
    spio_t_error_code SPIO_CHECK_code_ = (EXPR);        \
    code = SPIO_CHECK_code_;                            \
    if (SPIO_FAILED(SPIO_CHECK_code_)) {                \
      SPIO_BARF(SPIO_CHECK_code_);                      \
    }                                                   \
} while(0)
/* Sometimes useful for suppressing compiler warnings about unused barf: label */
#define SPIO_REFERENCE_BARF_LABEL() while (0) { goto barf; }

#define SPIO_NULL_CHECK(EXPR) if (SPIO_UNLIKELY((EXPR) == NULL)) { SPIO_BARF(SPIO_E_OUT_OF_MEMORY); } else { /* empty */}

#if SPIO_UNIX
/* Most POSIX functions return -1 on failure */
#define SPIO_POSIX_CHECK1(EXPR, ERRNO, DEFAULT_ERROR) if (SPIO_UNLIKELY((EXPR) == -1)) { int posix_check_err_no = (ERRNO); SPIO_TRACE_LINE_l("errno==", posix_check_err_no); SPIO_BARF(spio_map_unix_error(posix_check_err_no, (DEFAULT_ERROR))); } else { /* empty */}
#define SPIO_POSIX_CHECK(EXPR, DEFAULT_ERROR) SPIO_POSIX_CHECK1((EXPR), errno, (DEFAULT_ERROR))
#endif  /* SPIO_UNIX */

#if SPIO_WIN32
/* Most Win32 functions return 0/NULL on failure */
#define SPIO_WIN32_CHECK1(EXPR, ERRNO, DEFAULT_ERROR) if (SPIO_UNLIKELY((EXPR) == 0)) { DWORD win32_check_err_no = (ERRNO); SPIO_TRACE_LINE_l("GetLastError==", win32_check_err_no); SPIO_BARF(spio_map_win32_error(win32_check_err_no, (DEFAULT_ERROR))); } else { /* empty */}
#define SPIO_WIN32_CHECK(EXPR, DEFAULT_ERROR) SPIO_WIN32_CHECK1((EXPR), GetLastError(), (DEFAULT_ERROR))
#endif  /* SPIO_WIN32_CHECK1 */

#define SPIO_ASSERT_MAIN_THREAD() SPIO_ASSERT(spio_is_in_main_thread() == SPIO_S_TRUE)


#if SPIO_WIN32

#if SPIO_INCLUDE_OS_TYPES

#define SPIO_BARF_LAST_ERROR(DEFAULT_ERROR) do { SPIO_TRACE_LINE_l("BARF GetLastError()==", GetLastError()); SPIO_BARF(spio_map_win32_error(GetLastError(), (DEFAULT_ERROR))); } while(0)

#define SPIO_HANDLE_CHECK(EXPR, DEFAULT_ERROR) if ((EXPR) == INVALID_HANDLE_VALUE) { SPIO_BARF_LAST_ERROR(DEFAULT_ERROR); } else { /* empty */}

#if 0
#define SPIO_CHECK_LAST_ERROR(DEFAULT_ERROR) if (GetLastError() != NO_ERROR) { SPIO_BARF(spio_map_win32_error(GetLastError(), (DEFAULT_ERROR))); } else { /* empty */}
#endif  /* 0 */

#endif  /* SPIO_INCLUDE_OS_TYPES */

#endif  /* SPIO_WIN32 */

#define SPIO_DEBUG_DUMP_BACKTRACE_OPTION_STDOUT SPIO_BIT(0)
#define SPIO_DEBUG_DUMP_BACKTRACE_OPTION_STDERR SPIO_NEXT_BIT(SPIO_DEBUG_DUMP_BACKTRACE_OPTION_STDOUT)

extern void spio_debug_dump_backtrace(char const *title, spio_t_bits options);

extern spio_t_error_code spio_is_in_main_thread(void);

extern spio_t_error_code spio_init_debug(spio_t_bits options);

extern int spio_assert_failure(char const *function, char const *file, int line);

extern int spio_msb32_debug(spio_t_uint32 x);
extern int spio_msb64_debug(spio_t_uint64 x);

#endif  /* SPIO_DEBUG_H_INCLUDED */
