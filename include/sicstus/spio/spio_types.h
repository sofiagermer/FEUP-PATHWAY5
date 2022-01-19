#ifndef SPIO_TYPES_H_INCLUDED
#define SPIO_TYPES_H_INCLUDED 1

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 or later */
#  define SPIO__FUNCTION__ __func__
#else
# if __GNUC__ >= 2
#  define SPIO__FUNCTION__ __FUNCTION__
# elif _MSC_VER >= 1300         /* VS .NET 2003 or newer */
#  define SPIO__FUNCTION__ __FUNCTION__
# endif
#endif
#ifndef SPIO__FUNCTION__
# define SPIO__FUNCTION__ "<unknown>"
#endif  /* SPIO__FUNCTION__ */

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 or later */
#  define SPIO_INLINE inline
#else
# if __GNUC__
#  define SPIO_INLINE __inline__
# elif _MSC_VER
#  define SPIO_INLINE __inline
# endif
#endif
#ifndef SPIO_INLINE
# define SPIO_INLINE             /* nothing */
#endif

#include "spio_config.h"

/* [PM] 4.2.1+ These used to live in spio_debug.h, where they belong, but they are needed very early. */
/* [PM] 4.1.2 These compiletime asserts have no runtime cost so leave them enabled at all times */
#define SPIO_COMPILETIME_ASSERT(TEST)  do { int spio_compiletime_assert[ (TEST) ? 1 : -1 ]; (void)(sizeof spio_compiletime_assert); } while(0)
#define SPIO_COMPILETIME_ASSERT_DECLARATION(TEST) extern int spio_compiletime_assert_declaration[ (TEST) ? 1 : -1 ]


#ifndef SPIO_EXPECT
#if defined(__GNUC__)
/* [PM] 4.0 Tells gcc that EXPR is likely to have value C. */
#define SPIO_EXPECT(EXPR, C) __builtin_expect((EXPR), (C))
#endif  /* __GNUC__ */
#endif  /* SPIO_EXPECT */

#ifndef SPIO_EXPECT
#define SPIO_EXPECT(EXPR, C) (EXPR)
#endif  /* SPIO_EXPECT */

/* [PM] 4.0 Tell compiler that EXPR is unlikely to be non-zero. Use in if conditions */
#define SPIO_UNLIKELY(EXPR) (SPIO_EXPECT((EXPR), 0) != 0)


#if SPIO_INCLUDE_OS_TYPES
#if SPIO_WIN32
#ifndef STRICT
#define STRICT 1
#endif

#pragma warning( push )

#ifdef _PREFAST_
/*
  wspiapi.h(1001) : warning C6011: Dereferencing NULL pointer 'pfGetAddrInfo': Lines: 995, 996, 998, 999, 1001
 */
#pragma warning( disable : 6011 )
#endif  /* _PREFAST_ */

/* [PM] 4.0 Always include winsock2 instead of windows to prevent winsock.h from making subsequent inclusion of winsock2.h impossible */
#include <winsock2.h>

#pragma warning( pop ) 

#endif  /* SPIO_WIN32 */
#endif /* SPIO_INCLUDE_OS_TYPES */

#if SPIO_HAVE_UNISTD_H
#include <unistd.h>             /* ssize_t, intptr_t */
#endif  /* SPIO_HAVE_UNISTD_H */
#if SPIO_HAVE_INTTYPES_H
#include <inttypes.h>		/* uintptr_t */
#endif /* SPIO_HAVE_INTTYPES_H */

#ifdef _MSC_VER
#define SPIO_CDECL __cdecl
#else  /* !_MSC_VER */
#define SPIO_CDECL              /* empty */
#endif  /* !_MSC_VER */

#if defined(__GNUC__)
#define SPIO_GCC__EXTENSIONS__ __extension__ /* [PM] 4.1 SPRM 11519 */
#else  /* !__GNUC__ */
#define SPIO_GCC__EXTENSIONS__ /* empty */
#endif  /* !__GNUC__ */

typedef unsigned char spio_t_uint8;
typedef spio_t_uint8 spio_t_byte;
typedef signed short spio_t_int16;
typedef unsigned short spio_t_uint16;
typedef signed int spio_t_int32;
typedef unsigned int spio_t_uint32;
#define SPIO_UINT32_MAX (~(spio_t_uint32)0)
#ifdef _MSC_VER                    /* MS CL.EXE */
typedef          __int64 spio_t_int64;
typedef unsigned __int64 spio_t_uint64;

#define SPIO_PRId64 "I64d"
#define SPIO_PRIu64 "I64u"
#define SPIO_PRIx64 "I64x"
#define SPIO_PRIX64 "I64X"

#else  /* !_MSC_VER */
SPIO_GCC__EXTENSIONS__          /* [PM] 4.1 SPRM 11519 */
typedef signed long long spio_t_int64;
SPIO_GCC__EXTENSIONS__          /* [PM] 4.1 SPRM 11519 */
typedef unsigned long long spio_t_uint64;

#define SPIO_PRId64 "lld"
#define SPIO_PRIu64 "llu"
#define SPIO_PRIx64 "llx"
#define SPIO_PRIX64 "llX"

#endif  /* !_MSC_VER */
#define SPIO_PRI64  SPIO_PRId64

#if SPIO_WIN64

/* Corresponds to a an argument of type spio_t_intptr or spio_t_uintptr */
#define SPIO_PRIdPTR SPIO_PRI64 
#define SPIO_PRIuPTR SPIO_PRIu64
#define SPIO_PRIxPTR SPIO_PRIx64
#define SPIO_PRIXPTR SPIO_PRIX64 

#else /* !SP_WIN64 */

/* All other platforms have sizeof (void*) == sizeof(long). Also see SPRIuPTR in sicstus.h. */

#if SPIO_INTPTR_T_IS_LONG
/* [PM] 4.2.2 Some 32-bit platforms (Mac OS X 10.{5,6,7,8}) have int
   == long but intptr_t == long instead of the more common intptr_t ==
   int. */
#define SPIO_PRI_PREFIX "l"
#elif SPIO_PTR_BIT == SPIO_WORD_BIT /* PTR == INT, i.e. 32-bit */
#define SPIO_PRI_PREFIX /* empty */
#else                   /* 64-bit */
#define SPIO_PRI_PREFIX "l"
#endif  /* 64-bit */


/* Corresponds to a an argument of type spio_t_intptr or spio_t_uintptr */
#define SPIO_PRIdPTR SPIO_PRI_PREFIX "d"
#define SPIO_PRIuPTR SPIO_PRI_PREFIX "u"
#define SPIO_PRIxPTR SPIO_PRI_PREFIX "x"
#define SPIO_PRIXPTR SPIO_PRI_PREFIX "X"

#endif  /* !SP_WIN64 */
#define SPIO_PRIPTR SPIO_PRIdPTR

/* [PM] 4.2.1+ <n>LL long long constants is OK here. */
#define SPIO_INT64_MAX (SPIO_GCC__EXTENSIONS__ 9223372036854775807LL) /* (2**63)-1 */
#define SPIO_INT64_MIN (SPIO_GCC__EXTENSIONS__ (-SPIO_INT64_MAX - 1LL))

#if _MSC_VER >= 1300            /* Visual Studio .NET */
#define SPIO__w64 __w64
#else
#define SPIO__w64               /* empty */
#endif

/* not all compilers have ssize_t yet */
#if SPIO_HAVE_SSIZE_T
typedef ssize_t spio_t_ssize;
#if defined SSIZE_MAX
#define SPIO_SSIZE_MAX SSIZE_MAX
#endif /* defined SSIZE_MAX */
#else  /* !SPIO_HAVE_SSIZE_T */

#if SPIO_WIN32

#if defined(_WIN64)
typedef __int64 spio_t_ssize;
#define SPIO_SSIZE_MAX SPIO_INT64_MAX
#else  /* !_WIN64 */
typedef SPIO__w64 long spio_t_ssize;
#if defined LONG_MAX
#define SPIO_SSIZE_MAX LONG_MAX
#endif /* defined LONG_MAX */
#endif  /* !_WIN64 */

#else  /* !SPIO_WIN32 */

typedef long spio_t_ssize;      /* de facto OK on all sane (non-Win64) platforms */
#if defined LONG_MAX
#define SPIO_SSIZE_MAX LONG_MAX
#endif /* defined LONG_MAX */

#if SPIO_DEBUG
#error "no ssize_t"             /* but we expect all sane, non-Windows, platforms to have ssize_t  */
#endif  /* SPIO_DEBUG */

#endif  /* !SPIO_WIN32 */


#endif  /* !SPIO_HAVE_SSIZE_T */

#define SPIO_SSIZE_MIN ((spio_t_ssize) ((-SPIO_SSIZE_MAX) - ((spio_t_ssize)-1)))

/* [PM] 4.2.1 spio_t_intptr Should be the same as intptr_t (which is not available 
   everywhere). An educated guess is that ssize_t is the same
   thing. */
#if SPIO_HAVE_INTPTR_T
typedef intptr_t spio_t_intptr;
#else /* !SPIO_HAVE_INTPTR_T */
typedef spio_t_ssize spio_t_intptr;
#endif /* !SPIO_HAVE_INTPTR_T */

#if SPIO_HAVE_UINTPTR_T
typedef uintptr_t spio_t_uintptr;
#else /* !SPIO_HAVE_UINTPTR_T */
typedef size_t spio_t_uintptr;
#endif /* !SPIO_T_UINTPTR */


SPIO_COMPILETIME_ASSERT_DECLARATION((sizeof(void*) == sizeof(spio_t_intptr)));
SPIO_COMPILETIME_ASSERT_DECLARATION((sizeof(void*) == sizeof(spio_t_uintptr)));

typedef spio_t_int64 spio_t_offset; /* seek (file-) offset */
#define SPIO_OFFSET_MAX SPIO_INT64_MAX
#define SPIO_OFFSET_MIN SPIO_INT64_MIN

typedef spio_t_int32 spio_t_refcount;


#if SPIO_WIN32
typedef spio_t_uint32 spio_t_DWORD;
typedef void *spio_t_HANDLE;
#if SPIO_INCLUDE_OS_TYPES
SPIO_COMPILETIME_ASSERT_DECLARATION(sizeof(spio_t_DWORD) == sizeof(DWORD));
SPIO_COMPILETIME_ASSERT_DECLARATION(sizeof(spio_t_HANDLE) == sizeof(HANDLE));
typedef HANDLE spio_t_os_file_handle;
typedef HANDLE spio_t_os_process_handle;
typedef DWORD spio_t_pid;
typedef DWORD spio_t_thread_id;
#else  /* !SPIO_INCLUDE_OS_TYPES */
typedef spio_t_HANDLE spio_t_os_file_handle;
typedef spio_t_HANDLE spio_t_os_process_handle;
typedef spio_t_DWORD spio_t_pid;
typedef spio_t_DWORD spio_t_thread_id;
#endif  /* !SPIO_INCLUDE_OS_TYPES */

#if SPIO_INCLUDE_OS_TYPES
#define SPIO_INVALID_OS_FILE_HANDLE ((spio_t_os_file_handle)INVALID_HANDLE_VALUE)
#define SPIO_INVALID_OS_PROCESS_HANDLE ((spio_t_os_process_handle)INVALID_HANDLE_VALUE)
#endif  /* SPIO_INCLUDE_OS_TYPES */

#elif SPIO_UNIX
typedef int spio_t_os_file_handle; /* file descriptor */
typedef pid_t spio_t_os_process_handle; /* pid_t always defined (in unistd.h) */
#define SPIO_INVALID_OS_FILE_HANDLE ((spio_t_os_file_handle) -1)
#define SPIO_INVALID_OS_PROCESS_HANDLE ((spio_t_os_process_handle) -1)

typedef pid_t spio_t_pid;
typedef void *spio_t_thread_id;

#endif  /* SPIO_UNIX */

/* flag bits for flags and options */
typedef spio_t_uint32 spio_t_bits;
#define SPIO_BITS_MAX SPIO_UINT32_MAX

typedef spio_t_uint16 spio_t_bits16;

typedef spio_t_uint32 spio_t_wchar; /* UNICODE char etc */
typedef spio_t_int32 spio_t_wcharint; /* Room for spio_t_wchar and negative (e.g., EOF) values.  */

typedef spio_t_uint8 spio_t_utf8;
typedef spio_t_uint16 spio_t_utf16;
typedef spio_t_uint32 spio_t_utf32;
typedef spio_t_int32 spio_t_utf32int;

/* Like (double) SPIO_INT64_MAX, but ensures there are no rounding problems.

   These constants are exact powers of two, so they can be exactly
   representable as IEEE floating point.
*/
#define SPIO_INT64_EXCLUSIVE_MAX_AS_DOUBLE (9223372036854775808.0) /* (2^63) (i.e. 0x1p+63) */
#define SPIO_INT64_INCLUSIVE_MIN_AS_DOUBLE (-SPIO_INT64_EXCLUSIVE_MAX_AS_DOUBLE)

/* Use this to protect against undefined behaviour when casting a
   out-of-range double to a spio_t_int64.

   Note that the obvious (((double)SPIO_INT64_MIN) <= (X) && (X) <=
   ((double)SPIO_INT64_MAX)) WILL NOT WORK, see
   e.g. https://stackoverflow.com/a/30424410/22676 for a discussion.
*/
#define SPIO_DOUBLE_FITS_IN_INT64(X) (SPIO_INT64_INCLUSIVE_MIN_AS_DOUBLE <= (X) && (X) < SPIO_INT64_EXCLUSIVE_MAX_AS_DOUBLE)


typedef spio_t_int64 spio_t_time; /* seconds since the (POSIX-)Epoch UTC */

#define SPIO_TIME_MAX SPIO_INT64_MAX
#define SPIO_TIME_MIN SPIO_INT64_MIN
/* Use this to protect against undefined behaviour when casting a
   out-of-range double to a spio_t_time. */
#define SPIO_DOUBLE_FITS_IN_TIME(X) SPIO_DOUBLE_FITS_IN_INT64((X))

typedef struct spio_t_timespec_ spio_t_timespec; /* for things like timeout */
struct spio_t_timespec_ {
  spio_t_time sec;		/* Seconds.  */
  spio_t_time nsec;		/* Nanoseconds. (64-bit is overkill but 32-bit is not more performant on 64-bit) */
};


#define SPIO_BITS_(X) ((spio_t_bits)(X))
/* VARIANT */
enum spio_t_variant_type_ {
  SPIO_VARIANT_ILLEGAL=0,
#define SPIO_VARIANT_ILLEGAL SPIO_BITS_(SPIO_VARIANT_ILLEGAL)
  SPIO_VARIANT_END=1,           /* marks end of vector of properties (e.g., spio_t_arglist) */
#define SPIO_VARIANT_END SPIO_BITS_(SPIO_VARIANT_END)
  SPIO_VARIANT_STRING,
#define SPIO_VARIANT_STRING SPIO_BITS_(SPIO_VARIANT_STRING)
  SPIO_VARIANT_POINTER,
#define SPIO_VARIANT_POINTER SPIO_BITS_(SPIO_VARIANT_POINTER)
  SPIO_VARIANT_FLAGS,
#define SPIO_VARIANT_FLAGS SPIO_BITS_(SPIO_VARIANT_FLAGS)
  SPIO_VARIANT_INT,
#define SPIO_VARIANT_INT SPIO_BITS_(SPIO_VARIANT_INT)
  SPIO_VARIANT_INT64,
#define SPIO_VARIANT_INT64 SPIO_BITS_(SPIO_VARIANT_INT64)

#if 0                           /* not for now */
  SPIO_VARIANT_BOOL,
  /* #define SPIO_VARIANT_BOOL SPIO_VARIANT_BOOL */ 
#define SPIO_VARIANT_BOOL SPIO_BITS_(SPIO_VARIANT_BOOL)
#endif
  SPIO_VARIANT_FIRST_UNUSED
};
#define SPIO_VARIANT_TYPE_MASK ((spio_t_bits)0x07) /* at most 7 elements in spio_t_variant_type_ (must have all bits set!) */
SPIO_COMPILETIME_ASSERT_DECLARATION(SPIO_VARIANT_FIRST_UNUSED <= SPIO_VARIANT_TYPE_MASK+1);

#define SPIO_VARIANT_TYPE(PVARIANT) ((spio_t_variant_type)((PVARIANT)->flags & SPIO_VARIANT_TYPE_MASK))
#if 0
/* requires SPIO_VARIANT_INIT already done */
#define SPIO_VARIANT_SET_TYPE(PVARIANT, TYPE) (((PVARIANT)->flags &= ~SPIO_VARIANT_TYPE_MASK), ((PVARIANT)->flags |= (TYPE)))
#endif  /* 0 */
#define SPIO_VARIANT_INIT_TYPE(VARIANT, TYPE) (((VARIANT).flags = (TYPE)))

#define SPIO_VARIANT_INIT(VARIANT) SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_ILLEGAL)
#define SPIO_VARIANT_INIT_STATIC_STRING(VARIANT, STRING) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_STRING), SPIO_SET_MASK((VARIANT).flags, SPIO_VARIANT_FLAG_STATIC_DATA), (VARIANT).u.string = (STRING))
#define SPIO_VARIANT_INIT_STRING(VARIANT, STRING) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_STRING), (VARIANT).u.string = (STRING))
#define SPIO_VARIANT_INIT_POINTER(VARIANT, PTR) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_POINTER), (VARIANT).u.p = (void*)(PTR))
#define SPIO_VARIANT_INIT_END(VARIANT) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_END), (VARIANT).u.p = NULL)

/* cannot forward declare enums (duh!) */
typedef enum spio_t_variant_type_ spio_t_variant_type;

typedef struct spio_t_variant_ spio_t_variant;
struct spio_t_variant_ {
#define SPIO_VARIANT_FLAG_DYNAMIC (SPIO_VARIANT_TYPE_MASK+1) /* variant struct is spio_alloc-ated and should be spio_free-d  */
#define SPIO_VARIANT_FLAG_STATIC_DATA SPIO_NEXT_BIT(SPIO_VARIANT_FLAG_DYNAMIC) /* variant u.string should NOT be spio_free-d  */

  spio_t_bits flags;
  union {
    char const *string;         /* SPIO_VARIANT_STRING */
    spio_t_int64 i64;           /* SPIO_VARIANT_INT64 */
    void const *p;              /* SPIO_VARIANT_POINTER (SPIO_VARIANT_END) */
    spio_t_bits flags;          /* SPIO_VARIANT_FLAGS */
    int i;                      /* SPIO_VARIANT_INT */
#ifdef SPIO_VARIANT_BOOL
    int b;                      /* SPIO_VARIANT_BOOL */
#endif
  } u;
};

/* KEYED PROPERTIES */
typedef struct spio_t_property_ spio_t_property;
struct spio_t_property_ {
  char const *key;
  spio_t_variant v;
};


#define SPIO_PROPERTY_INITIALIZER_END { NULL, { SPIO_VARIANT_END, { NULL } } }
#define SPIO_PROPERTY_INIT_END(PROPERY) do { (PROPERY).key = NULL; SPIO_VARIANT_INIT_END((PROPERY).v); }while(0)
#define SPIO_PROPERTY_INITIALIZER_STATIC_(KEY, TYPE, VALUE) { (KEY), { ((TYPE) | SPIO_VARIANT_FLAG_STATIC_DATA), { (VALUE) } } }
#if 1
#define SPIO_PROPERTY_INITIALIZER_STATIC_STRING(KEY, STRING) SPIO_PROPERTY_INITIALIZER_STATIC_((KEY), SPIO_VARIANT_STRING, (STRING))
#else
#define SPIO_PROPERTY_INITIALIZER_STATIC_STRING(KEY, STRING) { (KEY), { (SPIO_VARIANT_STRING | SPIO_VARIANT_FLAG_STATIC_DATA), { (STRING) } } }
#endif
#define SPIO_PROPERTY_INITIALIZER_STATIC_POINTER(KEY, PTR) SPIO_PROPERTY_INITIALIZER_STATIC_((KEY), SPIO_VARIANT_POINTER, (PTR))

typedef spio_t_property *spio_t_arglist;

/* Return values for spio_{os}_device_type() */
enum spio_t_device_type_ {
  SPIO_DEVICE_TYPE_ILLEGAL = 0,
  SPIO_DEVICE_TYPE_UNKNOWN,
  SPIO_DEVICE_TYPE_FILE,
  SPIO_DEVICE_TYPE_TTY,
#define SPIO_DEVICE_TYPE_CONSOLE SPIO_DEVICE_TYPE_TTY /* WIN32 NAME */
  SPIO_DEVICE_TYPE_SOCKET,
  SPIO_DEVICE_TYPE_FIFO,
#define SPIO_DEVICE_TYPE_PIPE SPIO_DEVICE_TYPE_FIFO /* WIN32 NAME */
  SPIO_DEVICE_TYPE_CHARACTER,
#define SPIO_DEVICE_TYPE_SERIAL SPIO_DEVICE_TYPE_CHARACTER /* WIN32 NAME */

  SPIO_DEVICE_TYPE_DIRECTORY,   /* UNIX only */
  SPIO_DEVICE_TYPE_BLOCK,       /* UNIX only */
  SPIO_DEVICE_TYPE_LINK,        /* UNIX only */

  SPIO_DEVICE_TYPE_FIRST_UNUSED_,
  SPIO_DEVICE_TYPE_LAST = SPIO_DEVICE_TYPE_FIRST_UNUSED_-1
};
typedef enum spio_t_device_type_ spio_t_device_type;

typedef struct spio_t_dirent_ spio_t_dirent;
typedef struct spio_t_dir_ spio_t_dir;

enum spio_t_init_option_type_ {
  SPIO_INIT_OPTION_TYPE_INVALID=0,
  SPIO_INIT_OPTION_TYPE_NULL=1,
  /* [PM] 4.1.3 the spio_t_init_option type is a spio_t_init_option_system_property */
  SPIO_INIT_OPTION_TYPE_SYSTEM_PROPERTY=2
};
typedef enum spio_t_init_option_type_ spio_t_init_option_type;

/* type=SPIO_INIT_OPTION_TYPE_SYSTEM_PROPERTY */
typedef struct spio_t_init_option_system_property_ spio_t_init_option_system_property;
struct spio_t_init_option_system_property_ {
  char const *key;              /* SICStus internal encoding (UTF-8) */
  char const *value;            /* SICStus internal encoding (UTF-8) */
};

typedef struct spio_t_init_option_ spio_t_init_option;
struct spio_t_init_option_ {
  spio_t_init_option_type type;
  union spio_t_init_option_u_ {
    void *reserved;
    spio_t_init_option_system_property prop;
  } u;
};
#define SPIO_INIT_OPTION_TYPE_DEFINED 1

typedef struct spio_t_init_options_ spio_t_init_options;
struct spio_t_init_options_ {
#define SPIO_INIT_OPTIONS_VERSION 1 /* SP 4.1.3 */
  unsigned long version;
  size_t noptions;
  spio_t_init_option *options; /* noptions elements */
};


#include "spio_errors.h"

#endif  /* SPIO_TYPES_H_INCLUDED */
