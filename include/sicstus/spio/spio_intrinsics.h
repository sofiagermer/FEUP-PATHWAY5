#ifndef SPIO_INTRINSICS_H_INCLUDED
#define SPIO_INTRINSICS_H_INCLUDED

/*
  Bit-fiddling and related compiler intrinsics.

  NOTE: Some intrinsics are undefined for some inputs (e.g. if the
  argument is zero). See their documentation, below, for details.

  NOTE: Only the lowercase names, starting with "spio_" should be used
  by clients outside of this file. I.e. use spio_msb32(), never
  SPIO_MSB32_().

  Not all intrinsics are implemented on all platforms. Implemented
  intrics will have their name defined as a macro, which can be used
  for testing availability.

  NOTE: If you want to use these intrinsics from foreign code,
  e.g. library(clpfd), define SPIO_FORCE_INLINE_INTRINSICS to 1 before
  including any files.

*/

#include "spio_config.h"
#include "spio_types.h"
#include "spio_debug.h"

#include <limits.h> /* Defines INT_WIDTH et al. in GCC */
#if SPIO_HAVE_INTTYPES_H
#include <inttypes.h>		/* uintptr_t */
#endif /* SPIO_HAVE_INTTYPES_H */

#if defined(_MSC_VER)	       /* Microsoft Visual Studio compilers */
#include <intrin.h>
#endif	/* defined(_MSC_VER) */

#if defined(_MSC_VER)	       /* Microsoft Visual Studio compilers */
#pragma warning( push )

/* We may need to suppress warnings about unused functions (but I
   think unused inlined functions are ignored, by default). */
/* #pragma warning( disable : XXXX ) */
#endif	/* defined(_MSC_VER) */


/************************************ NO CODE BEFORE THIS POINT ******************************************/

/*
  Define SPIO_FORCE_INLINE_INTRINSICS to 1 before including this file
  in order to ensure that the defined intrinsics will be usable
  without runtime support. This is needed when using the intrinsics
  from a foreign resource.
*/
#if SPIO_FORCE_INLINE_INTRINSICS
#undef SPIO_DEBUG_INTRINSICS

/* SPIO_DEBUG_INTRINSICS will generate calls to debug versions that
   are not available from outside the runtime. In particular, the
   debug versions are not available from foreign resources, like
   clpfd. */
#define SPIO_DEBUG_INTRINSICS 0
#endif	/* SPIO_FORCE_INLINE_INTRINSICS */

#if !defined(SPIO_DEBUG_INTRINSICS)
#if SPIO_DEBUG
#define SPIO_DEBUG_INTRINSICS 1
#endif	/* SPIO_DEBUG */
#endif /* !defined(SPIO_DEBUG_INTRINSICS) */

/*
  int spio_count_leading_zeros32(spio_t_uint32 x)
  int spio_count_leading_zeros64(spio_t_uint64 x)

  Compute the number of leading zeros. If the argument is zero, the
  result is the argument width (32 or 64).

  These are inlined, and it is expected that they are slightly more
  efficient if the compiler can determine that the argument is
  non-zero.

  If they are available, they are also defined as macros.
*/


#if defined(__GNUC__) || defined(__clang__)

/*
  NOTE: The GCC/CLANG CLZ intrinsics are undefined for zero argument.
*/

#if LONG_BIT == 32
#define SPIO_CLZ32_(X) __builtin_clzl((X))
#elif WORD_BIT == 32
#define SPIO_CLZ32_(X) __builtin_clz((X))
#endif

#if LONG_BIT == 64
#define SPIO_CLZ64_(X) __builtin_clzl((X))
#elif LONG_BIT == 32
/* long long must be at least 64-bit but presumably it is exactly 64-bit if long is 32-bit. */
#define SPIO_CLZ64_(X) __builtin_clzll((X))
#endif

#if defined(SPIO_CLZ32_)
static SPIO_INLINE int spio_count_leading_zeros32(spio_t_uint32 x) {
  if (SPIO_UNLIKELY(x == 0)) {
    return 32;
  }
  return SPIO_CLZ32_(x);
}
#define spio_count_leading_zeros32 spio_count_leading_zeros32
#else  /* !defined(SPIO_CLZ32_) */
/* FIXME: Implement a fallback. */
#endif	/* defined(SPIO_CLZ32_) */

#if defined(SPIO_CLZ64_)
static SPIO_INLINE int spio_count_leading_zeros64(spio_t_uint64 x) {
  if (SPIO_UNLIKELY(x == 0)) {
    return 64;
  }
  return SPIO_CLZ64_(x);
}
#define spio_count_leading_zeros64 spio_count_leading_zeros64
#else  /* !defined(SPIO_CLZ64_) */
/* FIXME: Implement a fallback. */
#endif	/* defined(SPIO_CLZ64_) */

/* The CLZ intrinsic gives undefined result for zero argument. We do
   not accept zero argument here, so CLZ is OK. */
#if defined(SPIO_CLZ32_)
#define SPIO_MSB32_(X) ((32 - 1) - SPIO_CLZ32_((X)))
#endif	/* defined(SPIO_CLZ32_) */
#if defined(SPIO_CLZ64_)
#define SPIO_MSB64_(X) ((64 - 1) - SPIO_CLZ64_((X)))
#endif	/* defined(SPIO_CLZ64_) */

#elif defined(_MSC_VER)		/* Microsoft Visual Studio compilers */


#if defined(_M_IX86) || defined(_M_AMD64) /* _BitScanReverse is x86/x64 only */
/* TODO: This can use __lzcnt once we know LZCNT is available everywhere (Haswell) */
static SPIO_INLINE int spio_count_leading_zeros32(spio_t_uint32 x) {
  unsigned long index;
  if (SPIO_UNLIKELY(!_BitScanReverse(&index, x))) {
    /* x == 0 */
    return 32;
  }
  return (32 - 1) - (int) index;
}
#define spio_count_leading_zeros32 spio_count_leading_zeros32
#endif	/* defined(_M_IX86) || defined(_M_AMD64) */

#if defined(_M_AMD64)		/* _BitScanReverse64 is x64 only */
/* TODO: This can use __lzcnt64 once we know LZCNT is available everywhere (Haswell) */
static SPIO_INLINE int spio_count_leading_zeros64(spio_t_uint64 x) {
  unsigned long index;
  if (SPIO_UNLIKELY(!_BitScanReverse64(&index, x))) {
    /* x == 0 */
    return 64;
  }
  return (64 - 1) - (int) index;
}
#define spio_count_leading_zeros64 spio_count_leading_zeros64
#endif	/* defined(_M_AMD64) */

/* If CPU is older than Haswell __lzcnt() will do BSR which counts
   from LSB instead of from MSB, so gives wrong results.

   So, instead fallback to the generic code (which should be inlined
   to a BSR and a subtraction).
*/
#if 0
#if _MSC_VER >= 1800 && (defined(_M_IX86) || defined(_M_AMD64)) /* __lzcnt is x86/x64 only and >= VS 2013 */
#define SPIO_MSB32_(X) ((32 - 1) - (int)__lzcnt((X)))
#endif	/* defined(_M_IX86) || defined(_M_AMD64) */

#if _MSC_VER >= 1800 && defined(_M_AMD64)		/* __lzcnt64 is x64 only and >= VS 2013 */
#define SPIO_MSB64_(X) ((64 - 1) - (int)__lzcnt64((X)))
#endif	/* defined(_M_AMD64) */
#endif	/* 0 */

#endif	/* defined(_MSC_VER) */

/*
  int spio_msb32(spio_t_uint32 x)
  int spio_msb64(spio_t_uint64 x)

  Compute the zero-based index of the first set 1-bit, counting from
  the right.  IF NO BIT IS SET, THE RESULT IS UNDEFINED.

  NOTE: IF THE ARGUMENT IS ZERO, THE RESULT IS UNDEFINED.

  Examples:

  spio_msb32(0x0A) == SPIO_MSB64(0x0A) == 3
  spio_msb32(-1) == 31
  spio_msb64(-1) == 63

  spio_msb32(0) == will make "demons fly out of your nose."
*/

#if !defined(SPIO_MSB32_)
#if SPIO_DEBUG && !FORCE_BUILD && !defined(_MSC_VER) /* On Windows we currently need the fallback. */
#error "Expected SPIO_MSB32_ to be defined."
#endif	/* !FORCE_BUILD */

#define SPIO_MSB32_(X) ((32 - 1) - spio_count_leading_zeros32((X)))
#endif	/* !defined(SPIO_MSB32_) */

#if !defined(SPIO_MSB64_)

#if SPIO_DEBUG && !FORCE_BUILD && !defined(_MSC_VER) /* On Windows we currently need the fallback. */
#error "Expected SPIO_MSB64_ to be defined on 64-bit platforms."
#endif	/* !FORCE_BUILD */

#if defined(spio_count_leading_zeros64)
#define SPIO_MSB64_(X) ((64 - 1) - spio_count_leading_zeros64((X)))
#endif	/* defined(spio_count_leading_zeros64) */

#endif	/* !defined(SPIO_MSB64_) */

#if SPIO_DEBUG_INTRINSICS

#if defined(SPIO_MSB32_)
#define spio_msb32(X) spio_msb32_debug((X))
#endif	/* defined(SPIO_MSB32_) */

#if defined(SPIO_MSB64_)
#define spio_msb64(X) spio_msb64_debug((X))
#endif	/* defined(SPIO_MSB64_) */

#else  /* !SPIO_DEBUG_INTRINSICS */

#if defined(SPIO_MSB32_)
#define spio_msb32(X) SPIO_MSB32_((X))
#endif	/* defined(SPIO_MSB32_) */

#if defined(SPIO_MSB64_)
#define spio_msb64(X) SPIO_MSB64_((X))
#endif	/* defined(SPIO_MSB64_) */

#endif	/* !SPIO_DEBUG_INTRINSICS */


/*
  int spio_count_trailing_zeros32(spio_t_uint32 x)
  int spio_count_trailing_zeros64(spio_t_uint64 x)

  Compute the number of trailing zeros. If the argument is zero, the
  result is the argument width (32 or 64).

  These are inlined, and it is expected that they are slightly more
  efficient if the compiler can determine that the argument is
  non-zero.
*/

#if defined(__GNUC__) || defined(__clang__)

#define SPIO_BSF_int_(X) __builtin_ctz((X))
#define SPIO_BSF_long_(X) __builtin_ctzl((X))
#define SPIO_BSF_long_long_(X) __builtin_ctzll((X))

#if LONG_BIT == 32
#define SPIO_BSF32_(X) SPIO_BSF_long_((X))
#elif WORD_BIT == 32
#define SPIO_BSF32_(X) SPIO_BSF_int_((X))
#endif

#if LONG_BIT == 64
#define SPIO_BSF64_(X) SPIO_BSF_long_((X))
#elif LONG_BIT == 32
/* long long must be at least 64-bit but presumably it is exactly 64-bit if long is 32-bit. */
#define SPIO_BSF64_(X) SPIO_BSF_long_long_((X))
#endif

static SPIO_INLINE int spio_count_trailing_zeros32(spio_t_uint32 x) {
  if (SPIO_UNLIKELY(x == 0)) {
    return 32;
  }
  return SPIO_BSF32_(x);
}
#define spio_count_trailing_zeros32 spio_count_trailing_zeros32

static SPIO_INLINE int spio_count_trailing_zeros64(spio_t_uint64 x) {
  if (SPIO_UNLIKELY(x == 0)) {
    return 64;
  }
  return SPIO_BSF64_(x);
}
#define spio_count_trailing_zeros64 spio_count_trailing_zeros64

#elif defined(_MSC_VER)		/* Microsoft Visual Studio compilers */

#if defined(_M_IX86) || defined(_M_AMD64) /* _BitScanForward is x86/x64 only */
static SPIO_INLINE int spio_count_trailing_zeros32(spio_t_uint32 x) {
  unsigned long index;
  if (SPIO_UNLIKELY(!_BitScanForward(&index, x))) {
    /* x == 0 */
    return 32;
  }    
  return (int) index;
}
#define spio_count_trailing_zeros32 spio_count_trailing_zeros32
#endif	/* defined(_M_IX86) || defined(_M_AMD64) */

#if defined(_M_AMD64)		/* _BitScanForward64 is x64 only */
static SPIO_INLINE int spio_count_trailing_zeros64(spio_t_uint64 x) {
  unsigned long index;
  if (SPIO_UNLIKELY(!_BitScanForward64(&index, x))) {
    /* x == 0 */
    return 64;
  }
  return (int) index;
}
#define spio_count_trailing_zeros64 spio_count_trailing_zeros64
#endif	/* defined(_M_AMD64) */

#endif	/* definded(_MSC_VER) */



/************************************ NO CODE AFTER THIS POINT ******************************************/


#if defined(_MSC_VER)	       /* Microsoft Visual Studio compilers */
#pragma warning( pop )
#endif	/* defined(_MSC_VER) */

#endif /* SPIO_INTRINSICS_H_INCLUDED */
