#ifndef SPIO_MEMOPS_H_INCLUDED
#define SPIO_MEMOPS_H_INCLUDED 1
/* [PM] 4.2 memory primitives */

#include "spio_config.h"

#if __APPLE__
#if HAVE_STDATOMIC_H
/* [PM] 4.3.4 atomic_thread_fence() */
#include <stdatomic.h>
#else  /* !HAVE_STDATOMIC_H */
/* OSMemoryBarrier() */
/* [PM] 4.3.4 Must define OSATOMIC_DEPRECATED=0 to suppress deprecation warning for OSMemoryBarrier in macOS 10.12. */
#include <libkern/OSAtomic.h>
#endif	/* !HAVE_STDATOMIC_H */
#endif  /* __APPLE__ */

#if (_WIN32 || WIN32 || _WIN64)
/* Includes winnt.h via windows.h or similar */
#include "spio_win32.h"
#endif  /* (_WIN32 || WIN32 || _WIN64) */

#ifndef SPIO_HAVE_GCC__SYNC_SYNCHRONIZE
#if ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 1))) /* GCC >= 4.1.0 */
/* has __sync_synchronize() */
#define SPIO_HAVE_GCC__SYNC_SYNCHRONIZE 1
#endif  /* GCC >= 4.1 */
#endif /* SPIO_HAVE_GCC__SYNC_SYNCHRONIZE */

/* Define SPIO_CompilerMemoryBarrier_() which tells the compiler to not
   move memory operations past either earlier or later. Does not (at
   least are not guaranteed to) prevent CPU re-orderings. 

   Must be async signal safe.

   You should probably use SPIO_CompilerAndCPUMemoryBarrier() instead.
*/

/* Microsoft Compilers */
#ifndef SPIO_CompilerMemoryBarrier_
#ifdef _MSC_VER
/* [PM] 4.2.1 Work around bug in VS 2005 when compiling without
   optimization.  See
   <https://developer.mozilla.org/En/Developer_Guide/Build_Instructions/Intrin.h>
 */
#if _MSC_VER <= 1400            /* VS 2005 */
#define _interlockedbittestandreset SP_interlockedbittestandreset_NAME_CHANGED_TO_AVOID_MSVS2005_ERROR
#define _interlockedbittestandset SP_interlockedbittestandset_NAME_CHANGED_TO_AVOID_MSVS2005_ERROR
#endif /* _MSC_VER <= 1400 */

#include <intrin.h>

/*
  [PM] 4.2.1 Work around bug in VS 2005 causing undefined symbol
  linker errors.  See
  <https://developer.mozilla.org/En/Developer_Guide/Build_Instructions/Intrin.h>
  and
  <http://connect.microsoft.com/VisualStudio/feedback/details/100051>
 */
#if _MSC_VER <= 1400            /* VS 2005 */
#pragma intrinsic(_ReadWriteBarrier)
#endif  /* _MSC_VER <= 1400 */
#define SPIO_CompilerMemoryBarrier_() do{       \
  _ReadWriteBarrier();                          \
} while(0)
#endif  /* _MSC_VER */
#endif  /* SPIO_CompilerMemoryBarrier_ */

#ifndef SPIO_CompilerMemoryBarrier_
#ifdef __SUNPRO_C
#if __SUNPRO_C<=0x580
 /* [PM] 4.2 Sun Studio 11 (Sun C 5.8), which we use on Solaris 8, do not support asm volatile syntax */
#define SPIO_CompilerMemoryBarrier_() do {      \
  /* nothing */                                 \
  } while(0)
#else  /* > 5.8, e.g. Solaris Studio 12 */
/* [PM] 4.2 SunPro, a.k.a. Oracle Solaris Studio. */
#define SPIO_CompilerMemoryBarrier_() do {      \
  /* mbarrier.h __compiler_barrier() */                    \
  asm volatile("":::"memory");                  \
  } while(0)
#endif /* > Sun C 5.8 */
#endif  /* __SUNPRO_C */
#endif  /* SPIO_CompilerMemoryBarrier_ */

/* GCC and compatible (do this after all other compilers since there are liars out there!) */
#ifndef SPIO_CompilerMemoryBarrier_
#if defined(__GNUC__)           /* claims to be GCC */
#define SPIO_CompilerMemoryBarrier_() do{       \
  __asm__ __volatile__ ("" : : : "memory");     \
} while(0)
#endif    /* __GNUC__ */
#endif  /* !SPIO_CompilerMemoryBarrier_ */


/* Define SPIO_CPUMemoryBarrier_() which provides a full CPU memory
   barrier. Does not (at least are not guaranteed to) prevent compiler
   re-orderings.

   Must be async signal safe.

   You should probably use SPIO_CompilerAndCPUMemoryBarrier() instead.
*/

/* Mac OS X, any compiler  */
#ifndef SPIO_CPUMemoryBarrier_
#if __APPLE__
#if HAVE_STDATOMIC_H
/* [PM] 4.3.4 Xcode 8, macOS 10.12 deprecates OSMemoryBarrier() in
   preference to the stdatomic.h routines. With
   -DOSATOMIC_USE_INLINED=1 OSMemoryBarrier() turns into a static
   inlined procedure, with body
   "atomic_thread_fence(memory_order_seq_cst);" (see
   /usr/include/libkern/OSAtomicDeprecated.h), which we duplicate
   here.  Presumably we could use this on all platforms that have
   stdatomic.h.

   Apple LLVM version 8.0.0 (clang-800.0.38) expands this to an mfence
   instruction, which should ensure the construct is async signal
   safe (we use mfence on other platforms).
 */
#define SPIO_CPUMemoryBarrier_() do{            \
  atomic_thread_fence(memory_order_seq_cst);	\
} while(0)
#endif	/* HAVE_STDATOMIC_H */
#endif /* __APPLE__ */
#endif  /* SPIO_CPUMemoryBarrier_ */

/* Mac OS X, any compiler  */
#ifndef SPIO_CPUMemoryBarrier_
#if __APPLE__
/* We always use GCC >= 4.2 so could use the GCC intrinsic but I do
   not know whether it is as trustworthy as the libkern routine. */
#define SPIO_CPUMemoryBarrier_() do{            \
  OSMemoryBarrier();                            \
} while(0)
#endif /* __APPLE__ */
#endif  /* SPIO_CPUMemoryBarrier_ */

/* Windows, any compiler */
#ifndef SPIO_CPUMemoryBarrier_
#if (_WIN32 || WIN32 || _WIN64)
#define SPIO_CPUMemoryBarrier_() do{            \
  MemoryBarrier();                              \
} while(0)
#endif  /* (_WIN32 || WIN32 || _WIN64)  */
#endif  /* SPIO_CPUMemoryBarrier_ */

/* Sun Studio */
#ifndef SPIO_CPUMemoryBarrier_
#ifdef __SUNPRO_C
/* [PM] 4.2 SunPro, a.k.a. Oracle Solaris Studio. */
#if defined(__i386) || defined(__x86_64)
#define SPIO_CPUMemoryBarrier_() do{            \
  /* mbarrier.h __machine_rw_barrier() */       \
  asm volatile("mfence":::"memory");            \
} while(0)
#elif defined (__sparc)
#if __SUNPRO_C<=0x580
 /* [PM] 4.2 Sun Studio 11 (Sun C 5.8), which we use on Solaris 8, do not support asm volatile syntax */
#define SPIO_CPUMemoryBarrier_() do{                                    \
  asm("membar #LoadStore | #StoreLoad | #LoadLoad | #StoreStore");      \
} while(0)
#else  /* > 5.8, e.g. Solaris Studio 12 */
#define SPIO_CPUMemoryBarrier_() do{                                    \
  /* mbarrier.h __machine_rw_barrier() */                               \
  asm volatile("membar #LoadStore | #StoreLoad | #LoadLoad | #StoreStore":::"memory"); \
} while(0)
#endif /* > Sun C 5.8 */
#endif  /* __sparc */
#endif  /* __SUNPRO_C */
#endif  /* !SPIO_CPUMemoryBarrier_ */


/* GCC and compatible */
#ifndef SPIO_CPUMemoryBarrier_
/* We unconditionally fall back to the GCC 4.1 intrinsic. The hope is
   that non-GCC compilers will be GCC compatible. */
#if SPIO_HAVE_GCC__SYNC_SYNCHRONIZE || 1
#define SPIO_CPUMemoryBarrier_() do{            \
  /* GCC 4.1, ... */                            \
  __sync_synchronize();                         \
} while(0)
#endif  /* SPIO_HAVE_GCC__SYNC_SYNCHRONIZE */
#endif  /* !SPIO_CPUMemoryBarrier_ */

/* Define SPIO_CompilerAndCPUMemoryBarrier() which provide a full
   Compiler and CPU memory barrier. I.e. neither the compiler nor the
   CPU will re-order read/write accesses past this. 

   Must be async signal safe.

   This is what you want to use.
*/

#ifndef SPIO_CompilerAndCPUMemoryBarrier
#define SPIO_CompilerAndCPUMemoryBarrier() do{  \
  SPIO_CompilerMemoryBarrier_();                \
  SPIO_CPUMemoryBarrier_();                     \
  SPIO_CompilerMemoryBarrier_();                \
} while(0)
#endif  /* !SPIO_CompilerAndCPUMemoryBarrier */

#endif /* SPIO_MEMOPS_H_INCLUDED */

