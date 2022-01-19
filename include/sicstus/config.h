/* config_template.h.  Generated from config.h.in by configure.  */
/* -*- Mode:C -*-
 * config.h.in:		Template file for config.h
 *
 * NOTE: Do *NOT* modify the contents of this file unless you know
 * exactly what you're doing. If you want to modify a flag or a
 * setting, use configure instead.
 * NOTE: This file must not include any other files. Ever!
 *       The reason is that it should be safe to include this file and
 *       then define feature macros (such as _XOPEN_SOURCE).
 */


/* [PM] 4.1 The ECLIPSE_CDT should only be defined when Eclipse CDT is indexing the code. It should never be defined when actually compiling */
#if ECLIPSE_CDT
/* Always let CDT see the whole file */
/* #undef SP_NO_PRIVATE_CONFIG */
#endif  /* ECLIPSE_CDT */

/* [PM] 3.9 sicstus.h defines SP_NO_PRIVATE_CONFIG 1 to prevent
   non-public defines from being seen by user code. */

/* Things that should be seen by user code (and SP run-time) that
   includes sicstus.h */
#if !SP_PUBLIC_CONFIG_INCLUDED
#define SP_PUBLIC_CONFIG_INCLUDED 1

/***
 ***
 *** Beginning of public section ****
 ***
 ***/

#define SICSTUS_MAJOR_VERSION 4
#define SICSTUS_MINOR_VERSION 7
#define SICSTUS_REVISION_VERSION 0
#define SICSTUS_BETA_VERSION 0

/* must be in public section for spio/config.h */
/* [PM] 4.0.5 true if this is a release build (this disables some debug things that SICSTUS_BETA_VERSION would ordinarily enable) */
#define SICSTUS_RELEASE_BUILD 1

#define RELEASE_YEAR 2021

/* [PM] 4.2.1 No public source code (headers and plain source) should
   depend on DBG. Instead SICSTUS_DBG should be used everywhere.

   Non-public source code may continue to use DBG, for now, but
   SICSTUS_DBG is preferable.

   [PM] 4.2.1 NOTE: We must maintain binary compatibility between
   debug and non-debug builds and between DBG/SICSTUS_DBG off and
   on. This is so that we can link non-debug (customer) code with a
   debug build of SICStus.
   
   So, be careful when using DBG/SICSTUS_DBG.
*/

#define SICSTUS_DBG 0
/* [PM] 4.2.1 an experiment */
#define SP_NEW_DBG_POLICY 1

/* [PD] 3.11.1 */
#define SICSTUS_VERSION_STRING "4.7.0"

/*
  
  In 3.7 (3.8) SICSTUS_VERSION was 37 (38) and there was no way to
  find out the revision/patch level. Prior to 3.7 there was no
  SICSTUS_VERSION defined at all.

  From 3.9 format changed to enable more than 100 minor releases, each
  with 100 patch releases.
  (In fact SICStus minor release is what others might call major releases.)

  XXYYZZ, 3.9.1 would be 030901
*/
/* #define SICSTUS_VERSION (((((SICSTUS_MAJOR_VERSION)*100)+(SICSTUS_MINOR_VERSION))*100)+(SICSTUS_REVISION_VERSION)) */
/* [PM] 4.0.5 Now a constant to allow token concatenation */
#define SICSTUS_VERSION 40700

/* (Optional) consistency check. */
#ifdef SICSTUS_TARGET_VERSION
#if SICSTUS_TARGET_VERSION != SICSTUS_VERSION
#error "SICStus version mismatch, SICSTUS_TARGET_VERSION!=SICSTUS_VERSION"
#endif
#endif  /* SICSTUS_TARGET_VERSION */

#define SP_GET_DISPATCH_NAME SP_get_dispatch_40700
#define SP_get_dispatch_STRINGIZE__(X) #X
#define SP_get_dispatch_STRINGIZE_(X) SP_get_dispatch_STRINGIZE__(X)
#define SP_GET_DISPATCH_NAME_STRING SP_get_dispatch_STRINGIZE_(SP_GET_DISPATCH_NAME)

/* [PM] 4.1.3 backward compatibility */
#define SP_get_dispatch_NAME_STRING SP_GET_DISPATCH_NAME_STRING
#define SP_get_dispatch_NAME SP_GET_DISPATCH_NAME

/* [PM] 4.0.5 SP_get_dispatch now has a version specific name */
#define SP_get_dispatch SP_get_dispatch_NAME

#define SP_OS_DATA_PLATFORM_NAME_STRING "linux"
#define SP_JIT_ABI_STRING "linuxx64"

/* [PM] 3.9.1 non-zero on platforms where loading multiple sicstuses into the same process does *not* work. */
#define SP_NO_MULTI_SP_FEATURE 0

/* [PM] 4.0.5 non-zero if we allow static foreign resources to use multi-sp dispatch mechanism */
#define SP_ALLOW_MULTI_STATIC 1

/* [PM] 4.2.1+ Whether splfr-generated glue code should handle calling convention. Needs to be in public part. */
#define SP_FLI_APPLY_ASM_GENERIC 0

/* [PM] 4.0 Use SPIO I/O package (always true in SP4) */
#define SICSTUS_SPIO 1

/* [PM] 4.0 True on OSes (like Win32) where user_main will be called
   with a argv containing UTF-8 strings. This also implies that the
   argv argument to SU_initialize etc should use UTF-8. Note that argv
   passed to SP_initialize should always use encoded (i.e. UTF-8)
   strings. Also see SP_set_argv. */
#define SP_USER_MAIN_ARGV_UTF8 0

/* [PM] 3.10.2 non-zero to make GetSICStusAPIProcInit the same as
   GetSICStusAPIProc. This requires the users to call
   SetupSICStusDISPATCH() explicitly before the first call to a
   SP_... routine. The only reason to have this non-zero is to work
   around a bug in the HPUX (ipf-hpux-B.11.22) C compiler (cc -V says:
   cc: HP aC++/ANSI C B3910B A.05.41 [Nov 1 2002]) */
#ifndef SP_INHIBIT_IMPLICIT_APIPROCINIT /* Allow users to redefine it from cc command line */
#define SP_INHIBIT_IMPLICIT_APIPROCINIT 0
#endif /* SP_INHIBIT_IMPLICIT_APIPROCINIT */

/* [PM] Calling convention for public API. Empty except on Windows where it is __cdecl */
#define SPCDECL 

/* [PM] 4.0 Are we using (the GCC 4.0 feature) -fvisibility=hidden? */
#define SP_USE_GCC_VISIBILITY 1

/* [PM] 3.9b4 The line following this marker is filled in with great
   effort by Emulator/Makefile */
/* @PUBLIC_CONFIGURE_THINGS_FOR_CONFIG_H@ */

#define SP_SIG_IGN ((SP_SigFun*)(void*)SIG_IGN)
#define SP_SIG_DFL ((SP_SigFun*)(void*)SIG_DFL)
#define SP_SIG_ERR ((SP_SigFun*)(void*)SIG_ERR)
#define SP_SIGINT SIGINT
#define SP_SIGQUIT SIGQUIT
#define SP_SIGBREAK SIGBREAK


/* [PM] 3.9.2 TERMINATIONDATE is now defined as zero if it should be ignored */
#define TERMINATIONDATE 0

/* used by spio_config.h */
#define SP_BIGENDIAN 0

#define SP_SIZEOF_INT 4
#define SP_SIZEOF_LONG 8
#define SP_SIZEOF_VOID_P 8


#define SP_WIN32 0
/* [PM] 4.2.1+ 64-bit windows.
   SP_WIN64 is only true if SP_WIN32 is. */
#define SP_WIN64 0

/***
 ***
 *** End of public section ****
 ***
 ***/

#endif /* !SP_PUBLIC_CONFIG_INCLUDED */


#if !SP_NO_PRIVATE_CONFIG

#if  !SP_CONFIG_H_INCLUDED
#define SP_CONFIG_H_INCLUDED 1

#define SICSTUS_PRIVATE_CONFIG_H 1
#if SP_WIN64 && !SP_WIN32
#error "[PM] 4.2.1+ inconsistency: SP_WIN64 is true but SP_WIN32 is false"
#endif

#if SP_NEW_DBG_POLICY

/* [PM] 4.2.1 the DBG/DBG_POISON is temporary to catch any legacy use. */
#if !DBG_POISON
#if defined DBG
#error "[PM] 4.2.1 did not expect DBG to be defined"
#endif  /* defined DBG */
#endif  /* !DBG_POISON */

#if SICSTUS_DBG
#if DBG_POISON
/* The strange formatting is to ensure configure does not comment out the undefs. */
/* HIDE FROM configure */#/* HIDE FROM configure */undef/* HIDE FROM configure */DBG
/* HIDE FROM configure */#/* HIDE FROM configure */undef/* HIDE FROM configure */DBG_POISON
#endif  /* DBG_POISON */
#define DBG SICSTUS_DBG
#endif  /* SICSTUS_DBG */

#endif  /* SP_NEW_DBG_POLICY */

/* [PM] 4.2 DBG is what our sources use but it should be passed as a -D
   option (so it is seen by code that does not use the internal part
   of config.h, e.g. foreign resources, test suite, ..., */
/* [PM] 4.2 Verify that SICSTUS_DBG and DBG are the same */
/* [PM] 4.2.1 Note, this is in the non-public section so it should
   never trigger in typical customer code. */
#if (DBG+0) != (SICSTUS_DBG+0)
#error "[PM] 4.2 DBG != SICSTUS_DBG"
#endif  /* DBG != SICSTUS_DBG */



#define JIT 1
#define JIT_ON 1
/* whether FLI predicates should be jitted */
#define JIT_FLI 1

/* whether to preload FLI failure continuation */
#define JIT_PRELOAD_FAIL 0

/* whether to use lq/stq instead of ld/std (POWER) */
#define JIT_USE_LQ_STQ 0
/* whether to pass the continuation pointer, implicitly, in link register (i.e. as return address). Only POWER, currently. */
#define JIT_USE_PLCALL_PASS_CP_IN_LINK 0


/* [PM] 4.3.2 PPC64LE JIT. Implies JIT */
#define JIT_PPC64LE 0


/* [PM] 4.3.2 experimental. Implies !JIT */
#define LLVM_JIT 0

#define JIT_COUNTER_LIMIT_DEFAULT 0
#if JIT
#define PROCEDURE_COUNTER 1
#define WORKER_JIT_COUNTER_LIMIT 1
#endif  /* JIT */

/* [PM] 4.3 count global inferences (when profiling flag is set) */
#define PROFILE_INFERENCES 0

/* [PM] 4.3.3 Whether to use the new hash functions. */
#define USE_HASHMIX 1

/* [PM] 3.9.1 if a run-time system foo.exe is in <DIR>/foo.exe then
   <DIR>/<SP_RTSYS_DIRNAME> is the directory where all the sicstus
   files can be found (traditionally named "lib" on UNIX, "sp39" on
   Win32 although these should both be something like
   sicstus-<major>.<minor>.<revision> eventually).  Note that this is
   not the same as SP_EMULATOR_DIRNAME, the layout is something like
   <DIR>/<SP_RTSYS_DIRNAME>/<SP_EMULATOR_DIRNAME>
*/
#define SP_REGVER SP_REGVER_undefined
#define SP_RTSYS_DIRNAME "sp-4.7.0"
#define SP_EMULATOR_DIRNAME "sicstus-4.7.0"
/* [PM] 3.11.0 */
#define LICENSE_PRODUCT "sicstus4.7_linux"

#define SICSTUS_FORCE_BUILD 0
/* [PM] 4.2 FORCE_BUILD is what our sources use */
#ifndef FORCE_BUILD
#if SICSTUS_FORCE_BUILD
#define FORCE_BUILD 1
#endif  /* SICSTUS_FORCE_BUILD */
#endif  /* !FORCE_BUILD */


/* [PM] 4.3 whether conversion to ISO compatible error/2 exceptions is
   done in raise_exception/1 rather than on_exception/3 and
   catch/3. */
#define SP_TRANSLATE_TO_ISO_EXCEPTION_IN_RAISER 1


#define WCX_LEGACY_CHAR_CLASSIFICATION 1

/* [PD] 3.12.7 Mac/64-bit (interim) */
#define SP_MACOSX_FROM_OS_WCX_FILE_IS_NFC 0

/* [PM] 4.2.1+ */
#define ENABLE_LAZY_INT_CAST 1

/* [PM] 4.3 */
#define ENABLE_PROLOG_INVARIANTS 0

#if !defined(INT_INFO_LARGE_INDEXER)
#define INT_INFO_LARGE_INDEXER 1
#endif	/* !defined(INT_INFO_LARGE_INDEXER) */
#if !defined(INCORE_INFO_LARGECASE)
#define INCORE_INFO_LARGECASE 1
#endif	/* INCORE_INFO_LARGECASE */

/* [PM] valgrind */

/* whether valgrind-specific features should be enabled (like
   alternate code that triggers less warnings). Should usually be
   false, always false for release. */
#define ENABLE_VALGRIND 0
/* whether _all_ valgrind support should be explicitly turned off
   (like run-time detection of valgrind). Ideally, there should be no
   need to set this to true, even for platforms that does not have
   valgrind. In reality the valgrind headers contains GCC specific
   extensions that fail on non-GCC compilers (like Microsoft cl.exe)
   even when they attempt to expand to no-OPs. */
#define DISABLE_VALGRIND 0

/* [PM] 4.1.3+ Electric Sheep */
#define DISABLE_FOREIGN_RESOURCES 0

/* [PM] 4.2 Whether po/sav -file content should be compressed */
#define SP_COMPRESS_PO 1
#if SP_COMPRESS_PO
/* [PM] 4.2 compressed po files require compression layer in SPIO */
#define SPIO_LAYER_ZLIB 1
#endif  /* SPIO_LAYER_ZLIB */

/* [PM] 4.2 false if int_heap_warn should be used (which it should not, see internals.texi) */
#define NO_INT_HEAP_WARN 1

/* [PM] 3.10.2 xref bips_version.pl */
#define BIPS_VERSION 1040700

/* System information */
#define SYSTEM_NAME "x86_64-linux-glibc2.17"
/* [PM] 4.1.3+ unused: #define HARDWARE_NAME HARDWARE_NAME_undefined */
/* [PM] 4.1.3+ unused: #define NODE_NAME NODE_NAME_undefined */
/* [PM] 4.1.3+ unused: #define HOST_TYPE HOST_TYPE_undefined */

#define SHSFX "so"
#define SHPRE "lib"
#define FLI_SHSFX "so"

/* [PM] 4.0 for some platforms we need to know what calling convention is used. */
#define FLI_APPLY_ASM_GENERIC SP_FLI_APPLY_ASM_GENERIC /* [PM] 4.2.1+ public name, above */
/* (e.g., x86_64-linux-glibc2.3) */
#define FLI_APPLY_ASM_SYSV_ABI_X86_64 1
/* SYSV ABI PowerPC Big Endian 64-bit (e.g. Linux) */
#define FLI_APPLY_ASM_ABI_PPC 0
/* SYSV ABI PowerPC Little Endian 64-bit (e.g. Linux) */
#define FLI_APPLY_ASM_ABI_PPC64LE 0
/* SYSV ABI PowerPC 32-bit Big Endian (e.g. Linux) */
#define FLI_APPLY_ASM_SYSV_ABI_PPC32 0
/* ARM "Procedure Call Standard for the ARM 64-bit Architecture" 64-bit, Little Endian (e.g. Linux) */
#define FLI_APPLY_ASM_ABI_AARCH64 0
/* The Apple variant of FLI_APPLY_ASM_ABI_AARCH64. Only valid if FLI_APPLY_ASM_ABI_AARCH64 is also true. */
#define FLI_APPLY_ASM_ABI_AARCH64_SUB_ABI_APPLE 0

#define RTKERNEL_BASENAME_WITH_DSO_SUFFIX_STRING "libsprt4-7-0.so"


/* Something like "libsprt-4-1-2-instance-XX.so". Must contain exactly
   two adjacent X that will be replaced by a counter. Must be a
   literal string) */
#define RTKERNEL_INSTANCE_TEMPLATE_STRING "libsprt4-7-0-instance-XX"

/* Define as '0' to disable BDD's. */
#define BDD 1

#define SPIDER 1

/* [PM] 3.9 Define as '0' to disable CLPFD. */
#define ENABLE_CLPFD 1

/* Define as '0' to disable threaded code in the emulator. */
#define THREADED 2

/* 3.9.1
   . Define WAM_USE_W_IS_SELF_STORAGE as 1 to make wam() access
     self_storage directly instead of using the w argument. This may
     or may not be a good idea depending on architecture. Off by
     default certainly.
   [PM] 4.0 Effect of WAM_USE_W_IS_SELF_STORAGE: moe much worse, zjuk slightly worse

*/
#ifndef WAM_USE_W_IS_SELF_STORAGE /* allow override */
#define WAM_USE_W_IS_SELF_STORAGE 0
#endif  /* WAM_USE_W_IS_SELF_STORAGE */

#define USE_SPIO_COND_VAR_NOW_AS_WALLTIME 1

#if !defined(FIX_SPRM_14822)
#define FIX_SPRM_14822 1
#endif	/* !defined(FIX_SPRM_14822) */

#if !defined(FIX_SPRM_15204)
/* [PM] 4.4.0 SPRM 15203. Do not throw non-ISO evaluation_error(float_not_a_number). */
#define FIX_SPRM_15204 1
#endif  /* !defined(FIX_SPRM_15204) */

#if !defined(FIX_SPRM_15212)
/* [PM] 4.4.0 SPRM 15212. Barf on bignum => {long,double} overflow (SP_get_float(), +float, +integer) */
#define FIX_SPRM_15212 1
#endif  /* !defined(FIX_SPRM_15212) */
#if !defined(FIX_SPRM_15214)
/* [PM] 4.4.0 SPRM 15214. Jitted FLI +float was wrong when passed a bignum argument. */
#define FIX_SPRM_15214 1
#endif  /* !defined(FIX_SPRM_15214) */
#if !defined(FIX_SPRM_15211)
/* [PM] 4.4.0 SPRM 15211. report float overflow on implicit bignum==>float conversion in arithmetics. */
#define FIX_SPRM_15211 1
#endif  /* !defined(FIX_SPRM_15211) */
#if !defined(FIX_SPRM_15105)
#define FIX_SPRM_15105 1
#endif	/* !defined(FIX_SPRM_15105) */
#if FIX_SPRM_15105
#if defined(__INTEL_COMPILER) && !FORCE_BUILD
#error "[PM] 4.4.0 icc (icc 7) incorrectly thinks (value==0.0 ? 0.0 : value) is the same as value (see https://godbolt.org/g/SgyfBg)"
#endif		  /* defined(__INTEL_COMPILER) */
#endif		  /* FIX_SPRM_15105 */

/* [PM] 4.0.0 wam.c needs to know if assembly will be post-processed */
#define ENABLE_WAM_ASM_PEEP 0

#ifndef PREFETCHING             /* [PM] 4.0 allow manual override */
/* [PM] 3.10.1 This has been off for a long time. Should measure this. */
#define PREFETCHING 0
#endif  /* PREFETCHING */

#define WORKER_TERM_EARLY 0

#if 0                           /* [PM] 4.0 These should only be passed on command line */
   /* [PM] 3.10.1 Define as '1' to use undocumented Core Process */
   #define USE_CPSEnableForegroundOperation 0
   /* [PM] 3.11.1 Define as '1' to use documented MacOS X 10.3 function TransformProcessType */
   #define USE_TransformProcessType 0
#endif  /* 0 */

/* [PM] 4.0.2 Use the new thread-device I/O */
#define SPIO_USE_LAYER_THREADDEVICE 1

#define SP_OS_SIGNAL_DELIVERED_SET_INTERRUPT 1
#define SP_TIMEOUT_INTERRUPT_RESTART 1

/* [PM] 4.5.0+ Whether is/2 directives are recorded. 0, 1, or 2. */
#define SP_RECORD_IS_DIRECTIVES 2

/* [PM] 4.1.0 Both these must be true to prevent abortive close from
   blocking when there are pending blocking (write) operations,
   e.g. when writing to a fifo. */
#define SPIO_SKIP_ABORTIVE_SYNC_DEFAULT 1
#define SPIO_ABORTIVE_WRITE_CANCEL_DEFAULT 1

/* [PM] 4.1.0 some extra things for SPIDER development */
#define SPIDER_MAINTENANCE 0

/* [PM] 3.10 Development system can put up a license entry dialog */
#define DS_CAN_LICENSE_DIALOG 0

#define SP_ANDROID 0

/* [PM] 3.9.2 True if setitimer(ITIMER_VIRTUAL) sometimes signals too early. See library/timeout.c */
#define SETITIMER_EARLY 1

/* Define as 1 if system uses FIONBIO for non-blocking I/O */
/* #define USE_FIONBIO 0 */

/* Header files */
#define HAVE_ALLOCA_H 1
#define HAVE_ASSERT_H 1
#define HAVE_DIRECT_H 0
#define HAVE_ERRNO_H 1
#define HAVE_FCNTL_H 1
#define HAVE_FEATURES_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_STDINT_H 1
#define HAVE_IO_H 0
#define HAVE_LANGINFO_H 1
#define HAVE_LOCALE_H 1
#define HAVE_MALLOC_H 1
#define HAVE_NETDB_H 1
#define HAVE_SIGNAL_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRNLEN 1
#define HAVE_STDATOMIC_H 0
#define HAVE_SYS_FILE_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_SOCKET_H 1
#define HAVE_SYS_SYSCTL_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_UTSNAME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_PROCINFO_H 0
#define HAVE_SYS_LDR_H 0
#define HAVE_SYS_MMAN_H 1
#define HAVE_TIME_H 1
#define HAVE_PTHREAD_H 1
#define HAVE_DIRECT_H 0
#define HAVE_MATH_H 1
#define HAVE_FLOAT_H 1
#define HAVE_LIMITS_H 1
#define HAVE_POLL_H 1

/* [PM] 3.9.1 IRIX 6.5 */
#define HAVE_OBJLIST_H 0
#define HAVE_ELF_H 1



/* Libraries */
#define HAVE_LIBPTHREAD 1

#define SOLARIS_THREADS 0
#define POSIX_THREADS (HAVE_LIBPTHREAD && ! SOLARIS_THREADS)
#define HAVE_NATIVE_THREADS (SOLARIS_THREADS || HAVE_LIBPTHREAD || SP_WIN32)

/* [PM] 4.0.5 Enable license checks for runtime systems */
#define USE_RUNTIME_LICENSE_SPRT 0
#define USE_RUNTIME_LICENSE_SPRE 1
#define USE_RUNTIME_LICENSE 1

/* Types */
#define HAVE_SA_FAMILY_T 1
#define HAVE_INTPTR_T 1

#define SP_SIZEOF_SHORT 2
/* [PM] 4.2.2 Some other SP_SIZEOF_... in the public section above */
/* long long gcc and C99 */
#define SP_SIZEOF_LONG_LONG 8
/* __int64 Win32 */
#define SP_SIZEOF___INT64 0

/* __int128 GCC/CLANG */
#define SP_SIZEOF___INT128 16
/* __int128_t GCC/CLANG */
#define SP_SIZEOF___INT128_T 16

#define ALIGNOF_FLOAT 4
#define ALIGNOF_DOUBLE 8
#define ALIGNOF_VOID_P 8

/* Function availability. Keep sorted! */

/* [PM] 3.9.1 this is getargs() from procinfo.h unrelated to the command line parsing getargs */
#define HAVE_AIX_GETARGS 0

#define HAVE_COMPLIANT_SNPRINTF 1
#define HAVE_CLOCK_GETTIME 1
#define HAVE_CLOSEFROM 0
#define HAVE_DIRFD 1
#define HAVE_FLOCKFILE 1
#define HAVE__FILENO 0
#define HAVE_FCNTL 1

#if __STDC_VERSION__ < 199901L
# if __GNUC__ >= 2
#  define SP__FUNCTION__ __FUNCTION__
# elif _MSC_VER >= 1300         /* VS .NET 2003 or newer */
#  define SP__FUNCTION__ __FUNCTION__
# else
#  define SP__FUNCTION__ "<unknown>"
# endif
# else  /* c99 */
#  define SP__FUNCTION__ __func__
#endif

#define HAVE_GETADDRINFO 1
#define HAVE_GETNAMEINFO 1
#define HAVE_GAI_STRERROR 1
#define HAVE_FREEADDRINFO 1

#define HAVE_GET_CURRENT_DIR_NAME 1
#define HAVE_GETCWD 1
#define HAVE_GETEXECNAME 0
#define HAVE_GETGRGID 1
#define HAVE_GETGRGID_R 1
#define HAVE_LONG_AND_SHORT_PATHNAMES 0
#define HAVE_GETPAGESIZE 1
#define HAVE_GETPWNAM 1
#define HAVE_GETPWNAM_R 1
#define HAVE_GETPWUID 1
#define HAVE_GETPWUID_R 1
#define HAVE_GETRUSAGE 1
#define HAVE_GETRLIMIT 1
#define HAVE_ISATTY 1
#define HAVE__ISATTY 0
#define HAVE_NL_LANGINFO 1
#define HAVE_PTHREAD_MUTEXATTR_SETTYPE 1
#define HAVE_PTHREAD_TESTCANCEL 1
#define HAVE__STRDUP 0
#define HAVE_GETENV 1
#define HAVE_RAISE 1
#define HAVE_SETLOCALE 1
#define HAVE_SIGACTION 1
#define HAVE_SIGNAL 1
#define HAVE_SNPRINTF 1
#define HAVE__SNPRINTF 0
#define HAVE_STRERROR 1
#define HAVE_THREAD_SAFE_STRERROR 0
#define HAVE_STRERROR_R 1
#define HAVE_SYSCONF 1
#define HAVE_VSNPRINTF 1
#define HAVE__VSNPRINTF 0
#define HAVE_WAITID 1
#define HAVE_TIMES 1
#define HAVE_DEBUGBREAK 0
#define HAVE_DLADDR 1
#define HAVE_LOADQUERY 0
#define HAVE_COPYSIGN 1
#define HAVE__COPYSIGN 0
#define HAVE_SETRLIMIT 1
#define HAVE_SELECT 1
#define HAVE_POLL 1

/* [PM] 4.1.3 poll() does not work for devices (i.e. Mac OS X). > 1 to
   assume broken for devices, 1 to do a run-time test (hope springs
   eternal...) and zero to assume working for devices.
*/
#define SP_POLL_BROKEN_FOR_DEVICES 0
/* [PM] 4.1.3 poll() does not work for fifos (i.e. Mac OS X 10.5.x but not Mac OS X 10.6.4) */
#define SP_POLL_BROKEN_FOR_FIFOS 2

/* [PM] 4.1.3 Explicitly override default use and non-use of poll() (poll() is broken for fifos on Linux but still usable) */
#define SP_USE_POLL 1
#define SP_DONT_USE_POLL 0

#define USE_PERF 1
#define USE_CAPSTONE 0
#define USE_OPDIS 0

/* Work around (Mac OS X <= 10.5.x) broken cancellation points. */
#define SP_HAVE_BROKEN_CANCELLATION_POINTS 0
/* [PM] 4.1.3 Avoid pthread_cancel(), when possible, by waiting a
   while for worker threads to exit. */
#define SP_AVOID_PTHREAD_CANCEL 1

/* [PM] 4.0  */
#define HAVE_ARITHMETIC_RIGHT_SHIFT 1

/* [PM] 3.9.1 IRIX 6.5 */
#define HAVE__RLD_OBJ_HEAD 0

/* [PM] 3.10 MSVC */
#define HAVE__HEAPMIN 0

#define HAVE_PROC_SELF_FD 1
#define HAVE_LINUX_PROC_SELF_EXE 1

/* Preprocessor symbols */
/* Define as 1 if I_SETSIG and S_INPUT are defined and needed for SIGPOLL signals to be generated. */
/* #define HAVE_SETSIG 0 */

/* [PM] 3.9.0 Should be 1 on everything except Win32 */
#define POSIX_SIGNALS 1

/* Misc */

/* [PM] detecting non-finite float */

/* [PM] 3.9 new way. */
#define SP_ISFINITE isfinite
#define SP_ISNAN isnan
#define SP_ISNAN_FINITE_IN_MATH_H 1
#define SP_ISNAN_FINITE_IN_FLOAT_H 0
#define SP_ISNAN_FINITE_IN_IEEEFP_H 0

/* [PM] 3.9.1 missing in 3.8 and 3.9.0! */
#define NAN_COMPARE_FAILS 1


#if HAVE_COPYSIGN
#define SP_COPYSIGN copysign
#elif HAVE__COPYSIGN
/* Visual Studio 2012 (VC 11) */
#define SP_COPYSIGN _copysign
#endif	/* HAVE__COPYSIGN */

/* [MC] 3.9.1 */
#define ACCURATE_QUOTIENT 1

/* [PD] 3.9 define as 1 to enable MT-Jasper to use PushLocalFrame/PopLocalFrame
   in make_arg_array() instead of crashing. */
#define JASPER_PUSH_LOCAL_REFS 0


/* [PM] Create all NaN equal */
#define CANONICAL_NAN 1

/* [PM] 3.10 xref memman.c, configure.in */
#define MEM_ALLOC MM_USE_MMAP

/* [PM] 4.2.1 Whether MAP_HUGETLB or similar functionality is (or may be) available */
#define SP_HUGEPAGES 1
/* [PM] 4.2.1 Whether MAP_HUGETLB or similiar should be used if available (ignored unless SP_HUGEPAGES) */
#define SP_HUGEPAGES_DEFAULT 0

#define MSVC_STRING_POOLING_BUG 0

/* [PM] 3.9.1b4 now set by configure.in */
#define LogSizeOfWord 3

#define SP_AIX 0
#define SP_DARWIN 0

#if __sparc__ && !sparc
#define sparc 1
#endif

/* CPU architecture BEGIN */
/* E.g. for JIT. Mutually exclusive (at most one of these is non-zero. */
/* Intel 32-bit */
#define SP_X86 0
/* Intel 64-bit */
#define SP_X64 1
/* POWER 64-bit */
#define SP_POWER64 0
/* AARCH64 (64-bit) */
#define SP_AARCH64 0
/* CPU architecture END */

#define SP_INTEL (SP_X86 || SP_X64)

#if JIT
#if SP_POWER64

/* No longer configurable. */
#define LINK_CALLEE_TOC_ENTRIES 1

#endif	/* SP_POWER64 */
#endif	/* JIT */

/* [PM] 3.9.1 Define our sized types 
 * These belong in support.h but are needed by some test-cases.
 */
#if HAVE_LIMITS_H
#include <limits.h>             /* [PM] 3.11.1 LLONG_MAX, LONG_LONG_MAX etc user below */
#endif /* HAVE_LIMITS_H */

#ifndef SP_int8
typedef signed char SP_int8;
#endif  /* SP_int8 */
#ifndef SP_uint8
typedef unsigned char SP_uint8;
#endif  /* SP_uint8 */

#ifndef SP_int16
#if (SP_SIZEOF_SHORT==2)
typedef short SP_int16;
typedef unsigned short SP_uint16;
#define SP_int16 SP_int16
#endif /* (SP_SIZEOF_SHORT==2) */
#endif /* !SP_int16 */
#define SP_int16_max ((SP_int16)0x7fff)
#define SP_int16_min ((-SP_int16_max) - 1)
#ifndef SP_int16
#error "no 16bit int type"
#endif /* !SP_int16 */

#define SP_uint16 SP_uint16

#ifndef SP_int32
#if (SP_SIZEOF_INT==4)
typedef int SP_int32;
#define SP_int32 SP_int32
#endif /* (SP_SIZEOF_INT==4) */
#endif /* SP_int32 */
#define SP_int32_max_raw (0x7fffffff)
#define SP_int32_max ((SP_int32)SP_int32_max_raw)
#define SP_int32_min ((-SP_int32_max) - 1)
#ifndef SP_int32
#error "no 32bit int type"
#endif /* !SP_int32 */

#ifndef SP_uint32
#if (SP_SIZEOF_INT==4)
typedef unsigned int SP_uint32;
#define SP_uint32 SP_uint32
#endif /* (SP_SIZEOF_INT==4) */
#endif /* SP_uint32 */
#define SP_uint32_max_raw (0xffffffff)
#define SP_uint32_max ((SP_uint32)SP_uint32_max_raw)
#define SP_uint32_min ((SP_uint32)0)
#ifndef SP_uint32
#error "no 32bit unsigned int type"
#endif /* !SP_uint32 */

#if SP_SIZEOF_INT==4 && defined(INT_MAX)
#if SP_int32_max_raw != INT_MAX
#error "inconsistency SP_int32_max_raw != INT_MAX"
#endif
#endif /* (SP_SIZEOF_INT==4 && defined(INT_MAX)) */

/* SP_int64 is first of int,long,long long,__int64
   This type is optional
*/
#ifndef SP_int64
#if (SP_SIZEOF_INT==8)
typedef int SP_int64;
typedef unsigned int SP_uint64;
/*
  [PM] 4.2.3 printf format codes for SP_int64/SP_uint64
 */
#define SPRIu64 "u"
#define SPRId64 "d"
#define SPRIx64 "x"
#define SPRIX64 "X"

#define SP_int64 SP_int64
#endif /* (SP_SIZEOF_INT==8) */
#endif /* SP_int64 */
#ifndef SP_int64
#if (SP_SIZEOF_LONG==8)
typedef long SP_int64;
typedef unsigned long SP_uint64;

#define SPRIu64 "lu"
#define SPRId64 "ld"
#define SPRIx64 "lx"
#define SPRIX64 "lX"

#define SP_int64 SP_int64
#endif /* (SP_SIZEOF_LONG==8) */
#endif /* SP_int64 */
#ifndef SP_int64
#if (SP_SIZEOF_LONG_LONG==8)
#if defined(__GNUC__)
__extension__                   /* [PM] 4.1 SPRM 11519 */
#endif  /* GCC */
typedef long long SP_int64;
#if defined(__GNUC__)
__extension__                   /* [PM] 4.1 SPRM 11519 */
#endif  /* GCC */
typedef unsigned long long SP_uint64;

#define SPRIu64 "llu"
#define SPRId64 "lld"
#define SPRIx64 "llx"
#define SPRIX64 "llX"

#define SP_int64 SP_int64

#endif /* (SP_SIZEOF_LONG_LONG==8) */
#endif /* SP_int64 */
#ifndef SP_int64
#if (SP_SIZEOF___INT64==8)
typedef __int64 SP_int64;
typedef unsigned __int64 SP_uint64;

#define SPRIu64 "I64u"
#define SPRId64 "I64d"
#define SPRIx64 "I64x"
#define SPRIX64 "I64X"

#define SP_int64 SP_int64
#endif /* (SP_SIZEOF___INT64==8) */
#endif /* SP_int64 */

#ifdef SP_int64
#define SP_uint64 SP_uint64
#endif

#ifdef SP_int64

#if (SP_SIZEOF_LONG_LONG==8)    /* int64 is long long */

/* C99 limit names */
#ifndef SP_int64_max
#ifdef LLONG_MAX
#define SP_int64_max LLONG_MAX
#endif /* LLONG_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef LLONG_MIN
#define SP_int64_min LLONG_MIN
#endif /* LLONG_MIN */
#endif /* SP_int64_min */

/* GCC limit names */
#ifndef SP_int64_max
#ifdef LONG_LONG_MAX
#define SP_int64_max LONG_LONG_MAX
#endif /* LONG_LONG_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef LONG_LONG_MIN
#define SP_int64_min LONG_LONG_MIN
#endif /* LONG_LONG_MIN */
#endif /* SP_int64_min */

/* AIX limit names */
#ifndef SP_int64_max
#ifdef LONGLONG_MAX
#define SP_int64_max LONGLONG_MAX
#endif /* LONGLONG_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef LONGLONG_MIN
#define SP_int64_min LONGLONG_MIN
#endif /* LONGLONG_MIN */
#endif /* SP_int64_min */

#endif /* int64 is long long */

/* Microsoft Visual Studio VC98 */
#ifndef SP_int64_max
#ifdef _I64_MAX
#define SP_int64_max _I64_MAX
#endif /* _I64_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef _I64_MIN
#define SP_int64_min _I64_MIN
#endif /* _I64_MIN */
#endif /* SP_int64_min */

/* Fallback. Problem is that we do not know what suffix to use for the constants (LL, i64) */
#ifndef SP_int64_max
#define SP_int64_max ((SP_int64)0x7fffffffffffffff)
#endif /* SP_int64_max */
#ifndef SP_int64_min
#define SP_int64_min ((-SP_int64_max) - 1)
#endif /* SP_int64_min */

#endif /* SP_int64 */

/* SP_int128/SP_uint128 is first of long,long long, __int128, __int128_t
   This type is optional and not generally available (GCC/CLANG on x64 has it)
*/
#ifndef SP_int128
#if (SP_SIZEOF_LONG==16)
typedef long SP_int128;
typedef unsigned long SP_int128;
#define SP_int128 SP_int128
#endif /* (SP_SIZEOF_LONG==16) */
#endif /* SP_int128 */

#ifndef SP_int128
#if (SP_SIZEOF_LONG_LONG==16)
#if defined(__GNUC__)
__extension__                   /* [PM] 4.1 SPRM 11519 */
#endif  /* GCC */
typedef long long SP_int128;
#if defined(__GNUC__)
__extension__                   /* [PM] 4.1 SPRM 11519 */
#endif  /* GCC */
typedef unsigned long long SP_uint128;
#define SP_int128 SP_int128
#endif /* (SP_SIZEOF_LONG_LONG==16) */
#endif /* SP_int128 */

#if !defined(SP_int128)
#if SP_SIZEOF___INT128 == 16

typedef __int128 SP_int128;
#define SP_int128 SP_int128
typedef unsigned __int128 SP_uint128;
#define SP_uint128 SP_uint128

#endif /* SP_SIZEOF___INT128 == 16 */
#endif	/* !defined(SP_int128) */

/* __int128_t and __uint128_t have been around since GCC 2.95 but,
   since it is not mentioned in the GCC manuals, we should not use
   them. Also, newer compilers have __int128 so it should not be a
   problem. */
#if 0
#if !defined(SP_int128)
#if SP_SIZEOF___INT128_T == 16

typedef __int128_t SP_int128;
#define SP_int128 SP_int128

#endif /* SP_SIZEOF___INT128_T == 16 */
#endif	/* !defined(SP_int128) */

#if !defined(SP_uint128)
#if SP_SIZEOF___UINT128_T == 16

typedef __uint128_t SP_uint128;
#define SP_uint128 SP_uint128

#endif /* SP_SIZEOF___UINT128_T == 16 */
#endif	/* !defined(SP_uint128) */
#endif	/* 0 */

/* signed version implies presence of unsigned version */
#if !defined(SP_uint128)
#ifdef SP_int128
#define SP_uint128 SP_uint128
#endif
#endif	/* !defined(SP_uint128) */



#ifdef SP_int128
#define SP_int128_max ((SP_int128)0x7fffffffffffffffffffffffffffffff )
#define SP_int128_min ((-SP_int128_max) - 1)

#endif /* SP_int128 */

/* SP_largest_int is first of SP_int128,SP_int64,SP_int32; SP_largest_uint is the corresponding unsigned type */
#ifndef SP_largest_int
#ifdef SP_int128
typedef SP_int128 SP_largest_int;
#define SP_largest_int SP_largest_int
typedef SP_uint128 SP_largest_uint;
#define SP_largest_uint SP_largest_uint
#endif /* SP_int128 */
#endif /* SP_largest_int */
#ifndef SP_largest_int
#ifdef SP_int64
typedef SP_int64 SP_largest_int;
#define SP_largest_int SP_largest_int
typedef SP_uint64 SP_largest_uint;
#define SP_largest_uint SP_largest_uint
#endif /* SP_int64 */
#endif /* SP_largest_int */
#ifndef SP_largest_int
#ifdef SP_int32
typedef SP_int32 SP_largest_int;
#define SP_largest_int SP_largest_int
typedef SP_uint32 SP_largest_uint;
#define SP_largest_uint SP_largest_uint
#endif /* SP_int32 */
#endif /* SP_largest_int */
#ifndef SP_largest_int
#error "no SP_largest_int defined"
#endif /* !SP_largest_int */
#ifndef SP_largest_uint
#error "no SP_largest_uint defined"
#endif /* !SP_largest_int */

#define ENABLE_MEM_STATS 0
#define ENABLE_SPACE_OUT 0

#define SP_MALLOC_RETURNS_NULL 1

/* [PM] 3.10.2 cl 13.x bug. Intrinsic strcat is broken */
#define SP_STRCAT_INTRINSIC_BROKEN 0
#if _MSC_VER
#if SP_STRCAT_INTRINSIC_BROKEN
#pragma function(strcat)        /* force use of real function */
#endif  /* SP_STRCAT_INTRINSIC_BROKEN */
#endif  /* _MSC_VER */

#endif /* !SP_CONFIG_H_INCLUDED */
#endif /* !SP_NO_PRIVATE_CONFIG */
#ifndef SP_CONFIG_SUFFIX_H_INCLUDED
#define SP_CONFIG_SUFFIX_H_INCLUDED
/* [PM] 4.1.3 This file goes last into the generated config.h

   It is useful for things that should be included everywhere,
   including user code, but that is not generated by configure.
 */

/* [PM] 4.1.3 Let GCC check printf-style arguments. */
#ifdef __GNUC__
#ifndef SPEXP_ATTRIBUTE__FORMAT_1_2
#define SPEXP_ATTRIBUTE__FORMAT_1_2 __attribute__ ((format (printf, 1, 2)))
#endif                              /* !SPEXP_ATTRIBUTE__FORMAT_1_2 */
#ifndef SPEXP_ATTRIBUTE__FORMAT_2_3
#define SPEXP_ATTRIBUTE__FORMAT_2_3 __attribute__ ((format (printf, 2, 3)))
#endif                              /* !SPEXP_ATTRIBUTE__FORMAT_2_3 */
#endif /* !GCC */

#ifndef SPEXP_ATTRIBUTE__FORMAT_1_2
#define SPEXP_ATTRIBUTE__FORMAT_1_2 /* empty */
#endif                              /* !SPEXP_ATTRIBUTE__FORMAT_1_2 */

#ifndef SPEXP_ATTRIBUTE__FORMAT_2_3
#define SPEXP_ATTRIBUTE__FORMAT_2_3 /* empty */
#endif                              /* !SPEXP_ATTRIBUTE__FORMAT_2_3 */

#endif  /* !SP_CONFIG_SUFFIX_H_INCLUDED */
