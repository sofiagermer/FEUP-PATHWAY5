/* Copyright (C) 1995, Swedish Institute of Computer Science. */

/* Declarations & definitions for the SICStus Prolog C interface. */

/*----------------------------------------------------------------------*/


#ifndef INCLUDED_SICSTUS_H
#define INCLUDED_SICSTUS_H

#if _MSC_VER > 1000
#pragma once
#endif /* _MSC_VER > 1000 */

#ifdef __GNUC__
#define SP_HAVE_GCC_VERSION(MAJOR,MINOR) ((__GNUC__ > (MAJOR)) || (__GNUC__ == (MAJOR) && __GNUC_MINOR__ >= (MINOR)))
#else  /* !__GNUC__ */
#define SP_HAVE_GCC_VERSION(MAJOR,MINOR) 0
#endif  /* !__GNUC__ */

#ifndef SICSTUS_OLDHOOKS
#define SICSTUS_OLDHOOKS 1      /* [PM] 4.0 Should make this zero for SP4 release */
#endif  /* SICSTUS_OLDHOOKS */

#ifdef __cplusplus
#define SP_BEGIN_DECL extern "C" {
#define SP_END_DECL }
#else  /* !__cplusplus */
#define SP_BEGIN_DECL
#define SP_END_DECL
#endif

SP_BEGIN_DECL

  /* [PM] 3.9 platform and version dependant info now goes into the
     public part of include/sicstus/config.h.

     The runtime system gets the non-public defines by not defining
     SP_NO_PRIVATE_CONFIG when including config.h.

     [PM] 4.1 SP_NO_PRIVATE_CONFIG is also used by spio_config.h for similar purpose.
   */
#define SP_NO_PRIVATE_CONFIG 1
#include "config.h"
/* #undef SP_NO_PRIVATE_CONFIG */

/* [PM] Split export declaration for foreign resources DLLs and sprt.dll */

   /* For declaring functions exported from sicstus runtime (DLL). */
      /* API functions accessed via dispatch table, not exported from DLL */

/* SPDLL_DECLARE_EXPORTED *declaring* functions imported from DLL, e.g. from glue code of a foreign function DLL. */
#if defined(_MSC_VER)
#define SPDLL_DECLARE_EXPORTED __declspec(dllexport)
#elif SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
#define SPDLL_DECLARE_EXPORTED __attribute__ ((__visibility__ ("default")))
#else
#define SPDLL_DECLARE_EXPORTED  /* empty */
#endif
/* SPDLL_DEFINE_EXPORTED *defining* functions imported from DLL, e.g. from glue code of a foreign function DLL. */
#if defined(_MSC_VER)
#define SPDLL_DEFINE_EXPORTED __declspec(dllexport)
#elif SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
#define SPDLL_DEFINE_EXPORTED /* empty */
#else
#define SPDLL_DEFINE_EXPORTED  /* empty */
#endif

#if SPDLL /* True if a dynamic (DLL) foreign resource, e.g., from splfr  */

      /* For *declaring* functions exported from glue code of a foreign function DLL
         Used by spld glue */
#if defined(_MSC_VER)
#define SPGLUEEXP __declspec(dllexport)
#elif SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
#define SPGLUEEXP __attribute__ ((__visibility__ ("default")))
#else
#define SPGLUEEXP
#endif

      /* For *defining* functions exported from glue code of a foreign function DLL */
#if defined(_MSC_VER)
#define SPGLUEEXP1 __declspec(dllexport)
#else
#define SPGLUEEXP1
#endif
#define SPGLUEEXP2

#else /* static foreign resource (or (static or dynamic) sicstus runtime) */

#ifndef SPGLUEEXP
#if SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0) && !SP_DISABLE_GCC_VISIBILITY_FOR_STATIC_RESOURCE
/* [PM] 4.1 This is necessary for (XCode 3.1.4) gcc 4.0.1 on Mac OS X
   10.5.8 in order for static linking with foreign resources to
   work. It should not hurt other platforms. */
#define SPGLUEEXP __attribute__ ((__visibility__ ("default")))
#endif
#endif  /* SPGLUEEXP */

#ifndef SPGLUEEXP
#define SPGLUEEXP
#endif  /* SPGLUEEXP */

#define SPGLUEEXP1
#define SPGLUEEXP2
#endif

/*----------------------------------------------------------------------*/
/* Common */

#define SP_TYPE_ERROR 0 /* 4.2.2 error return from SP_term_type */
#define SP_TYPE_VARIABLE 1
#define SP_TYPE_INTEGER 2
#define SP_TYPE_ATOM 3
#define SP_TYPE_FLOAT 4
#define SP_TYPE_COMPOUND 5

#define SP_SUCCESS 1
#define SP_FAILURE 0
#ifndef SP_ERROR
#define SP_ERROR (-1)
#endif

/* [PM] 3.9.1 SP_WHEN_ERROR, ... < 0 (never seen by (de)init functions) */
#define SP_WHEN_EXPLICIT 0
/* [PM] 4.0 We no longer unload foreign resources when saving:
   #define SP_WHEN_SAVE 1
*/
#define SP_WHEN_RESTORE 1
#define SP_WHEN_EXIT 2

#include <stddef.h>       /* [PM] 3.9.1 size_t has now made into the SICStus API prototypes */
#include <stdarg.h>

#include "sicstus_spio.h"


/* [PM] 3.9 PROTOTYPE/PROTOARGS are tags used by transhdr.pl to
            extract information about API functions. */

/* [PM] 3.9 Use PROTOTYPE when only arg types are specified */
#define PROTOTYPE(argl) argl
/* [PM] 3.9 Use PROTOARGS when arg types and (all) argnames are
            specified.  In this case the argname must be last
*/
#define PROTOARGS(argl) argl

#ifndef SP_FLI_CONST
#define SP_FLI_CONST const      /* used in splfr-generated glue */
#endif  /* !SP_FLI_CONST */

#define ANYPOINTER void *

/* [PM] 3.9.2 provide a default so only DS and Extended Runtimes need to care about this */
#ifndef SPLD_DSP
/* Kind of runtime; 0 RT, 1 DS, 2 Extende RT (3.9.2) */
#define SPLD_DSP 0
#endif /* SPLD_DSP */

#define SP_GLUE_INITIALIZE_OPTION_RESTORE   0x000001
#define SP_GLUE_INITIALIZE_OPTION_ARGV_UTF8 0x000100
#define SP_GLUE_INITIALIZE_OPTION_RESERVED1 0x001000
#define SP_GLUE_INITIALIZE_OPTION_RESERVED2 0x002000
#define SP_GLUE_INITIALIZE_OPTION_RESERVED3 0x004000
#define SP_GLUE_INITIALIZE_OPTION_RESERVED4 0x008000
#define SP_GLUE_INITIALIZE_OPTION_MAIN      0x010000
#define SP_GLUE_INITIALIZE_OPTION_RESERVED5 0x020000
#define SP_GLUE_INITIALIZE_OPTION_RESERVED6 0x040000


/* [PM] 4.2 Try to boot as development system, otherwise fall back to
   dsp value. SPRM 11749.

   The idea is that SP_GLUE_INITIALIZE_OPTION_TRY_DEVSYS will quitly
   cause a runtime system to boot from spds.sav if it is present. This
   would make it easy to debug a deployed runtime system by just
   copying spds.sav (and license info) to the right place.

   Corresponds to setting the system property (or environment
   variable) SP_USE_DEVSYS to "yes"
*/
#define SP_GLUE_INITIALIZE_OPTION_TRY_DEVSYS       SP_GLUE_INITIALIZE_OPTION_RESERVED1
/* Do not perform prolog debugger setup (ignored unless SP_GLUE_INITIALIZE_OPTION_TRY_DEVSYS). Obscure. */
#define SP_GLUE_INITIALIZE_OPTION_DONT_BOOT_DEVSYS SP_GLUE_INITIALIZE_OPTION_RESERVED2
/* Attach to SPIDER (implies SP_GLUE_INITIALIZE_OPTION_TRY_DEVSYS).
   Corresponds to setting the system property SP_ATTACH_SPIDER to
   "yes" */
#define SP_GLUE_INITIALIZE_OPTION_ATTACH_SPIDER SP_GLUE_INITIALIZE_OPTION_RESERVED3
/* Allow system properties & environment to set the DEVSYS and SPIDER options (this is the default in 4.2.0) */
#define SP_GLUE_INITIALIZE_OPTION_ALLOW_DEVSYS  SP_GLUE_INITIALIZE_OPTION_RESERVED4
/* Disallow system properties & environment to set the DEVSYS and
   SPIDER options. Corresponds to setting the system property
   SP_ALLOW_DEVSYS to "no" */
#define SP_GLUE_INITIALIZE_OPTION_DISALLOW_DEVSYS  SP_GLUE_INITIALIZE_OPTION_RESERVED5
/* Do not call trace/0 after booting DSRT. Corresponds to setting the
   system property SP_DEVSYS_NO_TRACE to "yes" */
#define SP_GLUE_INITIALIZE_OPTION_NO_TRACE  SP_GLUE_INITIALIZE_OPTION_RESERVED6

/* Alternative way to pass the option bits via the dsp
   parameter. This is more conventient for users since they can just
   redefine SPLD_DSP

   For example, to build a runtime system that always tries to attach
   to SPIDER and that do not automatically call trace/0 you could use:

     spld ... "--cflag=-DSPLD_DSP=SPLD_DSP_BIT_ATTACH_SPIDER|SPLD_DSP_BIT_NO_TRACE" ...

   To build a runtime system that cannot be told to start a
   development system, you could use:
     spld --no-allow-devsys ...
   or
     spld ... "--cflag=-DSPLD_DSP=SPLD_DSP_BIT_DISALLOW_DEVSYS" ...

 */
#define SPLD_DSP_BIT_TRY_DEVSYS      0x0010 /* Sets SP_GLUE_INITIALIZE_OPTION_TRY_DEVSYS */
#define SPLD_DSP_BIT_ATTACH_SPIDER   0x0020 /* Sets SP_GLUE_INITIALIZE_OPTION_ATTACH_SPIDER */
#define SPLD_DSP_BIT_ALLOW_DEVSYS    0x0040 /* Sets SP_GLUE_INITIALIZE_OPTION_ALLOW_DEVSYS */
#define SPLD_DSP_BIT_DISALLOW_DEVSYS 0x0080 /* Sets SP_GLUE_INITIALIZE_OPTION_DISALLOW_DEVSYS */
#define SPLD_DSP_BIT_NO_TRACE        0x0100 /* Sets SP_GLUE_INITIALIZE_OPTION_NO_TRACE */


#define SP_initialize(Argc,Argv,Options) \
  sp_glue_initialize((Argc),(Argv),(Options),sp_pre_linkage,sp_pre_map,SPLD_DSP,SP_GLUE_INITIALIZE_OPTION_RESTORE)

#define SP_errno SP_get_errno()
#define SP_stdin SP_get_stdin()
#define SP_stdout SP_get_stdout()
#define SP_stderr SP_get_stderr()
#define SP_curin SP_get_curin()
#define SP_curout SP_get_curout()

/*----------------------------------------------------------------------*/

/* [PM] 4.0.3 Tell SP_set_argv that argv strings are using the system
   locale (whatever that is). Only makes sense on Unix-like systems
   (Windows uses Unicode/UTF-8) */
#define SP_SET_ARGV_OPTION_SYSTEM_ENCODING SPIO_BIT(0)


#define SP_GET_ENCODING_OPTION_LOCALE SPIO_BIT(0)
#define SP_GET_ENCODING_OPTION_LOCALE_OPTION_ SPIO_NEXT_BIT(SP_GET_ENCODING_OPTION_LOCALE)
#define SP_GET_ENCODING_OPTION_LOCALE_OPTION  (SP_GET_ENCODING_OPTION_LOCALE_OPTION_ | SP_GET_ENCODING_OPTION_LOCALE)
#define SP_GET_ENCODING_OPTION_LOCALE_STDIO_  SPIO_NEXT_BIT(SP_GET_ENCODING_OPTION_LOCALE_OPTION_)
#define SP_GET_ENCODING_OPTION_LOCALE_STDIO   (SP_GET_ENCODING_OPTION_LOCALE_STDIO_ | SP_GET_ENCODING_OPTION_LOCALE)
#define SP_GET_ENCODING_OPTION_SEEKABLE       SPIO_NEXT_BIT(SP_GET_ENCODING_OPTION_LOCALE_STDIO_)


/*----------------------------------------------------------------------*/

/* [PM] 4.0 For internal use only */

/*
   Call this to cause a memory fault when you cannot be bothered to
   handle NULL from SP_malloc/SP_realloc/SP_calloc/SP_strdup etc

   MSG may be NULL

   Can be called from foreign resources etc.
*/
#define LAZY_MEMORY_FAULT(MSG) do {             \
  SP_ASSERT(0);                                 \
  sp_memory_fault((MSG),-1);                    \
} while (0)

/* [PM] 4.0 use this like LAZY_NULL_CHECK(p = SP_malloc(...)); when you cannot be bothered to handle out-of-memory

Can be called from foreign resources etc.
*/
#define LAZY_NULL_CHECK(EXPR) do{               \
  if ((EXPR) == NULL) LAZY_MEMORY_FAULT(NULL);  \
} while (0)


/*----------------------------------------------------------------------*/
/* Types */

#if SP_WIN64

typedef unsigned __int64 SP_term;
typedef unsigned __int64 SP_atom;
typedef unsigned __int64 SP_uinteger;
typedef signed   __int64 SP_integer;
typedef signed   __int64 SP_qid;	/* Choice offset */

#define SPRIuPTR "I64u"
#define SPRIdPTR "I64d"
#define SPRIxPTR "I64x"
#define SPRIXPTR "I64X"

#else  /* !SP_WIN64 */

typedef unsigned long SP_term;
typedef unsigned long SP_atom;
typedef unsigned long SP_uinteger;
typedef long          SP_integer;
typedef long          SP_qid;	/* Choice offset */


#if SP_SIZEOF_VOID_P == SP_SIZEOF_INT /* (32-bit) */
#if !defined(SPRI_PTR_PREFIX)
#if 1

/* [PM] 4.3.2 Even though int and long has the same size on this
   platform the SP_integer type is long (not int), so the printf
   string should also correspond to long. If nothing else, it avoids C
   compiler warnings. */
#define SPRI_PTR_PREFIX "l"
#else /* pre 4.3.2 */
#define SPRI_PTR_PREFIX /* empty */
#endif
#endif /* !defined(SPRI_PTR_PREFIX) */

#else /* 64-bit */
#define SPRI_PTR_PREFIX "l"
#endif  /* 64-bit */


#define SPRIuPTR SPRI_PTR_PREFIX "u"
#define SPRIdPTR SPRI_PTR_PREFIX "d"
#define SPRIxPTR SPRI_PTR_PREFIX "x"
#define SPRIXPTR SPRI_PTR_PREFIX "X"

#endif /* !SP_WIN64 */

#if SP_SIZEOF_VOID_P == 4 /* 32-bit */
#define SP_INTEGER_MAX SP_int32_max
#define SP_INTEGER_MIN SP_int32_min
#elif SP_SIZEOF_VOID_P == 8 /* 64-bit */
#define SP_INTEGER_MAX SP_int64_max
#define SP_INTEGER_MIN SP_int64_min
#endif /* 64-bit */

/* [PM] 4.2.1+

  printf format controls for SP_integer/SP_uinteger. Use as: 

     ... SP_integer x = 4711;  printf("x=0x%" SPRIxINTEGER " in hex", x); ... 

  would output:

     x=0x1267 in hex
  
  This can be used for portability, to ensure correct behavior on
  platforms (64-bit Windows) where SP_integer is not the same as long
  integer. Each <c> in the name SPRI<c>INTEGER denotes the conversion
  modifier of the same name.
*/
/* [PM] 4.2.2 SP_integer is always long on platforms where int == long (i.e. all except Win64) */
#if SP_WIN64
#define SPRIuINTEGER SPRIuPTR
#define SPRIdINTEGER SPRIdPTR
#define SPRIxINTEGER SPRIxPTR
#define SPRIXINTEGER SPRIXPTR
#else  /* !SP_WIN64 */
#define SPRIuINTEGER "lu"
#define SPRIdINTEGER "ld"
#define SPRIxINTEGER "lx"
#define SPRIXINTEGER "lX"
#endif /* !SP_WIN64 */

/* [PM] 4.2.1+

  printf format controls for size_t. Use as: 

     ... size_t x = 4711;  printf("x=%" SPRIdSZ " bytes", x); ... 

  would output:

     x=4711 bytes
  
  This can be used for portability, instead of the POSIX 'z' control
  which is not available on Windows. Each <c> in the name SPRI<c>SZ
  denotes the conversion modifier of the same name.
*/
#if SP_WIN32
/* [PM] 4.2.1+ I (capital I) corresponds to 32 (64) bits integers on
   32 (64) bit Windows platforms, so we can use the same for all
   Windows platforms regardless of word width. */
#define SPRIuSZ "Iu"
#define SPRIdSZ "Id"
#define SPRIxSZ "Ix"
#define SPRIXSZ "IX"
#else  /* !SP_WIN32 */
/* [PM] 4.2.1+ z corresponds to size_t sized integers, so we can use
   the same for all platforms regardless of word width. (z is POSIX,
   not available on Windows) */
#define SPRIuSZ "zu"
#define SPRIdSZ "zd"
#define SPRIxSZ "zx"
#define SPRIXSZ "zX"
#endif /* !SP_WIN32 */

struct SP_pred_ref_;            /* forward declaration */
typedef struct SP_pred_ref_ *SP_pred_ref;
typedef int SP_term_ref;
typedef struct SP_globref_ *SP_globref; /* [MC] 4.0.5 */
typedef struct { void *mutex; } SP_mutex;

/* Use this like:
static SP_mutex my_mutex = SP_MUTEX_INITIALIZER;
...
SP_mutex_lock(&my_mutex);
...
SP_mutex_unlock(&my_mutex);
*/
#define SP_MUTEX_INITIALIZER {0}


/*** External Object types */

typedef struct SP_external_object_info_ *SP_external_object_type;
typedef struct SP_external_object_link_ SP_external_object_link;

typedef void (SPCDECL *SP_external_object_finalizer)(void *obj, void* type_data);

/* Should wrap with a structure where the first arg is put by SPPutExternalObjectLink */
typedef int (SPCDECL *SP_external_object_putter)(SP_term_ref, SP_external_object_link*);


typedef int (SPCDECL SP_EventFun) (void *);

#define SP_EVENTFUN2_OPTION_DEINITIALIZE   0x0001
#define SP_EVENTFUN2_OPTION_FAILURE        0x0002
#define SP_EVENTFUN2_OPTION_ERROR          0x0004
#define SPIO_S_EVENT_SUCCESS SPIO_S_NOERR           /* as SP_SUCCESS from SP_EventFun */
#define SPIO_S_EVENT_FAILURE SPIO_S_PRIVATE_ERROR_1 /* as SP_FAILURE from SP_EventFun */
#define SPIO_S_EVENT_ERROR   SPIO_S_PRIVATE_ERROR_2 /* as SP_ERROR from SP_EventFun */
typedef spio_t_error_code (SPCDECL SP_EventFun2) (void *data, spio_t_bits options);

#define SP_SIGACTION_OPTION_SOFT_INTERRUPT 0x0001
#define SP_SIGACTION_OPTION_HARD_INTERRUPT 0x0002
#define SP_SIGACTION_OPTION_RESTART        0x0004

/* typedef void (SPCDECL SP_VoidFun) (void); */
typedef void (SPCDECL SP_SigFun) (int, void *);
typedef void (SPCDECL SP_SetWindowTitleHook) (void *, char const *title_ienc);
typedef /* ienc */ char const * (SPCDECL SP_GetWindowTitleHook) (void *);

/* return zero if there were no work to be done, return one if it did some work */
typedef int (SPCDECL SP_idle_hook) (void *cookie);



  /* Foreign resources and glue functions */

struct SICSTUS_API_STRUCT;      /* forward declaration, real def in spaux.h */
typedef struct SICSTUS_API_STRUCT SPEnv;


#if MULTI_SP_AWARE
/* Name mangling is used for link time checking. If the foreign
   declaration says 'foo' then the function called by the generated
   glue-code will be 'foo_spenv_multi_sp' if MULTI_SP_AWARE is set
   when compiling.
*/

#define SP_MANGLE(FNAME) FNAME ## _spenv_multi_sp

#define SPAPI_ARG_NAME spenv_arg
#define SPAPI_STASH_NAME (&((SPAPI_ARG_NAME)->stash))
#define SPAPI_ARG0 SPAPI_ARG_NAME
#define SPAPI_ARG SPAPI_ARG0,
#define SPAPI_ARG_PROTO_DECL0 SPEnv *SPAPI_ARG_NAME
#define SPAPI_ARG_LOCAL_DECL SPEnv *SPAPI_ARG_NAME;
#define SPAPI_ARG_PROTO_DECL SPAPI_ARG_PROTO_DECL0, 
#define SPAPI_ARG_IGNORE do{(void)SPAPI_ARG_NAME;}while(0)

/* This is where SP_put_list et al find the dispatch table, it is
   passed as an argument to all functions that use the SICStus API. */
#ifndef SICStusDISPATCHVAR      /* allow it to be overridden by user */
#define SICStusDISPATCHVAR SPAPI_ARG_NAME
#endif /* SICStusDISPATCHVAR */


#else /* !MULTI_SP_AWARE */
#define SPAPI_ARG0
#define SPAPI_ARG
#define SPAPI_ARG_PROTO_DECL0 void
#define SPAPI_ARG_LOCAL_DECL
#define SPAPI_ARG_PROTO_DECL
#define SPAPI_ARG_IGNORE

/* Use the definition of SICStusDISPATCHVAR from spaux.h */
#endif /* !MULTI_SP_AWARE */

/* Used by glue code */
struct worker;

#if SP_FLI_APPLY_ASM_GENERIC
typedef struct sp_t_fli_call_ {
  void *stash_arg;
  size_t ninteger_arg;
  size_t ndouble_arg;
  SP_integer integer_return;
  double double_return;
  SP_integer integer_arg[256];
  double double_arg[256];
} sp_t_fli_call;
#endif /* SP_FLI_APPLY_ASM_GENERIC */

#if SP_FLI_APPLY_ASM_GENERIC
typedef void (SPCDECL SP_GenericGlueFun) (sp_t_fli_call *args_info);
typedef SP_GenericGlueFun *SP_GenericGlueFunPtr;
#endif  /* SP_FLI_APPLY_ASM_GENERIC */

/* [PM] 3.9b4 
   The (type of the) glue function actually called by the SICStus
   runtime when a function that implements a predicate in a foreign
   resource is to be invoked (Not to be confused with SP_RTPred) */

typedef int (SPCDECL SP_GlueFun) PROTOTYPE((SPEnv *spenv));
typedef SP_GlueFun *SP_GlueFunPtr;

/* Used with SP_define_c_predicate */
typedef int (SPCDECL SP_CPredFun)(SP_term_ref goal, void *stash);

typedef void (SPCDECL SP_InitFunPlain) (int);
typedef void (SPCDECL SP_InitFunMulti) (SPEnv *, int);

typedef union {
  SP_InitFunPlain *plain;
  SP_InitFunMulti *multi;
} SP_InitFunUnion;



/* Type of foreign resource set-up function (generated by splfr) */

struct SICSTUS_DISPATCH_TABLE_STRUCT;

/* [PM] 3.10 If SP_DISPATCH_API_VERSION_MAJOR differs then
   sp_main_helper may not find the SP API functions it needs. This was
   a real issue in SP 3.10. */
#define SP_MAINFUN_PARAMS_VERSION (((SP_DISPATCH_API_VERSION_MAJOR)<<16) + 1) /* The LSB is for real main-fun param changes */


struct SP_MAINFUN_PARAMS_STRUCT;
typedef struct SP_MAINFUN_PARAMS_STRUCT SP_MAINFUN_PARAMS;

typedef int (SPCDECL SP_MainFun)(SP_MAINFUN_PARAMS *params);

/*----------------------------------------------------------------------*/

/* Undocumented */
#define SPTI_EVENT_FLAG_RESERVED_1_ 0x00001
#define SPTI_EVENT_FLAG_RESERVED_2  0x00002
#define SPTI_EVENT_FLAG_RESERVED_3  0x00004
#define SPTI_EVENT_FLAG_RESERVED_4  0x00008
#define SPTI_EVENT_FLAG_RESERVED_5  0x00010
#define SPTI_EVENT_FLAG_RESERVED_6  0x00020
#define SPTI_EVENT_FLAG_RESERVED_7  0x00040
#define SPTI_EVENT_FLAG_RESERVED_8  0x00080
#define SPTI_EVENT_FLAG_INITED      SPTI_EVENT_FLAG_RESERVED_1_

#define SPTI_REQUEST_JIT_DEFINED    1
#define SPTI_REQUEST_JIT_UNDEFINED  2
#define SPTI_REQUEST_ONLOAD         3
#define SPTI_REQUEST_ONUNLOAD       4
#define SPTI_REQUEST_JIT_START      5
#define SPTI_REQUEST_JIT_END        6


typedef union sp_t_spti_event_u_ sp_t_spti_event_u;
union sp_t_spti_event_u_ {
  void const *ptr;
  char const *str;
  size_t size;
  spio_t_bits flags;
  SP_integer integer;
};
typedef struct sp_t_spti_event_ sp_t_spti_event;
struct sp_t_spti_event_ {
  size_t size;
  spio_t_bits flags;
  SPEnv *spenv;
  SP_integer request;
  size_t params_len;
  sp_t_spti_event_u *params;
};

#define SP_SPTI_HOOK_OPTION_RESERVED_1 0x00001
#define SP_SPTI_HOOK_OPTION_RESERVED_2 0x00002
#define SP_SPTI_HOOK_OPTION_RESERVED_3 0x00004
#define SP_SPTI_HOOK_OPTION_RESERVED_4 0x00008
#define SP_SPTI_HOOK_OPTION_RESERVED_5 0x00010
#define SP_SPTI_HOOK_OPTION_RESERVED_6 0x00020
#define SP_SPTI_HOOK_OPTION_RESERVED_7 0x00040
#define SP_SPTI_HOOK_OPTION_RESERVED_8 0x00080

typedef spio_t_error_code (SPCDECL SP_spti_hook) (sp_t_spti_event *event, void *cookie);

#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_1_ 0x00001
#define SP_INSTALL_SPTI_HOOK_OPTION_UNINSTALL SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_1_
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_2 0x00002
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_3 0x00004
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_4 0x00008
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_5 0x00010
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_6 0x00020
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_7 0x00040
#define SP_INSTALL_SPTI_HOOK_OPTION_RESERVED_8 0x00080

/*----------------------------------------------------------------------*/

/* Wide character encoding (WCX) */

#define SP_WCX_FLAG       -1

#define CHT_LAYOUT_CHAR    1
#define CHT_SMALL_LETTER   2
#define CHT_CAPITAL_LETTER 3
#define CHT_SOLO_CHAR      4
#define CHT_SYMBOL_CHAR    5
#define CHT_UNDECIDED      6
#define CHT_OTHER          7

  /* WCX conversion contexts (SP_from_os(), SP_to_os() */

#define WCX_FILE		0x0001
#define WCX_OPTION		0x0002
#define WCX_BEST_EFFORT         0x0003

  /* WCI (alias Ienc) utilities for use in the foreign code */


#define WCI_MAX_BYTES 6

/*----------------------------------------------------------------------*/
/* Global vars defined in the "resource table" file used by Runtime
   Systems. */

extern SP_MainFun *sp_pre_linkage[];
extern char *sp_pre_map[];

/* WCX */
/* The parameter which should be 0, 1, or 2, denoting user_input,
user_output, and user_error respectively.

The post-hook is called after the streams have been created and
enable the user to modify the default streams.
*/

#define SP_STREAMHOOK_STDIN   0x00
#define SP_STREAMHOOK_STDOUT  0x01
#define SP_STREAMHOOK_STDERR  0x02
#define SP_STREAMHOOK_BIN     0x40
#define SP_STREAMHOOK_WCI     0x41
#define SP_STREAMHOOK_NULL    0x80
#define SP_STREAMHOOK_OPEN    0x81
#define SP_STREAMHOOK_LIB     0x90   
#define SP_STREAMHOOK_C       0xff

typedef SP_stream * (SPCDECL SP_UserStreamHook) (void *, int which);
typedef void (SPCDECL SP_UserStreamPostHook) (void *, int which, SP_stream *);


/* Alloc */

typedef int (SPCDECL SP_InitAllocHook) (size_t alignment, void *cookie);
typedef void (SPCDECL SP_DeinitAllocHook) (void *cookie);
typedef void* (SPCDECL SP_AllocHook) (size_t size, size_t *actual_sizep, void *cookie);
typedef int (SPCDECL SP_FreeHook) (void *ptr, size_t size, int force, void *cookie);

#define SP_on_fault(Stmt,Reason,CleanupStmt)	\
{						\
  jmp_buf m__buf;				\
						\
  if (!setjmp(m__buf))				\
    {						\
      sp_set_abort_env(&m__buf);		\
      {Stmt};					\
      sp_set_abort_env(NULL);			\
    }						\
  else						\
    {						\
      (Reason) = sp_get_abort_err_ienc();	\
      {CleanupStmt};				\
    }						\
}

/*----------------------------------------------------------------------*/
/* Error codes used to raise_exception from C-code */
/* xref Bips/intrins1.pl */
#define INSTANTIATION_ERROR 0
#define TYPE_ERROR 1
#define DOMAIN_ERROR 2
#define REPRESENTATION_ERROR 3
#define EXISTENCE_ERROR 4
#define SYSTEM_ERROR 5
#define PERMISSION_ERROR 6
#define EVALUATION_ERROR 7
#define CONSISTENCY_ERROR 8
#define RESOURCE_ERROR 9
#define UNINSTANTIATION_ERROR 10
#define INTERNAL_ERROR 11 /* [PM] 4.0 Never seen by prolog, only used with DBG */


/* Some types, domains, etc. */
/* xref Bips/intrins1.pl */
#define TYPE_ATOMIC (1<<8)	/* unused */
#define TYPE_ATOM (2<<8)
#define TYPE_NUMBER (3<<8)
#define TYPE_INTEGER (4<<8)
#define TYPE_COMPOUND (5<<8)	/* unused */
#define TYPE_VAR (6<<8)
#define TYPE_SPCODES (7<<8)
#define TYPE_FLOAT (8<<8)
#define TYPE_EVALUABLE (9<<8)

#define DOMAIN_1_ARITY (1<<8)	/* unused */
#define DOMAIN_CHAR (2<<8)
#define DOMAIN_NONZERO (3<<8)	/* unused */
#define DOMAIN_GT_ZERO (4<<8)	/* unused */
#define DOMAIN_GE_ZERO (5<<8)
#define DOMAIN_EXPRESSION (6<<8) /* unused */
#define DOMAIN_LIST (7<<8)	/* used in library(tcltk) */
#define DOMAIN_MUTABLE (8<<8)	/* unused */
#define DOMAIN_STREAM (9<<8)
#define DOMAIN_INPUT_STREAM (10<<8) /* unused */
#define DOMAIN_OUTPUT_STREAM (11<<8) /* unused */
#define DOMAIN_ATTR_MODULE (12<<8)   /* unused */
#define DOMAIN_IO_DESCR_STREAM (13<<8)	/* unused */
#define DOMAIN_ABS_LE_ONE (14<<8)	/* unused */
#define DOMAIN_ABS_GE_ONE (15<<8)	/* unused */
#define DOMAIN_GE_ONE (16<<8)	/* unused */
#define DOMAIN_GT_FZERO (17<<8)	/* unused */
#define DOMAIN_GE_FZERO (18<<8)	/* unused */
/* [PM] 4.4.0 */
#define DOMAIN_INT (19<<8)
#define DOMAIN_INT64 (20<<8)
#define DOMAIN_INT32 (21<<8)
#define DOMAIN_INT16 (22<<8)
#define DOMAIN_INT8 (23<<8)
#define DOMAIN_UINT (24<<8)
#define DOMAIN_UINT64 (25<<8)
#define DOMAIN_UINT32 (26<<8)
#define DOMAIN_UINT16 (27<<8)
#define DOMAIN_UINT8 (28<<8)


#define REPR_MAXARITY (1<<8)	/* unused */
#define REPR_PATHLEN (2<<8)	/* unused */
#define REPR_NAN (3<<8)
#define REPR_WORD (4<<8)	/* unused */
#define REPR_MISENC (5<<8)
#define REPR_MAXATOM (6<<8)
#define REPR_INT (9<<8)
#define REPR_INT64 (10<<8)
#define REPR_INT32 (11<<8)
#define REPR_INT16 (12<<8)
#define REPR_INT8 (13<<8)
#define REPR_UINT (14<<8)
#define REPR_UINT64 (15<<8)
#define REPR_UINT32 (16<<8)
#define REPR_UINT16 (17<<8)
#define REPR_UINT8 (18<<8)
#define REPR_FLI_INT (19<<8)
#define REPR_ADDRESS (20<<8)	/* STRUCTS_XXX */
#define REPR_OFFSET (21<<8)	/* STRUCTS_XXX */

#define EXIST_EOF (1<<8)	/*  */
#define EXIST_PREDICATE (2<<8)	/* unused */
#define EXIST_FUNCTION (3<<8)	/* unused */
#define EXIST_RESOURCE (4<<8)
#define EXIST_FILE (5<<8)

#define PERM_READ_STREAM (1<<8)
#define PERM_WRITE_STREAM (2<<8)
#define PERM_READ_FILE (3<<8)
#define PERM_WRITE_FILE (4<<8)
#define PERM_SEEK (5<<8)
#define PERM_ASS_MULTIFILE (6<<8)
#define PERM_EOF (7<<8)

#define CONS_RESTORE (1<<8)

#define EVAL_FLOAT_OFLO (1<<8)
#define EVAL_UNDEFINED (2<<8)
#define EVAL_ZERO_DIV (3<<8)
#define EVAL_FLOAT_NOT_A_NUMBER (4<<8)

#define RES_MEMORY (1<<8)

#if SP_WIN32
#define SP_syserror_win32(PRED,CALL) sp_raise_win32_error(GetLastError(), PRED, CALL)
#else  /* !SP_WIN32 (a.k.a. UNIX)  */
#define SP_syserror_errno(PRED,CALL) sp_raise_errno_error(errno, PRED, CALL)
#endif  /* !SP_WIN32 */


/*----------------------------------------------------------------------*/

/* Options for SP_expand_file_name */
/* Return slash terminated path */
#define SP_EXPAND_FILE_NAME_OPTION_DIR        0x0001
/* do not accept relative path as input */
#define SP_EXPAND_FILE_NAME_OPTION_NO_CWD     0x0002
/* Do not expand environment variable */
#define SP_EXPAND_FILE_NAME_OPTION_NO_ENV     0x0004
/* Do not expand ~ or ~user */
#define SP_EXPAND_FILE_NAME_OPTION_NO_HOME    0x0008
/* If !SP_EXPAND_FILE_NAME_OPTION_DIR root dir becomes "/.", "c:/." etc */
#define SP_EXPAND_FILE_NAME_OPTION_ROOT_DOT   0x00010
/* If !SP_EXPAND_FILE_NAME_OPTION_DIR root dir gives error */
#define SP_EXPAND_FILE_NAME_OPTION_ROOT_SLASH 0x0020

#define SP_EXPAND_FILE_NAME_OPTION_NO_CASE_NORMALIZATION 0x0040

/* Options for SP_get_stream_counts */
#define SP_GET_STREAM_COUNTS_OPTION_READ  0x0001
#define SP_GET_STREAM_COUNTS_OPTION_WRITE 0x0002

/* [PM] 4.1 Options for SP_get_system_property() */
#define SP_GET_SYSTEM_PROPERTY_OPTION_EXCLUDE_ENVIRONMENT 0x0001
#define SP_GET_SYSTEM_PROPERTY_OPTION_EXCLUDE_PROPERTIES  0x0002

/* [PM] 4.1 Options for SP_system_properties() */
#define SP_SYSTEM_PROPERTIES_OPTION_NUL_SEPARATED       0x0001
#define SP_SYSTEM_PROPERTIES_OPTION_INCLUDE_ENVIRONMENT 0x0002
#define SP_SYSTEM_PROPERTIES_OPTION_EXCLUDE_PROPERTIES  0x0004


/*----------------------------------------------------------------------*/

/* [PM] 3.9 runtime system entry point (as called by spld-generated code)*/
extern int SPCDECL user_main(int argc, char *argv[]);
/* [PM] 3.9 xref charmain.c. Called from spld-generated code */
extern int SPCDECL sp_main_internal(int argc, char **argv);

#if SICSTUS_OLDHOOKS
/* [PM] 3.9 runtime system user hook (as called by spld-generated code) */
extern int SPCDECL SU_initialize(int argc, char *argv[]);
#endif  /* SICSTUS_OLDHOOKS */


#if !SPIO_INIT_OPTION_TYPE_DEFINED
#ifndef SP_DEFINE_SPIO_INIT_OPTION_TYPE
#define SP_DEFINE_SPIO_INIT_OPTION_TYPE 1
#endif  /* SP_DEFINE_SPIO_INIT_OPTION_TYPE */

#if SP_DEFINE_SPIO_INIT_OPTION_TYPE
enum spio_t_init_option_type_ {
  SPIO_INIT_OPTION_TYPE_INVALID=0,
  SPIO_INIT_OPTION_TYPE_NULL=1,
  /* [PM] 4.1.3 the spio_t_init_option type is a spio_t_init_option_system_property */
  SPIO_INIT_OPTION_TYPE_SYSTEM_PROPERTY=2,
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


#endif  /* SP_DEFINE_SPIO_INIT_OPTION_TYPE */

#endif /* !SPIO_INIT_OPTION_TYPE_DEFINED */

#if SPIO_INIT_OPTION_TYPE_DEFINED

/* [PM] 4.1.3 options to SP_set_initial_options and SP_initialize() */
#define SP_option_type_invalid SPIO_INIT_OPTION_TYPE_INVALID
#define SP_option_type_null SPIO_INIT_OPTION_TYPE_NULL
/* [PM] 4.1 the SP_option type is a SP_option_system_property */
#define SP_option_type_system_property SPIO_INIT_OPTION_TYPE_SYSTEM_PROPERTY
  /* [PM] 4.1 legacy */
#define SP_option_type_environment_property SP_option_type_system_property

typedef spio_t_init_option_type SP_option_type;

/* type==SP_option_type_system_property */
typedef spio_t_init_option_system_property SP_option_system_property;

/* [PM] 4.1 legacy */
typedef SP_option_system_property SP_option_environment_property;

typedef spio_t_init_option SP_option;

#else  /* !SPIO_INIT_OPTION_TYPE_DEFINED */
/* [PM] 3.11.2 WinCE options to SP_set_initial_options */
enum SP_option_type_ {
  SP_option_type_invalid=0,
  SP_option_type_null=1,
  /* [PM] 4.1 the SP_option type is a SP_option_system_property */
  SP_option_type_system_property=2,
#define SP_option_type_system_property SP_option_type_system_property
  /* [PM] 4.1 legacy */
  SP_option_type_environment_property = SP_option_type_system_property
};
typedef enum SP_option_type_ SP_option_type;

/* type=SP_option_type_system_property */
typedef struct SP_option_system_property_ SP_option_system_property;
struct SP_option_system_property_ {
  char const *key;              /* SICStus internal encoding (UTF-8) */
  char const *value;            /* SICStus internal encoding (UTF-8) */
};

/* [PM] 4.1 legacy */
typedef SP_option_system_property SP_option_environment_property;

typedef struct SP_option_ SP_option;
struct SP_option_ {
  SP_option_type type;
  union SP_option_u_ {
    void *reserved;
    SP_option_system_property prop;
  } u;
};
#endif  /* !SPIO_OPTION_TYPE_DEFINED */

typedef struct SP_options_ SP_options;
struct SP_options_ {
  SP_integer version;
  size_t noptions;
  SP_option *options; /* noptions elements */
};


#define SP_OPTIONS_VERSION_4_1_0 0x040100 /* New format in 4.1 */
#define SP_OPTIONS_VERSION_4_0_1 0x040001 /* introduced in 4.0.1 */
/* [PM] 4.1 Internal note: for new code (SP_OPTIONS_VERSION & 0xFF)
   must always be 0x00 (0x01 allowed for legacy reasons for
   SP_OPTIONS_VERSION_4_0_1). See glue_initialize__int(). */
#define SP_OPTIONS_VERSION SP_OPTIONS_VERSION_4_1_0
#define SP_OPTIONS_STATIC_INITIALIZER {SP_OPTIONS_VERSION, 0, NULL}

/*----------------------------------------------------------------------*/

struct SICSTUS_API_STRUCT;      /* forward declaration */
/*
  The following typedef for SP_get_dispatch_type expose more than it
  should. This is due to a need to forward declare this type before
  SICSTUS_API_STRUCT_TYPE is defined. You should pretend that it
  instead looked like:

typedef SICSTUS_API_STRUCT_TYPE * SPCDECL SP_get_dispatch_type(void *reserved);

*/
typedef struct SICSTUS_API_STRUCT * SPCDECL SP_get_dispatch_type(void *reserved /* should be NULL */);

struct SP_multi_sp_state;   /* [PM] 3.9b4 forward declaration */

/*----------------------------------------------------------------------*/
/* External function declarations, spaux.h contains generated
   definitions which are used on all platforms. Because of
   this, *ALL FUNCTION DECLARATIONS*, and *NOTHING ELSE*, must be inside
   the following else-endif.
   In particular, any typedefs or macros need to go above.
*/

   /* spaux declares all functions either directly or via dispatch so
      the extern declarations below are never seen by any code,
      they are just input to transhdr.pl */
#if LOCAL_INCLUDES
#include "spaux.h"
#else /* ! LOCAL_INCLUDES */
#include <sicstus/spaux.h>
#endif /* ! LOCAL_INCLUDES */
#undef SP_NO_PRIVATE_CONFIG     /* after the last include */
/* opaque for user code */
struct sp_fli_info;

struct SP_MAINFUN_PARAMS_STRUCT {
  SP_integer version;        /* This field must be first (SP_MAINFUN_PARAMS_VERSION) */
  /* The rest of the fields may vary when version is changed */
  SPEnv *spenv;               /* contains api and stash */

  int flags;                  /* in, out */
#define SP_MAINFUN_PARAM_FLAG_IN_ENTER    0x01 /* in -- about to load the resource */
#define SP_MAINFUN_PARAM_FLAG_IN_EXIT     0x02 /* in -- about to unload the resource */
#define SP_MAINFUN_PARAM_FLAG_IN_GLUE     0x04 /* in -- runtime expects splfr-generated glue (SP_FLI_APPLY_ASM_GENERIC) */
#define SP_MAINFUN_PARAM_FLAG_OUT_PLAIN   0x08 /* out -- foreign functions does not take a stash argument */
#define SP_MAINFUN_PARAM_FLAG_OUT_MULTI   0x10 /* out -- foreign functions takes a stash argument (i.e., multi-sp-aware) */
#define SP_MAINFUN_PARAM_FLAG_OUT_GLUE    0x20 /* out -- foreign functions wrapped by splfr-generated glue (SP_FLI_APPLY_ASM_GENERIC) */

#if !SP_MAINFUN_PARAM_FLAG_NO_LEGACY_NAMES
  /* [PM] 4.2.1+ legacy names */
#define SP_MAINFUN_PARAM_FLAG_ENTER  SP_MAINFUN_PARAM_FLAG_IN_ENTER  
#define SP_MAINFUN_PARAM_FLAG_EXIT   SP_MAINFUN_PARAM_FLAG_IN_EXIT   
#define SP_MAINFUN_PARAM_FLAG_PLAIN  SP_MAINFUN_PARAM_FLAG_OUT_PLAIN 
#define SP_MAINFUN_PARAM_FLAG_MULTI  SP_MAINFUN_PARAM_FLAG_OUT_MULTI 
#endif                             /* !SP_MAINFUN_PARAM_FLAG_NO_LEGACY_NAMES */

#if SP_FLI_APPLY_ASM_GENERIC
  const SP_GenericGlueFunPtr  *glue_funcs; /* out */
#else                          /* !SP_FLI_APPLY_ASM_GENERIC */
  const SP_GlueFunPtr  *funcs; /* out */
#endif                            /* !SP_FLI_APPLY_ASM_GENERIC */

  char const*          *prednames; /* out */
  const int            *arities;  /* out */

  SP_InitFunUnion init_fun;   /* out */
  SP_InitFunUnion deinit_fun; /* out */
};


extern
#if INCLUDED_FROM_RUNTIME
#if defined(_MSC_VER)
__declspec(dllexport)
#elif SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
__attribute__ ((__visibility__ ("default")))
#endif
#endif /* INCLUDED_FROM_RUNTIME */
SP_get_dispatch_type SP_get_dispatch;

/* [PM] The declarations below are never seen directly by C. They are
   parsed by transhdr.pl and emitted into spaux.h */

#if 0                           /* [PM] 4.0.5 DO NOT REMOVE! Parsed by transhdr.pl */

/*----------------------------------------------------------------------*/
/* Preparation */

/* [PM] 3.11.2 WinCE call this before SP_initialize/sp_glue_initialize to set-up options */
extern SPEXP(123) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_OLDHOOKS int SPCDECL SP_set_initial_options PROTOTYPE((SP_options const *));

/* Should not be hidded */
/* [PM] 4.1 the third arg is now a pointer to options, it used to be a void* reserved. */
extern SPEXP(1) SPEXPFLAG_PREINIT int SPCDECL sp_glue_initialize PROTOTYPE((int, char **, SP_options const *, SP_MainFun **, char **, int, int));

extern SPEXP(2) SPEXPFLAG_PREINIT void SPCDECL SP_deinitialize PROTOTYPE((void));
extern SPEXP(3) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS void SPCDECL SP_force_interactive PROTOTYPE((void));

extern SPEXP(191) int SPCDECL SP_set_argv PROTOARGS((int argc, char **argv_ienc, spio_t_bits options));

extern SPEXP(4) SPEXPFLAG_UNAVAILABLE SPEXPFLAG_HIDDEN int SPCDECL SP_is_interactive PROTOTYPE((void));
extern SPEXP(5) int SPCDECL SP_load PROTOTYPE((char const *));
extern SPEXP(6) SP_pred_ref SPCDECL SP_predicate PROTOTYPE((char const *,SP_integer,char const *));
extern SPEXP(7) SP_pred_ref SPCDECL SP_pred PROTOTYPE((SP_atom,SP_integer,SP_atom));
extern SPEXP(8) SP_term_ref SPCDECL SP_new_term_ref PROTOTYPE((void));
extern SPEXP(9) int SPCDECL SP_new_term_refs PROTOTYPE((int));
extern SPEXP(10) SPEXPFLAG_HIDDEN void SPCDECL SP_reset_term_refs PROTOTYPE((int));

extern SPEXP(11) SPEXPFLAG_HIDDEN SP_globref SPCDECL SP_alloc_globrefs PROTOTYPE((int));
extern SPEXP(12) SPEXPFLAG_HIDDEN void SPCDECL SP_free_globrefs PROTOTYPE((SP_globref,int));


/*----------------------------------------------------------------------*/
/* Control */

extern SPEXP(13) int SPCDECL SP_query PROTOARGS((SP_pred_ref predicate, ...));
extern SPEXP(14) int SPCDECL SP_query_cut_fail PROTOARGS((SP_pred_ref predicate, ...));
extern SPEXP(15) SP_qid SPCDECL SP_open_query PROTOARGS((SP_pred_ref predicate, ...));
extern SPEXP(16) SP_qid SPCDECL SP_open_query_array PROTOTYPE((SP_pred_ref, SP_term_ref *));
extern SPEXP(17) int SPCDECL SP_next_solution PROTOTYPE((SP_qid));
extern SPEXP(18) int SPCDECL SP_cut_query PROTOTYPE((SP_qid));
extern SPEXP(19) int SPCDECL SP_close_query PROTOTYPE((SP_qid));
extern SPEXP(20) int SPCDECL SP_event PROTOTYPE((SP_EventFun *, void *));
extern SPEXP(106) spio_t_error_code SPCDECL SP_schedule_async_event PROTOARGS((sp_t_async_event *async_event, spio_t_bits options));
extern SPEXP(192) spio_t_error_code SPCDECL SP_alloc_async_event PROTOARGS((sp_t_async_event **pasync_event, sp_t_async_event_fun *fun, void *cookie, spio_t_bits options));
extern SPEXP(128) void SPCDECL SP_free_async_event PROTOARGS((sp_t_async_event *async_event));

extern SPEXP(22) SPEXPFLAG_HIDDEN SP_term SPCDECL sp_ref_term PROTOARGS((SP_term_ref ref));

/*----------------------------------------------------------------------*/
/* SP_put_* */

extern SPEXP(26) int SPCDECL SP_put_variable PROTOTYPE((SP_term_ref));
extern SPEXP(27) int SPCDECL SP_put_term PROTOTYPE((SP_term_ref, SP_term_ref));
extern SPEXP(28) int SPCDECL SP_put_integer PROTOTYPE((SP_term_ref, SP_integer));
extern SPEXP(29) int SPCDECL SP_put_float PROTOTYPE((SP_term_ref, double));
extern SPEXP(30) int SPCDECL SP_put_atom PROTOTYPE((SP_term_ref, SP_atom));
extern SPEXP(31) int SPCDECL SP_put_string PROTOTYPE((SP_term_ref, char const *));
extern SPEXP(32) int SPCDECL SP_put_address PROTOTYPE((SP_term_ref, void *));
extern SPEXP(33) int SPCDECL SP_put_list_codes PROTOTYPE((SP_term_ref, SP_term_ref, char const *));
extern SPEXP(34) int SPCDECL SP_put_number_codes PROTOTYPE((SP_term_ref, char const *));
extern SPEXP(35) int SPCDECL SP_put_functor PROTOTYPE((SP_term_ref, SP_atom, int));
extern SPEXP(36) int SPCDECL SP_put_list PROTOTYPE((SP_term_ref));
extern SPEXP(37) int SPCDECL SP_cons_functor PROTOARGS((SP_term_ref term, SP_atom name, int arity, ...));
extern SPEXP(38) int SPCDECL SP_cons_list PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));

extern SPEXP(39) int SPCDECL SP_read_from_string PROTOARGS((SP_term_ref term, char const *string, SP_term_ref *values));
extern SPEXP(190) SPEXPFLAG_HIDDEN int SPCDECL SP_stream_from_string PROTOARGS((char const *string, SP_stream **pstream));

/*----------------------------------------------------------------------*/
/* SP_get_* */

extern SPEXP(40) int SPCDECL SP_get_integer PROTOTYPE((SP_term_ref, SP_integer *));
extern SPEXP(41) int SPCDECL SP_get_float PROTOTYPE((SP_term_ref, double *));
extern SPEXP(42) int SPCDECL SP_get_atom PROTOTYPE((SP_term_ref, SP_atom *));
extern SPEXP(43) int SPCDECL SP_get_string PROTOTYPE((SP_term_ref, char const **));
extern SPEXP(44) int SPCDECL SP_get_address PROTOTYPE((SP_term_ref, void * *));
extern SPEXP(45) int SPCDECL SP_get_list_codes PROTOTYPE((SP_term_ref, char const **));
extern SPEXP(46) int SPCDECL SP_get_number_codes PROTOTYPE((SP_term_ref, char const **));
extern SPEXP(47) int SPCDECL SP_get_functor PROTOTYPE((SP_term_ref, SP_atom *, int *));
extern SPEXP(48) int SPCDECL SP_get_list PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));
extern SPEXP(49) int SPCDECL SP_get_arg PROTOTYPE((int, SP_term_ref, SP_term_ref));



/*----------------------------------------------------------------------*/
/* SP_is_* */

extern SPEXP(50) int SPCDECL SP_term_type PROTOTYPE((SP_term_ref));
extern SPEXP(51) int SPCDECL SP_is_variable PROTOTYPE((SP_term_ref));
extern SPEXP(52) int SPCDECL SP_is_integer PROTOTYPE((SP_term_ref));
extern SPEXP(53) int SPCDECL SP_is_float PROTOTYPE((SP_term_ref));
extern SPEXP(54) int SPCDECL SP_is_atom PROTOTYPE((SP_term_ref));
extern SPEXP(55) int SPCDECL SP_is_compound PROTOTYPE((SP_term_ref));
extern SPEXP(56) int SPCDECL SP_is_list PROTOTYPE((SP_term_ref));
extern SPEXP(57) int SPCDECL SP_is_atomic PROTOTYPE((SP_term_ref));
extern SPEXP(58) int SPCDECL SP_is_number PROTOTYPE((SP_term_ref));


/*----------------------------------------------------------------------*/
/* Others */

extern SPEXP(59) SP_atom SPCDECL SP_atom_from_string PROTOTYPE((char const *));
extern SPEXP(60) SP_atom SPCDECL SP_existing_atom_from_string PROTOTYPE((char const *));
extern SPEXP(61) char const* SPCDECL SP_string_from_atom PROTOTYPE((SP_atom));
extern SPEXP(62) int SPCDECL SP_unify PROTOTYPE((SP_term_ref,SP_term_ref));
extern SPEXP(63) int SPCDECL SP_compare PROTOTYPE((SP_term_ref,SP_term_ref));
extern SPEXP(64) int SPCDECL SP_exception_term PROTOTYPE((SP_term_ref));
extern SPEXP(65) void SPCDECL SP_raise_exception PROTOTYPE((SP_term_ref));
extern SPEXP(66) void SPCDECL SP_fail PROTOTYPE((void));
extern SPEXP(67) char const * SPCDECL SP_error_message PROTOTYPE((int));
extern SPEXP(68) int SPCDECL SP_get_errno PROTOTYPE((void));


/*----------------------------------------------------------------------*/
/* Streams */

extern SPEXP(69) SP_stream* SPCDECL SP_get_stdin PROTOTYPE((void));
extern SPEXP(70) SP_stream* SPCDECL SP_get_stdout PROTOTYPE((void));
extern SPEXP(71) SP_stream* SPCDECL SP_get_stderr PROTOTYPE((void));
extern SPEXP(72) SP_stream* SPCDECL SP_get_curin PROTOTYPE((void));
extern SPEXP(73) SP_stream* SPCDECL SP_get_curout PROTOTYPE((void));


extern SPEXP(74) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS SP_UserStreamHook * SPCDECL SP_set_user_stream_hook PROTOTYPE((SP_UserStreamHook *, void *));
extern SPEXP(75) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS SP_UserStreamPostHook * SPCDECL SP_set_user_stream_post_hook PROTOTYPE((SP_UserStreamPostHook *, void *));

extern SPEXP(76) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE SPEXPFLAG_OLDHOOKS SP_ErrprintfHook * SPCDECL SP_set_errprintf_hook PROTOTYPE((SP_ErrprintfHook *, void *));

/* SP_set_memalloc_hooks() was deprecated in SP 4.5.0, and completely removed in 4.7.0. */
extern SPEXP(77) SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE int SPCDECL
SP_set_memalloc_hooks PROTOARGS((int hints,
				 SP_InitAllocHook *init_alloc_hook,
				 SP_DeinitAllocHook *deinit_alloc_hook,
				 SP_AllocHook *alloc_hook,
				 SP_FreeHook *free_hook,
                                 void *cookie));

/* SPIO stream API */

extern SPEXP(78) SPEXPFLAG_SPIO
       spio_t_error_code SPCDECL SP_create_stream PROTOARGS((void *user_data,
                                                             void const *user_class,
                                                             spio_t_simple_device_read *user_read,
                                                             spio_t_simple_device_write *user_write,
                                                             spio_t_simple_device_flush_output *user_flush_output,
                                                             spio_t_simple_device_seek *user_seek,
                                                             spio_t_simple_device_close *user_close,
                                                             spio_t_simple_device_interrupt *user_interrupt,
                                                             spio_t_simple_device_ioctl *user_ioctl,
                                                             void *args,
                                                             spio_t_bits create_options,
                                                             SP_stream **pstream));

extern SPEXP(24) SPEXPFLAG_SPIO
       spio_t_error_code SPCDECL SP_get_stream_user_data PROTOARGS((SP_stream *stream, void const *user_class, void **puser_data));


extern SPEXP(25) SPEXPFLAG_SPIO
       spio_t_error_code SPCDECL SP_get_stream_counts PROTOARGS((SP_stream *stream,
                                                                 spio_t_offset *item_offset,
                                                                 spio_t_offset *newline_count,
                                                                 spio_t_offset *line_length,
                                                                 spio_t_bits option));


extern SPEXP(23) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_create_os_stream PROTOARGS((spio_t_os_file_handle hFile, SP_stream **pstream, void *arg, spio_t_bits options));

/* These are used by SP_{get,put}_{byte,code} and should not be hidded (but are not publicly documented) */
extern SPEXP(79) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_get_byte_helper PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(80) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_put_byte_helper PROTOARGS((SP_stream *s, spio_t_uint8 byte, spio_t_bits options));
extern SPEXP(81) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_get_code_helper PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(82) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_put_code_helper PROTOARGS((SP_stream *s, spio_t_wchar code, spio_t_bits options));

extern SPEXP(83) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_printf PROTOARGS((char const *fmt, ...)) SPEXP_ATTRIBUTES(SPEXP_ATTRIBUTE__FORMAT_1_2);
extern SPEXP(84) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_fprintf PROTOARGS((SP_stream *stream, char const *fmt, ...)) SPEXP_ATTRIBUTES(SPEXP_ATTRIBUTE__FORMAT_2_3);
extern SPEXP(85) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL sp_ttyprintf PROTOARGS((char const *fmt, ...)) SPEXP_ATTRIBUTES(SPEXP_ATTRIBUTE__FORMAT_1_2);
extern SPEXP(184) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL sp_ttyflush PROTOARGS((void));

extern SPEXP(86) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_put_bytes PROTOARGS((SP_stream *s, spio_t_uint8 const *bytes, size_t byte_count, spio_t_bits options));
extern SPEXP(87) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_put_codes PROTOARGS((SP_stream *s, spio_t_wchar const *codes, size_t code_count, spio_t_bits options));
extern SPEXP(88) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_put_encoded_string PROTOARGS((SP_stream *s, char const *string, spio_t_bits options));

extern SPEXP(89) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_flush_output PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(21) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_fopen PROTOARGS((char const *path_ienc, void *reserved, spio_t_bits options, SP_stream **ps));
extern SPEXP(90) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_fclose PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(91) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_unget_code PROTOTYPE((SP_stream*, int));
extern SPEXP(92) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_unget_byte PROTOTYPE((SP_stream*, int));

extern SPEXP(138) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_next_stream PROTOARGS((SP_stream *s, SP_stream **pnext));

extern SPEXP(139) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN SPEXPFLAG_UNIX spio_t_error_code SPCDECL
                  SP_select PROTOARGS((int read_fds[], size_t *pnread_fds,
                                       int write_fds[], size_t *pnwrite_fds,
                                       SP_stream * read_streams[], size_t *pnread_streams,
                                       SP_stream * write_streams[], size_t *pnwrite_streams,
                                       spio_t_timespec *timeout,
                                       spio_t_bits options
                                       ));

extern SPEXP(139) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN SPEXPFLAG_WIN32 spio_t_error_code SPCDECL
                  SP_select PROTOARGS((spio_t_os_file_handle os_events[], size_t *pnos_events,
                                       SP_stream * read_streams[], size_t *pnread_streams,
                                       SP_stream * write_streams[], size_t *pnwrite_streams,
                                       spio_t_timespec *timeout,
                                       spio_t_bits options
                                       ));

extern SPEXP(140) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_open_socket_stream PROTOARGS((char const *nodename, char const *servname, void *args, SP_stream **pstream, spio_t_bits options));



/*----------------------------------------------------------------------*/

/* signal() replacement */
extern SPEXP(93) SP_SigFun* SPCDECL SP_signal PROTOTYPE((int, SP_SigFun *, void *));
extern SPEXP(127) spio_t_error_code SPCDECL SP_sigaction PROTOARGS((int signo, SP_SigFun *func, void *user_data, spio_t_bits options));

/* [PM] WinCE tell SP that a signal happened. Not yet public. (change
   name to SP_raise_signal when going public) */
extern SPEXP(94) SPEXPFLAG_HIDDEN int SPCDECL sp_raise_signal PROTOARGS((int signo));

extern SPEXP(95) int SPCDECL SP_define_c_predicate PROTOARGS((char const *name, int arity, char const *module, SP_CPredFun *proc, void *stash));

/* [PM] 4.1 legacy. Use SP_system_properties() instead, if at all. */
extern SPEXP(96) SPEXPFLAG_HIDDEN char** SPCDECL SP_copy_environ PROTOTYPE((void));
/* [PM] See library/system.c for how to use this, if at all */
extern SPEXP(193) spio_t_error_code SPCDECL SP_system_properties PROTOARGS((char ***pprops, spio_t_bits options));
/*----------------------------------------------------------------------*/
/* Undocumented. The following definitions should be avoided if possible. */

extern SPEXP(97) SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE int SPCDECL SP_action PROTOTYPE((int, void *)); /* Perform some action */
extern SPEXP(98) SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE SP_integer SPCDECL SP_inquiry PROTOTYPE((int)); /* Get information from Prolog */
  /* extern SPEXP int SPCDECL SP_choice_offset PROTOTYPE((int)); */
  /* extern SPEXP void SPCDECL SP_put_choice_argument PROTOTYPE((int offset,int,SP_term_ref)); */

extern SPEXP(99) SPEXPFLAG_HIDDEN void SPCDECL SP_write_term PROTOTYPE((SP_term_ref));


/*----------------------------------------------------------------------*/
/* Misc */

extern SPEXP(100) SPEXPFLAG_UNAVAILABLE SPEXPFLAG_HIDDEN int SPCDECL SP_isatty PROTOTYPE((SP_stream *));
extern SPEXP(101) void * SPCDECL SP_malloc PROTOTYPE((size_t));
extern SPEXP(102) void * SPCDECL SP_realloc PROTOTYPE((void *, size_t));
extern SPEXP(103) void SPCDECL SP_free PROTOTYPE((void *));

/* [PM] 4.0 gone */
extern SPEXP(104) SPEXPFLAG_UNAVAILABLE void * SPCDECL SP_memmove PROTOARGS((void *dest, void *src, int n));

/*----------------------------------------------------------------------*/
/* Raising standard errors from C-code */

/* SP_save_error() followed by SP_raise_error() will raise a standard
   error exception according to the constants defined below.
*/

/* [PM] 4.1.2 SP_save_error and SP_raise_error were never
   documented. They are fundamentally broken and should not be
   used. Use SP_raise_exception() instead. */
/* extern SPEXP(105) SPEXPFLAG_HIDDEN void SPCDECL SP_save_error PROTOARGS((int error_type, char const *error_msg_ienc, SP_term_ref culprit)); */
/* extern SPEXP(106) SPEXPFLAG_HIDDEN void SPCDECL SP_raise_error PROTOARGS((char const *module, char const *pred, int arity, int argn)); */
/* [PM] 4.3 Corresponds to the old SP_save_error()+SP_raise_error()
   idiom. New code should not use this, use SP_raise_exception()
   instead. Not public. */
extern SPEXP(105) SPEXPFLAG_HIDDEN void SPCDECL SP_save_and_raise_error PROTOARGS((int error_type, char const *error_msg_ienc, SP_term_ref culprit, char const *module, char const *pred, int arity, int argn));

extern SPEXP(107) SPEXPFLAG_HIDDEN SPEXPFLAG_UNIX void SPCDECL sp_raise_errno_error PROTOARGS((int errno_value, char const *ienc_pred, char const *ienc_syscall_name));
extern SPEXP(107) SPEXPFLAG_HIDDEN SPEXPFLAG_WIN32 void SPCDECL sp_raise_win32_error PROTOARGS((int gle_value, char const *ienc_pred, char const *ienc_syscall_name));

extern SPEXP(108) size_t SPCDECL SP_atom_length PROTOTYPE((SP_atom));
extern SPEXP(188) int SPCDECL SP_put_list_n_bytes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, unsigned char const *));
extern SPEXP(189) int SPCDECL SP_get_list_n_bytes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, size_t *, unsigned char *));
extern SPEXP(109) int SPCDECL SP_put_list_n_codes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, char const *));
extern SPEXP(110) int SPCDECL SP_get_list_n_codes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, size_t *, char *));
extern SPEXP(111) int SPCDECL SP_register_atom PROTOTYPE((SP_atom));
extern SPEXP(112) int SPCDECL SP_unregister_atom PROTOTYPE((SP_atom));

extern SPEXP(113) int SPCDECL SP_cons_functor_array PROTOARGS((SP_term_ref term, SP_atom name, int arity, SP_term_ref *args));
extern SPEXP(114) int SPCDECL SP_restore PROTOTYPE((char const *));
/* extern SPEXP(115) int SPCDECL SP_chdir PROTOTYPE((char const *)); */
/* extern SPEXP(116) char * SPCDECL SP_getcwd PROTOTYPE((char *,unsigned int)); */
/* [PM] 4.0 path uses internal (UTF-8) encoding. */
extern SPEXP(115) spio_t_error_code SPCDECL SP_set_current_dir PROTOTYPE((char const *));
/* [PM] 4.0 Returned path uses internal (UTF-8) encoding and is slash terminated. Returns NULL on failure. */
extern SPEXP(116) char * SPCDECL SP_get_current_dir PROTOTYPE((void));
/* [PM] 4.0 expand relative path */
extern SPEXP(174) spio_t_error_code SPCDECL SP_expand_file_name PROTOARGS((char const *relpath, char const *cwd, spio_t_bits options, char **pabspath));


/* support for SP_on_fault */
extern SPEXP(117) void SPCDECL SP_raise_fault PROTOARGS((char const *message_ienc));
/* Should not be hidded but is not documented */
extern SPEXP(118) void SPCDECL sp_set_abort_env PROTOTYPE((void *));
/* Should not be hidded but is not documented */
extern SPEXP(119) char const * SPCDECL sp_get_abort_err_ienc PROTOTYPE((void));

extern SPEXP(120) SPEXPFLAG_HIDDEN char const * SPCDECL sp_get_boot_path PROTOTYPE((void));
  /* extern SPEXP void * SPCDECL sp_get_self PROTOTYPE((void)); */
  /* extern SPEXP void SPCDECL sp_sigemptyset PROTOTYPE((void)); */

/* Returns a pointer to the version string */
extern SPEXP(121) char const * SPCDECL SP_get_emulator_version PROTOTYPE((void));
/* [PM] 3.9b4 Returns a pointer to the version specific path component, e.g., sicstus-3.9.0beta4 */
extern SPEXP(122) char const * SPCDECL SP_get_emulator_dir PROTOTYPE((void));

extern SPEXP(124) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_OLDHOOKS SP_SetWindowTitleHook * SPCDECL SP_set_set_window_title_hook PROTOTYPE((SP_SetWindowTitleHook *, void *));
extern SPEXP(125) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_OLDHOOKS SP_GetWindowTitleHook * SPCDECL SP_set_get_window_title_hook PROTOTYPE((SP_GetWindowTitleHook *, void *));

  /* Internal API functions */
extern SPEXP(126) SPEXPFLAG_HIDDEN void * SPCDECL sp_get_engine_global PROTOTYPE((void));

extern SPEXP(129) SPEXPFLAG_HIDDEN int SPCDECL sp_qload_or_restore PROTOTYPE((char const *,int));
extern SPEXP(130) SPEXPFLAG_HIDDEN void SPCDECL sp_variable_to_string PROTOTYPE((SP_term_ref,char *));

/* [PM] 4.0 sp_get_classpath() now returns a SP_malloc'd path, caller should SP_free it */
extern SPEXP(132) SPEXPFLAG_HIDDEN char * SPCDECL sp_get_classpath PROTOTYPE((void));
/* [PM] 4.0 sp_get_failed_bootpath should be hidden but that does not work due to how sicstus.h is included in spld-generated glue */
extern SPEXP(133) char const * SPCDECL sp_get_failed_bootpath PROTOTYPE((void));

extern SPEXP(134) SPEXPFLAG_HIDDEN int SPCDECL sp_prolog_initialized PROTOTYPE((void));
extern SPEXP(135) SPEXPFLAG_HIDDEN int SPCDECL sp_is_development_system PROTOTYPE((void));

extern SPEXP(136) void * SPCDECL SP_calloc PROTOTYPE((size_t, size_t));
extern SPEXP(137) char * SPCDECL SP_strdup PROTOTYPE((char const *));


  /* WCI (alias Ienc) utilities for use in the foreign code */
extern SPEXP(141) SPEXPFLAG_HIDDEN char const * SPCDECL SP_to_os PROTOARGS((char const *ienc, int context));
extern SPEXP(142) SPEXPFLAG_HIDDEN char const * SPCDECL SP_from_os PROTOARGS((char const *senc, int context));

extern SPEXP(143) int SPCDECL SP_wci_code PROTOARGS((int *pcode, char const *wci));

  /* SP_wci_code() determines the number of bytes that comprise the
     internally encoded character pointed to by wci. Also, if pcode is not
     a null pointer, SP_wci_code() converts the internally encoded
     character to a wide character code and places the result in the object
     pointed to by pcode. (The value of the wide character corresponding to
     the NUL character is zero.)  At most WCI_MAX_BYTES bytes will be
     examined, starting at the byte pointed to by wci, but note that no
     bytes following a NUL character will be examined.

     If wci is a null pointer, SP_wci_code() simply returns 0.  If wci is
     not a null pointer, then, if wci points to the NUL character,
     SP_wci_code() returns 0; if the next bytes form a valid internally
     encoded character, SP_wci_code() returns the number of bytes that
     comprise the converted internally encoded character; otherwise, wci
     does not point to a valid internally encoded character and
     SP_wci_code() returns the negated length of the invalid byte sequence.
     This latter case will not happen, if wci points to the beginning of a
     Prolog atom string, or to a position within such a string reached by
     repeated addition of lengths (SP_wci_len) of the preceding encoded
     wide characters */


extern SPEXP(144) int SPCDECL SP_wci_len PROTOARGS((char const *wci));
  /*
     SP_wci_len() determines the number of bytes comprising the multi-
     byte character pointed to by wci.  It is equivalent to

          SP_wci_code(NULL, wci);
  */


extern SPEXP(145) int SPCDECL SP_code_wci PROTOARGS((char *wci, int code));

  /*
     SP_code_wci() determines the number of bytes needed to  represent
     the  internally encoded  character  corresponding  to  the code whose
     value is code, and, if wci is not a null pointer, stores  the
     internally encoded  character  representation in the array pointed to
     by wci.  At most WCI_MAX_BYTES bytes are stored.

     If wci is a null pointer, SP_code_wci() simply returns 0.  If wci  is
     not  a  null  pointer,  SP_code_wci()  returns -1 if the value of
     code does not correspond to a  valid  internally encoded  character;
     otherwise  it  returns the number of bytes that comprise the
     internally encoded character corresponding to the value of code.
  */

  /* A utility that may be useful in wcx_chartype implementation */

/* [PM] 4.0.1 was visible in 4.0.0 by mistake */
extern SPEXP(146) SPEXPFLAG_HIDDEN int SPCDECL SP_latin1_chartype PROTOARGS((int char_code));

/* extern SPEXP(175) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encode_chars PROTOARGS((void const *from_buf, size_t *psize_from, spio_t_charset_enum from_charset, void *to_buf, size_t *psize_to, spio_t_charset_enum to_charset, void *args, spio_t_bits options)); */


/*
  [PM] 3.9b4 Support for loading multiple SICStus runtimes into the same process.
 */

/* 
   Mutexes are typically initialized by static initialization to 0
   We could have defined :
   int SP_init_mutex(SP_mutex *pstore)
   {
      (*pstore) = 0;
      return 1;
   }
*/
extern SPEXP(147) int SPCDECL SP_mutex_lock PROTOARGS((SP_mutex *pmutex));
extern SPEXP(148) int SPCDECL SP_mutex_unlock PROTOARGS((SP_mutex *pmutex));
extern SPEXP(149) int SPCDECL SP_mutex_destroy PROTOARGS((SP_mutex *pmutex));
extern SPEXP(150) int SPCDECL SP_load_sicstus_run_time PROTOARGS((SP_get_dispatch_type **ppfunc, void **phandle));

extern SPEXP(151) SPEXPFLAG_HIDDEN int SPCDECL SP_set_multi_sp_state PROTOTYPE((struct SP_multi_sp_state *));


/* [PM] Mar 2000 External Object Functions.

   THE EXTERNAL OBJECTS API WILL CHANGE BEFORE SICSTUS 3.9 FINAL!!

   The external object routines provides a mechanism for encoding
   pointers to data external to the prolog system in such a way that a
   finalizer will be run when the data can no longer be referenced by
   prolog code. The major trick is to encode the pointer as an atom
   and to use the atom GC to detect when the atom is no longer
   referenced by prolog.

*/

/* Use this to put an external object into a term ref */
extern SPEXP(152) int SPCDECL SP_put_external_object PROTOARGS((SP_term_ref tObj, SP_external_object_link *pObj));

/* Use this to get an external object into a term ref */
extern SPEXP(153) int SPCDECL SP_get_external_object PROTOARGS((SP_term_ref tObj, SP_external_object_link **ppObj));


/* For the simple cases use this to put by wrapping in unary compound
   with FUNCTOR as constructor. */
extern SPEXP(154) int SPCDECL SP_external_object_default_putter_helper PROTOARGS((SP_term_ref tr, SP_external_object_link *obj, SP_atom functor));

/* For complex cases use this to put the object pointer as the first
   arg of the compound term. See external_object_default_putter_helper
   for how to use this.  */
extern SPEXP(155) int SPCDECL SP_put_external_object_link PROTOARGS((SP_term_ref tObj, SP_external_object_link *pObj));

/* Put a term representation of the link that will not prevent garbage collection. */
extern SPEXP(156) int SPCDECL SP_put_weak_external_object_link PROTOARGS((SP_term_ref tObj, SP_external_object_link *pObj));

/* Register a new type of external object */
extern SPEXP(157) SP_external_object_type SPCDECL SP_register_external_object_type PROTOARGS((SP_external_object_finalizer finalizer, SP_external_object_putter putter, void *type_data));
/* wrap some kind of pointer as an external object of a certain type */
extern SPEXP(158) SP_external_object_link * SPCDECL SP_register_external_object PROTOARGS((void* data,SP_external_object_type object_type));

extern SPEXP(159) void SPCDECL SP_unlink_external_object PROTOARGS((SP_external_object_link *pObj, int finalize));
extern SPEXP(160) void * SPCDECL SP_get_external_object_data PROTOTYPE((SP_external_object_link *));
extern SPEXP(161) SP_external_object_type SPCDECL SP_get_external_object_type PROTOTYPE((SP_external_object_link *));
extern SPEXP(162) void SPCDECL SP_finalize_external_object PROTOARGS((SP_term_ref tObj, SP_integer *existed));
extern SPEXP(163) SP_integer SPCDECL SP_garbage_collect_external_objects PROTOARGS((SP_external_object_type object_type));

/* [PM] 3.9b5 These are not public, they are here so that winmain.c and tkterm.c can use them */
extern SPEXP(164) SPEXPFLAG_HIDDEN void SPCDECL SP_ctrlc_action PROTOTYPE((void));
extern SPEXP(165) SPEXPFLAG_HIDDEN void SPCDECL SP_ctrlbreak_action PROTOTYPE((void));

extern SPEXP(166) SPEXPFLAG_HIDDEN int SPCDECL sp_set_jasper_magic PROTOTYPE((void*, int));
extern SPEXP(167) SPEXPFLAG_HIDDEN void* SPCDECL sp_get_jasper_magic PROTOTYPE((int));
extern SPEXP(168) SPEXPFLAG_HIDDEN int SPCDECL sp_set_jasper_threadservermode PROTOTYPE((int, int));
extern SPEXP(169) SPEXPFLAG_HIDDEN int SPCDECL sp_get_jasper_threadservermode PROTOTYPE((int*, int));

/* [PM] 3.9.1 get and put arbitrary sized integers (bignums) */
extern SPEXP(170) int SPCDECL SP_get_integer_bytes PROTOARGS((SP_term_ref tr, void *buf, size_t *pbuf_size, int native));
extern SPEXP(171) int SPCDECL SP_put_integer_bytes PROTOARGS((SP_term_ref tr, void *buf, size_t buf_size, int native));

/* [PM] 4.1 Legacy, use SP_set_system_property() if at all.
 *  key and value are encoded string (UTF-8) */
extern SPEXP(172) SPEXPFLAG_HIDDEN int SPCDECL SP_setenv PROTOARGS((char const *key_ienc, char const *value_ienc));
/* [PM] 4.1 Equivalent to SP_get_system_property(key, &value, SPIO_OPTION_NONE).
 * key and returned value are encoded strings (UTF-8). Returned value is SP_malloc-ed and should be SP_free-ed. */
extern SPEXP(173) char* SPCDECL SP_getenv PROTOARGS((char const *key_ienc));
/* [PM] 4.1 These are intentionally not documented yet.
   key and returned value are encoded strings (UTF-8). Returned value is SP_malloc-ed and should be SP_free-ed. */
extern SPEXP(194) spio_t_error_code SPCDECL SP_get_system_property PROTOARGS((char const *key, char **pvalue, spio_t_bits options));
extern SPEXP(195) spio_t_error_code SPCDECL SP_set_system_property PROTOARGS((char const *key, char const *value, spio_t_bits options));


/* These (sp_spio_...) should not be used except as part of SP_ASSERT et al. */
extern SPEXP(176) int SPCDECL sp_spio_debug_break PROTOARGS((char const *file, int line));
extern SPEXP(177) char const * SPCDECL sp_spio_error_name PROTOARGS((spio_t_error_code code));
extern SPEXP(178) int SPCDECL sp_spio_trace_line PROTOARGS((char const *function, char const *file, int line, char const *msg, SP_integer l, char const *string, int level));
extern SPEXP(175) int SPCDECL sp_spio_assert_failure PROTOARGS((char const *function, char const *file, int line));


/* Character encodings (not yet public) */
extern SPEXP(179) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_get_encoding PROTOARGS((char const *encoding_name, spio_t_encoding **pencoding, spio_t_bits options));
extern SPEXP(180) SPEXPFLAG_HIDDEN void SPCDECL SP_encoding_release PROTOARGS((spio_t_encoding *encoding));
extern SPEXP(181) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encoding_open PROTOARGS((spio_t_encoding *encoding,
                                                                        spio_t_encoding_state **pencoding_state,
                                                                        void *args,
                                                                        spio_t_bits options));

extern SPEXP(182) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encode_from_codes PROTOARGS((spio_t_encoding *encoding,
                                                                            spio_t_wchar const *src, size_t src_size,
                                                                            spio_t_encoding_state **pencoding_state,
                                                                            spio_t_byte *dst, size_t dst_size,
                                                                            size_t *psrc_size_read,
                                                                            size_t *pdst_size_wrote,
                                                                            spio_t_bits options));

extern SPEXP(183) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encode_to_codes PROTOARGS((spio_t_encoding *encoding,
                                                                          spio_t_byte const *src, size_t src_size,
                                                                          spio_t_encoding_state **pencoding_state,
                                                                          spio_t_wchar *dst, size_t dst_size,
                                                                          size_t *psrc_size_read,
                                                                          size_t *pdst_size_wrote,
                                                                          spio_t_bits options));

extern SPEXP(196) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_codes_to_multi_bytes PROTOARGS((char const *encoding_name, spio_t_wchar const *src, size_t const src_len, void **pdst, size_t *pdst_size));

/* [PM] 4.0 Same as MEMORY_FAULT(msg, culprit). Internal for use by LAZY_MEMORY_FAULT and LAZY_NULL_CHECK */
extern SPEXP(185) void SPCDECL sp_memory_fault PROTOARGS((char const *msg, int culprit));

extern SPEXP(186) int SPCDECL SP_decode_reserved_exception PROTOARGS((SP_term_ref tr, SP_integer *pdata));

extern SPEXP(187) spio_t_error_code SPCDECL SP_install_idle_hook PROTOARGS((SP_idle_hook *hook, void *cookie, spio_t_bits options));

extern SPEXP(131) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_install_spti_hook PROTOARGS((SP_spti_hook *hook, void *cookie, spio_t_bits options));
/* Largest used SPEXP number: 196 */

/* End of external function declarations */
#endif  /* 0, declarations never seen by C */
/* [PM] *********** NOTHING below this line ********************** */

SP_END_DECL

#endif /* INCLUDED_SICSTUS_H */
