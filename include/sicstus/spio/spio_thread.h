#ifndef SPIO_THREAD_H_INCLUDED
#define SPIO_THREAD_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"

/* [PM] 4.2 match the volatile typedef in spio_thread.c, which see */
typedef struct spio_t_task_ volatile spio_t_task_;
typedef spio_t_task_ spio_t_task;

typedef void *spio_t_thread_function(spio_t_task *task, void *arg);

extern void spio_task_free(spio_t_task *task);

extern spio_t_error_code spio_task_result(spio_t_task *task, void **presult);
#define SPIO_TASK_STATUS_RUNNING SPIO_BIT(0)
extern spio_t_error_code spio_task_status(spio_t_task *task, spio_t_bits *pstatus);
extern spio_t_error_code spio_task_event(spio_t_task *task, spio_t_event **pevent);
extern int spio_task_is_running(spio_t_task *task);
#define SPIO_TASK_IS_RUNNING(TASK) spio_task_is_running((TASK))

/* SPIO_S_TRUE (SPIO_S_FALSE) if task has (not) been spio_task_cancel-ed */
extern spio_t_error_code spio_task_is_cancelled(spio_t_task *task);

/* only when SPIO_HAVE_TASK_CANCEL_EVENT (otherwise SPIO_E_NOT_SUPPORTED) */
extern spio_t_error_code spio_task_cancel_event(spio_t_task *task, spio_t_event **pevent);

#define SPIO_TASK_CANCEL_OPTION_ASYNC_ SPIO_BIT(0) /* do not wait for task to finish after cancel (currently not implemented) */
extern spio_t_error_code spio_task_cancel(spio_t_task *task, spio_t_bits options);



#define SPIO_THREAD_CREATE_OPTION_DETACHED SPIO_BIT(0)
/* #define SPIO_THREAD_CREATE_OPTION_JOINABLE SPIO_BIT(1) */
#define SPIO_THREAD_CREATE_OPTION_POOL SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_DETACHED)

/* allow thread to be (pthread_/Win32 APC-)cancelled */
#define SPIO_THREAD_CREATE_OPTION_CANCELLABLE SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_POOL)

/* used by spio_thread_pthread.h, spio_thread_win32.h */
#define SPIO_THREAD_CREATE_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_CANCELLABLE)
#define SPIO_THREAD_CREATE_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_PRIVATE_1)
#define SPIO_THREAD_CREATE_OPTION_PRIVATE_3 SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_PRIVATE_2)

extern spio_t_error_code spio_task_create(spio_t_bits options, spio_t_thread_function *start_routine, void *arg, spio_t_task **ptask);

/* For assertions only. != SPIO_S_FALSE in main thread or if pthread cancellability is off or unsupported */
extern spio_t_error_code spio_thread_not_cancellable(void);
extern spio_t_error_code spio_thread_not_cancelled(void);
#ifndef SPIO_ASSERT_NOT_CANCELLABLE
#if SPIO_TRACK_BIGLOCK_DEADLOCK
#define SPIO_ASSERT_NOT_CANCELLABLE() do{                       \
    if (!(spio_thread_not_cancellable() != SPIO_S_FALSE)) {     \
       spio_debug_break(__FILE__, __LINE__);                    \
    }                                                           \
  } while(0)
#endif  /* SPIO_TRACK_BIGLOCK_DEADLOCK */
#endif  /* SPIO_ASSERT_NOT_CANCELLABLE */

#ifndef SPIO_ASSERT_NOT_CANCELLED
#define SPIO_ASSERT_NOT_CANCELLED() SPIO_ASSERT(spio_thread_not_cancelled() != SPIO_S_FALSE)
#endif  /* !SPIO_ASSERT_NOT_CANCELLED */

#ifndef SPIO_ASSERT_NOT_CANCELLABLE
#define SPIO_ASSERT_NOT_CANCELLABLE() SPIO_ASSERT(spio_thread_not_cancellable() != SPIO_S_FALSE)
#endif  /* !SPIO_ASSERT_NOT_CANCELLABLE */

#if SPIO_TASK_PTHREAD_CANCEL_HANDLING
/* in spio_thread_pthread.c */
extern void spio_task_pthread_cancel_enable(void);
extern void spio_task_pthread_cancel_disable(void);
#define SPIO_TASK_PTHREAD_CANCEL_ENABLE() spio_task_pthread_cancel_enable()
#define SPIO_TASK_PTHREAD_CANCEL_DISABLE() spio_task_pthread_cancel_disable()
#else  /* !SPIO_TASK_PTHREAD_CANCEL_HANDLING */
#define SPIO_TASK_PTHREAD_CANCEL_ENABLE() /* empty */
#define SPIO_TASK_PTHREAD_CANCEL_DISABLE() /* empty */
#endif  /* !SPIO_TASK_PTHREAD_CANCEL_HANDLING */

extern void spio_testcancel(void);

#define SPIO_S_PROCESS_TIME SPIO_S_PRIVATE_ERROR_0
extern spio_t_error_code spio_thread_times(spio_t_thread_id id, spio_t_bits options, spio_t_timespec *time);


/* [PM] 4.1.3 do this first in all cleanup handlers (DBG, cancel should be disabled explicitly when calling cleanup pop with a non-zero argument) */
#if SPIO_DEBUG_LOCKS
#define SPIO_TASK_PTHREAD_CLEANUP_CANCEL_DISABLE() SPIO_TASK_PTHREAD_CANCEL_DISABLE()
#else  /* !SPIO_DEBUG_LOCKS */
#define SPIO_TASK_PTHREAD_CLEANUP_CANCEL_DISABLE() /* empty */
#endif                                             /* SPIO_DEBUG_LOCKS */

extern spio_t_error_code spio_thread_self(spio_t_thread_id *pself);

extern spio_t_error_code spio_thread_equal(spio_t_thread_id id1, spio_t_thread_id id2);

extern spio_t_error_code spio_init_thread(spio_t_bits options);

#if SPIO_HAVE_PTHREAD_H
#include <pthread.h>
#endif  /* SPIO_HAVE_PTHREAD_H */

#if SPIO_HAVE_PTHREAD_SETCANCELSTATE

/* [PM] 4.4.0 On Linux (E.g. Ubuntu 16.04+GCC 5.3, 17.04+GCC 6.3) the
   compiler will sometimes complain about "variable ... might be
   clobbered by ‘longjmp’ or ‘vfork’" when pthread_cleanup_push() and
   pthread_cleanup_pop() is used. This is arguably a bug in the the C
   compiler+headings. (configure --enable-opt --enable-dbg=1 ...)

   A workaround is to declare the affected variables as volatile, e.g.:
   ...
   int PTC_VOLATILE foo;
*/
#define PTC_VOLATILE volatile	/* PThread Cleanup VOLATILE */

#define SPIO_TASK_CLEANUP_PUSH(TASK, FUN, COOKIE) pthread_cleanup_push((FUN), ((TASK)==0 ? (COOKIE) : (COOKIE))) /* also quiet compiler about unused TASK. */
#define SPIO_TASK_CLEANUP_POP(DOIT) pthread_cleanup_pop((DOIT))

/* For debug code only, temporarily disable cancellation */
#define SPIO_DBG_WITHOUT_THREAD_CANCEL_BEGIN {                          \
  int SPIO_WITHOUT_THREAD_CANCEL_BEGIN_state = SPIO_PTHREAD_CANCEL_DISABLE; \
  int SPIO_DBG_WITHOUT_THREAD_CANCEL_BEGIN_cancellable = 0;		\
  SPIO_DEBUG_CANCEL_CLEANUP_PUSH1(SPIO_DBG_WITHOUT_THREAD_CANCEL_BEGIN_cancellable); \
  {                                                                     \
     int SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result = SPIO_PTHREAD_SETCANCELSTATE(SPIO_PTHREAD_CANCEL_DISABLE, &SPIO_WITHOUT_THREAD_CANCEL_BEGIN_state); \
     SPIO_ASSERT(SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result == 0); \
     (void)SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result;             \
  }                                                                     \
  { int SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result_dummy = 0 /* so we can terminate with ; */

#define SPIO_DBG_WITHOUT_THREAD_CANCEL_END                              \
  (void) SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result_dummy;         \
    }                                                                   \
    {                                                                   \
        int SPIO_WITHOUT_THREAD_CANCEL_END_pthread_result = SPIO_PTHREAD_SETCANCELSTATE(SPIO_WITHOUT_THREAD_CANCEL_BEGIN_state, &SPIO_WITHOUT_THREAD_CANCEL_BEGIN_state); \
        SPIO_ASSERT(SPIO_WITHOUT_THREAD_CANCEL_END_pthread_result == 0); \
        (void)SPIO_WITHOUT_THREAD_CANCEL_END_pthread_result;            \
     }                                                                  \
    SPIO_DEBUG_CANCEL_CLEANUP_POP1();					\
   }

#elif SPIO_WIN32 || !SPIO_HAVE_PTHREAD_SETCANCELSTATE
typedef void spio_t_task_cleanup_fun(void*);

#define SPIO_TASK_CLEANUP_PUSH(TASK, FUN, COOKIE) {                     \
    spio_t_task * const spio_task_cleanup_push_task_ = (TASK);          \
    spio_t_task_cleanup_fun * const spio_task_cleanup_push_fun_ = (FUN); \
    void * const spio_task_cleanup_push_cookie_ = (COOKIE);             \
    (void) 1;

#define SPIO_TASK_CLEANUP_POP(DOIT)                                     \
    if ((DOIT) != 0)                                                    \
      {                                                                 \
        (void)spio_task_cleanup_push_task_;                             \
        spio_task_cleanup_push_fun_(spio_task_cleanup_push_cookie_);    \
      }                                                                 \
    }


#define SPIO_DBG_WITHOUT_THREAD_CANCEL_BEGIN {				\
   int SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result_dummy = 0 /* so we can terminate with ; */

#define SPIO_DBG_WITHOUT_THREAD_CANCEL_END			\
  (void) SPIO_WITHOUT_THREAD_CANCEL_BEGIN_pthread_result_dummy;	\
  }

#endif  /* SPIO_WIN32 */

#if SPIO_USE_PTHREADS && (SPIO_DEBUG || SPIO_ASSERTIONS || SPIO_TRACK_BIGLOCK_DEADLOCK)
extern void spio_debug_cancel_cleanup_function(void *arg);

#define  SPIO_DEBUG_CANCEL_CLEANUP_PUSH() {                             \
   int spio_debug_cancel_cleanup_push_match_ = 42;                      \
   /* Not valid if called from interrupt thread: SPIO_ASSERT_NOT_BIGLOCK_OWNER(); */ \
   pthread_cleanup_push(spio_debug_cancel_cleanup_function, NULL)

#define  SPIO_DEBUG_CANCEL_CLEANUP_POP()                                \
  ; /* Avoid label at end of compound statement if label before POP */  \
  pthread_cleanup_pop(0);                                               \
  (void) spio_debug_cancel_cleanup_push_match_;                         \
} /* do not execute (so call implies cancellation processing) */

extern void spio_debug_cancel_cleanup_function1(void *arg);

#define  SPIO_DEBUG_CANCEL_CLEANUP_PUSH1(CANCELLABLE_FLAG) {            \
   SPIO_DEBUG_CANCEL_CLEANUP_PUSH();                                    \
   {                                                                    \
      int spio_debug_cancel_cleanup_push_match1_ = 42;                  \
      /* Not valid if called from interrupt thread: SPIO_ASSERT_NOT_BIGLOCK_OWNER(); */ \
      pthread_cleanup_push(spio_debug_cancel_cleanup_function1, (void*)&(CANCELLABLE_FLAG))

#define  SPIO_DEBUG_CANCEL_CLEANUP_POP1()                               \
      ; /* Avoid label at end of compound statement if label before POP */ \
      pthread_cleanup_pop(0); /* do not execute (so call implies cancellation processing) */ \
      (void) spio_debug_cancel_cleanup_push_match1_;                    \
   }                                                                    \
   SPIO_DEBUG_CANCEL_CLEANUP_POP();                                     \
}

#else  /* !SPIO_DEBUG */

/* no-op */

#define  SPIO_DEBUG_CANCEL_CLEANUP_PUSH() {
#define  SPIO_DEBUG_CANCEL_CLEANUP_POP() ; /* Avoid label at end of compound statement if label before POP */ }

#define  SPIO_DEBUG_CANCEL_CLEANUP_PUSH1(CANCELLABLE_FLAG) { (void)&(CANCELLABLE_FLAG); {
#define  SPIO_DEBUG_CANCEL_CLEANUP_POP1() ; /* Avoid label at end of compound statement if label before POP */ } }

#endif  /* !SPIO_DEBUG */

#if SPIO_HAVE_PTHREAD_SETCANCELSTATE

#define SPIO_PTHREAD_CANCEL_ENABLE PTHREAD_CANCEL_ENABLE
#define SPIO_PTHREAD_CANCEL_DISABLE PTHREAD_CANCEL_DISABLE
#define SPIO_PTHREAD_SETCANCELSTATE(NEW,POLD) pthread_setcancelstate((NEW),(POLD))

#else  /* !SPIO_HAVE_PTHREAD_SETCANCELSTATE */

/* [PM] 4.1.3+ Android/Bionic */
#if !(SPIO_ANDROID || SPIO_WIN32)
#error "[PM] 4.1.3+ expected SP_ANDROID to be true"
#endif  /* !SP_ANDROID */

#define SPIO_PTHREAD_CANCEL_ENABLE 1
#define SPIO_PTHREAD_CANCEL_DISABLE 0
#define SPIO_PTHREAD_SETCANCELSTATE(NEW,POLD) (((POLD)+(NEW)) != ((POLD)+(NEW))) /* always zero and uses NEW, POLD */

#endif /* !SPIO_HAVE_PTHREAD_SETCANCELSTATE */


#if ! defined(PTC_VOLATILE)
#define PTC_VOLATILE /* default to empty */
#endif /* ! defined(PTC_VOLATILE) */

#endif  /* SPIO_THREAD_H_INCLUDED */
