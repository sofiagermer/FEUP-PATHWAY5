#ifndef SPIO_EVENT_H_INCLUDED
#define SPIO_EVENT_H_INCLUDED 1
#include "spio_types.h"
#include "spio_errors.h"

typedef struct spio_t_event_ spio_t_event;

typedef struct spio_t_event_funcs_ spio_t_event_funcs;
struct spio_t_event_funcs_ {
  /* All these must be thread safe (FIXME: check this) */
  spio_t_error_code (*set)(spio_t_event *event);
  spio_t_error_code (*reset)(spio_t_event *event);
  spio_t_error_code (*query)(spio_t_event *event);
  void (*release)(spio_t_event *event);
};

#define SPIO_EVENT_LISTENER_OPTION_IMMEDIATE_CALL SPIO_BIT(0)
/* Called in main thread after having been removed as stream listener. */
#define SPIO_EVENT_LISTENER_OPTION_DEALLOCATE SPIO_NEXT_BIT(SPIO_EVENT_LISTENER_OPTION_IMMEDIATE_CALL)
typedef spio_t_error_code spio_t_event_listener(void *data, spio_t_bits options);

struct spio_t_event_ {
  spio_t_event_funcs *funcs;
#define SPIO_EVENT_FLAG_READONLY_ SPIO_BIT(0) /* cannot set or reset (not yet available) */
#define SPIO_EVENT_FLAG_REFCOUNT_BIT_0 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_READONLY_)
#define SPIO_EVENT_FLAG_REFCOUNT_BIT_1 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_REFCOUNT_BIT_0)

#define SPIO_EVENT_FLAG_PRIVATE1 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_REFCOUNT_BIT_1)
#define SPIO_EVENT_FLAG_PRIVATE2 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE1)
#define SPIO_EVENT_FLAG_PRIVATE3 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE2)
#define SPIO_EVENT_FLAG_PRIVATE4 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE3)
#define SPIO_EVENT_FLAG_PRIVATE5 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE4)
  spio_t_bits flags;
  /* event-type specific data */
};

/* only for implementors. Note: refcounting is not thread-safe. */
#define SPIO_EVENT_GET_REFCOUNT(EVENT) ((SPIO_MASK_IS_SET((EVENT)->flags, SPIO_EVENT_FLAG_REFCOUNT_BIT_1) ? 1<<1 : 0) | (SPIO_MASK_IS_SET((EVENT)->flags, SPIO_EVENT_FLAG_REFCOUNT_BIT_0) ? 1<<0 : 0))
/* only for implementors */
#define SPIO_EVENT_SET_REFCOUNT(EVENT, REFCOUNT) do{                    \
    spio_t_event *event_ = (EVENT);                                     \
    int refcount_ = (REFCOUNT);                                         \
    SPIO_ASSERT(0 <= refcount_  && refcount_ <= 2);                     \
    if (refcount_ & (1<<1)) {                                           \
      SPIO_SET_MASK(event_->flags, SPIO_EVENT_FLAG_REFCOUNT_BIT_1);     \
    }                                                                   \
    if (refcount_ & (1<<0)) {                                           \
      SPIO_SET_MASK(event_->flags, SPIO_EVENT_FLAG_REFCOUNT_BIT_0);     \
    }                                                                   \
    /* By clearing last we ensure no thread will (briefly) see refcount==0.
       This is only for debugging, set/get refcount is only allowed from main thread */ \
    if (!(refcount_ & (1<<1))) {                                        \
      SPIO_CLEAR_MASK(event_->flags, SPIO_EVENT_FLAG_REFCOUNT_BIT_1);   \
    }                                                                   \
    if (!(refcount_ & (1<<0))) {                                        \
      SPIO_CLEAR_MASK(event_->flags, SPIO_EVENT_FLAG_REFCOUNT_BIT_0);   \
    }                                                                   \
                                                                        \
    SPIO_ASSERT(SPIO_EVENT_GET_REFCOUNT(event_) == refcount_);          \
  } while (0)

#if SPIO_TRACK_BIGLOCK_DEADLOCK
#define SPIO_ASSERT_NOT_BIGLOCK_OWNER_FL(F,L) do{       \
      if (spio_owns_biglock() == SPIO_S_TRUE)           \
         {                                              \
           spio_debug_break((F), (L));                  \
         }                                              \
   } while (0)
#else  /* !SPIO_TRACK_BIGLOCK_DEADLOCK */
#define SPIO_ASSERT_NOT_BIGLOCK_OWNER_FL(F,L) do {      \
  char*f=(F); /* visible in debugger */                 \
  int l = (L);                                          \
  SPIO_ASSERT(!(spio_owns_biglock() == SPIO_S_TRUE));   \
  (void)f;                                              \
  (void)l;                                              \
  } while (0)
#endif  /* SPIO_TRACK_BIGLOCK_DEADLOCK */

#define SPIO_ASSERT_NOT_BIGLOCK_OWNER() SPIO_ASSERT_NOT_BIGLOCK_OWNER_FL(__FILE__,__LINE__)


extern spio_t_error_code spio_event_set(spio_t_event *event);
extern spio_t_error_code spio_event_reset(spio_t_event *event);
extern spio_t_error_code spio_event_query(spio_t_event *event);
extern spio_t_error_code spio_event_wait(spio_t_event *events[], size_t nevents, spio_t_timespec *timeout);

extern spio_t_error_code spio_init_event(spio_t_bits options);

#define SPIO_EVENT_NEW_OPTION_SET SPIO_BIT(0) /* create the event set */
extern spio_t_error_code spio_event_new(spio_t_bits options, spio_t_event **pevent);
extern void spio_event_free(spio_t_event *event);
extern spio_t_error_code spio_add_event_listener(spio_t_event *event, spio_t_event_listener *listener, void *data, spio_t_bits options);
extern spio_t_error_code spio_remove_event_listener(spio_t_event *event, spio_t_event_listener *listener, void *data, spio_t_bits options);
#endif  /* SPIO_EVENT_H_INCLUDED */
