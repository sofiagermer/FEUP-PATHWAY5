#ifndef SPIO_QUEUE_H_INCLUDED
#define SPIO_QUEUE_H_INCLUDED 1

#include "spio_types.h"
#include "spio_utils.h"

struct spio_t_queue_;           /* opaque */
typedef struct spio_t_queue_ spio_t_queue;

typedef struct spio_t_queue_entry_ spio_t_queue_entry;
struct spio_t_queue_entry_ {
  spio_t_queue_entry *next;
};

#define SPIO_QUEUE_NEW_OPTION_THREADSAFE SPIO_BIT(0) /* mutex-protect queue operations */
#define SPIO_QUEUE_NEW_OPTION_WAITABLE SPIO_NEXT_BIT(SPIO_QUEUE_NEW_OPTION_THREADSAFE)
/* [PM] 4.1.3 use a recursive mutex. Not allowed with SPIO_QUEUE_NEW_OPTION_WAITABLE */
#define SPIO_QUEUE_NEW_OPTION_RECURSIVE_LOCK SPIO_NEXT_BIT(SPIO_QUEUE_NEW_OPTION_WAITABLE)
extern spio_t_error_code spio_queue_new(spio_t_queue **pqueue, spio_t_bits options);
extern void spio_queue_free(spio_t_queue *queue);

#define SPIO_QUEUE_PUT_LAST_OPTION_ALREADY_LOCKED SPIO_BIT(0)
extern spio_t_error_code spio_queue_put_last(spio_t_queue *queue, spio_t_queue_entry *entry, spio_t_bits options);

#define SPIO_QUEUE_GET_FIRST_OPTION_PEEK SPIO_BIT(0)
#define SPIO_QUEUE_GET_FIRST_OPTION_WAIT SPIO_NEXT_BIT(SPIO_QUEUE_GET_FIRST_OPTION_PEEK)

/* cancellation point */
extern spio_t_error_code spio_queue_get_first(spio_t_queue *queue, spio_t_queue_entry **pentry, spio_t_bits options);

#define SPIO_QUEUE_GET_LAST_OPTION_PEEK SPIO_BIT(0)
#define SPIO_QUEUE_GET_LAST_OPTION_WAIT SPIO_NEXT_BIT(SPIO_QUEUE_GET_LAST_OPTION_PEEK)
#define SPIO_QUEUE_GET_LAST_OPTION_ALREADY_LOCKED SPIO_NEXT_BIT(SPIO_QUEUE_GET_LAST_OPTION_WAIT)

/* cancellation point */
extern spio_t_error_code spio_queue_get_last(spio_t_queue *queue, spio_t_queue_entry **pentry, spio_t_bits options);

#define SPIO_QUEUE_CONTAINS_OPTION_ALREADY_LOCKED SPIO_BIT(0)
extern spio_t_error_code spio_queue_contains(spio_t_queue *queue, spio_t_queue_entry *entry, spio_t_bits options);
extern spio_t_error_code spio_queue_size(spio_t_queue *queue);

extern spio_t_error_code spio_queue_lock(spio_t_queue *queue);
extern spio_t_error_code spio_queue_unlock(spio_t_queue *queue);

extern spio_t_error_code spio_queue_signal(spio_t_queue *queue);

/* For use with SPIO_TASK_CLEANUP_PUSH */
extern void spio_queue_unlock_cleanup(void *arg);

#endif  /* SPIO_QUEUE_H_INCLUDED */
