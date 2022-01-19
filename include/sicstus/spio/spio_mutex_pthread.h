#ifndef SPIO_MUTEX_PTHREAD_H_INCLUDED
#define SPIO_MUTEX_PTHREAD_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"
#include "spio_mutex.h"

#include <pthread.h>

struct spio_t_mutex_ {
  pthread_mutex_t pthread_mutex;
};

extern spio_t_error_code spio_mutex_new_pthread(spio_t_mutex **pthread, spio_t_bits options);
extern void spio_mutex_free_pthread(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_lock_pthread(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_unlock_pthread(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_debug_is_locked_by_other_pthread(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_debug_is_unlocked_pthread(spio_t_mutex *mutex);
#endif  /* SPIO_MUTEX_PTHREAD_H_INCLUDED */
