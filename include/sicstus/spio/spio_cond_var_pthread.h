#ifndef SPIO_COND_VAR_PTHREAD_H_INCLUDED
#define SPIO_COND_VAR_PTHREAD_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"
#include "spio_cond_var.h"

#include <pthread.h>

struct spio_t_cond_var_ {
  pthread_cond_t pthread_cond_var;
};

extern spio_t_error_code spio_cond_var_new_pthread(spio_t_cond_var **pthread, spio_t_bits options);
extern void spio_cond_var_free_pthread(spio_t_cond_var *cond);
extern spio_t_error_code spio_cond_var_wait_pthread(spio_t_cond_var *cond, spio_t_mutex *mutex);
extern spio_t_error_code spio_cond_var_timedwait_pthread(spio_t_cond_var *cond, spio_t_mutex *mutex, spio_t_timespec *timeout, spio_t_bits options);
extern spio_t_error_code spio_cond_var_signal_pthread(spio_t_cond_var *cond, spio_t_mutex *mutex, spio_t_bits options);

extern spio_t_error_code spio_cond_var_now_pthread(spio_t_timespec *now);

#endif  /* SPIO_COND_VAR_PTHREAD_H_INCLUDED */
