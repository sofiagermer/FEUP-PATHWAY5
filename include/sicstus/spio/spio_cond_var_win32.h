#ifndef SPIO_COND_VAR_WIN32_H_INCLUDED
#define SPIO_COND_VAR_WIN32_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"
#include "spio_cond_var.h"

extern spio_t_error_code spio_cond_var_new_win32(spio_t_cond_var **win32, spio_t_bits options);
extern void spio_cond_var_free_win32(spio_t_cond_var *cond);
extern spio_t_error_code spio_cond_var_wait_win32(spio_t_cond_var *cond, spio_t_mutex *mutex);
extern spio_t_error_code spio_cond_var_timedwait_win32(spio_t_cond_var *cond, spio_t_mutex *mutex, spio_t_timespec *timeout, spio_t_bits options);
extern spio_t_error_code spio_cond_var_signal_win32(spio_t_cond_var *cond, spio_t_mutex *mutex, spio_t_bits options);

extern spio_t_error_code spio_init_module_cond_var_win32(spio_t_bits options);

/* NYI */
extern spio_t_error_code spio_cond_var_now_win32(spio_t_timespec *now);

#endif  /* SPIO_COND_VAR_WIN32_H_INCLUDED */
