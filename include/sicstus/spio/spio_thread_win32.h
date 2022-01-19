#ifndef SPIO_THREAD_WIN32_H_INCLUDED
#define SPIO_THREAD_WIN32_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"
#include "spio_thread.h"

extern spio_t_error_code spio_thread_run_task_win32(spio_t_task *task, spio_t_bits options);
extern spio_t_error_code spio_task_new_win32(spio_t_thread_function *fun, void *arg, spio_t_bits options, spio_t_task **ptask);
extern void spio_task_free_win32(spio_t_task *task);
extern spio_t_error_code spio_task_cancel_win32(spio_t_task *t, spio_t_bits options);

extern spio_t_error_code spio_win32_self(spio_t_thread_id *pself);
extern spio_t_error_code spio_thread_equal_win32(spio_t_thread_id id1, spio_t_thread_id id2);

extern spio_t_error_code spio_init_thread_win32(spio_t_bits options);

extern spio_t_error_code spio_win32_thread_times(spio_t_thread_id id, spio_t_bits options, spio_t_timespec *time);

#endif  /* SPIO_THREAD_WIN32_H_INCLUDED */
