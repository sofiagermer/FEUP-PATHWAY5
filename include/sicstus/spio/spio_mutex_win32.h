#ifndef SPIO_MUTEX_WIN32_H_INCLUDED
#define SPIO_MUTEX_WIN32_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"
#include "spio_mutex.h"

extern spio_t_error_code spio_mutex_new_win32(spio_t_mutex **win32, spio_t_bits options);
extern void spio_mutex_free_win32(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_lock_win32(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_unlock_win32(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_debug_is_locked_by_other_win32(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_debug_is_unlocked_win32(spio_t_mutex *mutex);

#endif  /* SPIO_MUTEX_WIN32_H_INCLUDED */
