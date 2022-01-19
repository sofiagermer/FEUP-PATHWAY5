#ifndef SPIO_MUTEX_H_INCLUDED
#define SPIO_MUTEX_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

struct spio_t_mutex_;           /* opaque */
typedef struct spio_t_mutex_ spio_t_mutex;

/* Note: Do not use recursive mutexes with condition variables! */
#define SPIO_MUTEX_NEW_OPTION_RECURSIVE SPIO_BIT(0)
extern spio_t_error_code spio_mutex_new(spio_t_mutex **pmutex, spio_t_bits options);
extern void spio_mutex_free(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_lock(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_unlock(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_debug_is_locked_by_other(spio_t_mutex *mutex);
/* May give SPIO_E_NOT_SUPPORTED (Win32) */
extern spio_t_error_code spio_mutex_debug_is_unlocked(spio_t_mutex *mutex);

extern void spio_mutex_unlock_cleanup(void *arg); /* for use with SPIO_TASK_CLEANUP_PUSH */
#endif  /* SPIO_MUTEX_H_INCLUDED */
