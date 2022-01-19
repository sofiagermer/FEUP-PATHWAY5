#ifndef SPIO_TIMER_H_INCLUDED
#define SPIO_TIMER_H_INCLUDED 1

/*
  Timer facility. Inspired by the POSIX Timers option.
 */

#include "spio_types.h"

struct spio_t_timer_;
typedef struct spio_t_timer_ *spio_t_timer;

struct spio_t_timerspec_ {
  spio_t_timespec value;
  spio_t_timespec interval;
};
typedef struct spio_t_timerspec_ spio_t_timerspec;


typedef void spio_t_timer_callback(spio_t_timer timerid, void *cookie);
typedef spio_t_error_code spio_t_timer_now_callback(spio_t_timespec *now, spio_t_timer timerid, void *cookie);

/* The current wallclock time, as used by timers.
   May be called from any thread.
*/
extern spio_t_error_code spio_timer_now(spio_t_timespec *now);

extern spio_t_error_code spio_timer_create(spio_t_timer *timerid,
					   spio_t_timer_callback *callback,
					   spio_t_timer_now_callback *now_callback,
					   void *cookie,
					   spio_t_bits options);

extern spio_t_error_code spio_timer_delete(spio_t_timer timerid);

/* From spio_timer_settime() and spio_timer_gettime() if the timer was not already armed. */
#define SPIO_S_TIMER_DISARMED SPIO_S_PRIVATE_ERROR_0
#define SPIO_S_TIMER_DISARMED_FIRING SPIO_S_PRIVATE_ERROR_1
#define SPIO_S_TIMER_ARMED SPIO_S_PRIVATE_ERROR_2
#define SPIO_S_TIMER_ARMED_FIRING SPIO_S_PRIVATE_ERROR_3

#define SPIO_TIMER_SETTIME_OPTION_DISARM SPIO_BIT(0) /* Disarm (stop) the timer. In this case value is ignored. */
#define SPIO_TIMER_SETTIME_OPTION_ABSOLUTE SPIO_NEXT_BIT(SPIO_TIMER_SETTIME_OPTION_DISARM) /* value/ovalue specifies absolute times. */
#define SPIO_TIMER_SETTIME_OPTION_RELATIVE SPIO_NEXT_BIT(SPIO_TIMER_SETTIME_OPTION_ABSOLUTE) /* value/ovalue specifies relative times. */

extern spio_t_error_code spio_timer_settime(spio_t_timer timerid, spio_t_bits options,
					    spio_t_timerspec const *value,
					    spio_t_timerspec *ovalue);

#define SPIO_TIMER_GETTIME_OPTION_ABSOLUTE SPIO_BIT(0) /* ovalue specifies absolute time. */
#define SPIO_TIMER_GETTIME_OPTION_RELATIVE SPIO_NEXT_BIT(SPIO_TIMER_GETTIME_OPTION_ABSOLUTE) /* ovalue specifies relative time. */
extern spio_t_error_code spio_timer_gettime(spio_t_timer timerid, spio_t_bits options, spio_t_timerspec *ovalue);


extern spio_t_error_code spio_init_timer(spio_t_bits options);

/* If timer is a valid timer, return it, otherwise return NULL */
extern spio_t_timer spio_timer_find(spio_t_timer timer);

extern spio_t_timespec *spio_timespec_zero_clamped_a_minus_b(spio_t_timespec const *a, spio_t_timespec const *b, spio_t_timespec *result);
extern spio_t_timespec *spio_timespec_clamped_add(spio_t_timespec const *a, spio_t_timespec const *b, spio_t_timespec *result);
extern int spio_timespec_is_zero(spio_t_timespec const *value);
extern int spio_timespec_a_le_b(spio_t_timespec const *a, spio_t_timespec const *b);

#endif	/* SPIO_TIMER_H_INCLUDED */
