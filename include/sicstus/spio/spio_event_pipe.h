#ifndef SPIO_EVENT_PIPE_H_INCLUDED
#define SPIO_EVENT_PIPE_H_INCLUDED 1
#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"

/* [PM] 4.2 A full memory barrier on success */
extern spio_t_error_code spio_pipe_event_wait(spio_t_event *events[], size_t nevents, spio_t_timespec *timeout);

extern spio_t_error_code spio_pipe_event_new(spio_t_bits options, spio_t_event **pevent);
extern spio_t_error_code spio_init_event_pipe(spio_t_bits options);

/* Read-only event based on read/write-ability of an external file descriptor */
spio_t_error_code spio_fd_event_new(int read_fd, int write_fd, spio_t_bits options, spio_t_event **pevent);


/* INTERNAL USE ONLY */
typedef struct spio_t_event_raw_ spio_t_event_raw;
extern spio_t_error_code spio_event_raw_new(spio_t_event_raw **praw);
extern void spio_event_raw_free(spio_t_event_raw *raw);


/* [PM] 4.2 A full memory barrier on success */
extern spio_t_error_code spio_event_raw_set(spio_t_event_raw *raw);

extern spio_t_error_code spio_event_raw_reset(spio_t_event_raw *raw);

/* [PM] 4.2 A full memory barrier on success */
extern spio_t_error_code spio_event_raw_wait(spio_t_event_raw *raw, spio_t_timespec *timeout);

#endif  /* SPIO_EVENT_PIPE_H_INCLUDED */
