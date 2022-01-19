#ifndef SPIO_EVENT_WIN32_H_INCLUDED
#define SPIO_EVENT_WIN32_H_INCLUDED 1
#include "spio_types.h"
#include "spio_errors.h"


extern spio_t_error_code spio_event_win32_wait(spio_t_event *events[], size_t nevents, spio_t_timespec *timeout);
extern spio_t_error_code spio_event_win32_new(spio_t_bits options, spio_t_event **pevent);
extern spio_t_error_code spio_event_win32_init(spio_t_bits options);

extern spio_t_error_code spio_event_win32_handle(spio_t_event *event, HANDLE *phEvent);

/* Read-only event based on passed in os event/HANDLDE */
spio_t_error_code spio_event_win32_handle_event_new(HANDLE hEvent, spio_t_bits options, spio_t_event **pevent);

#endif  /* SPIO_EVENT_WIN32_H_INCLUDED */
