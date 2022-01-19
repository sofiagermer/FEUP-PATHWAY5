#ifndef SPIO_ASYNC_WIN32_H_INCLUDED
#define SPIO_ASYNC_WIN32_H_INCLUDED 1

#include "spio_types.h"

#if SPIO_ASYNC_INTERNAL

extern void spio_async_free_win32(spio_t_async_info *async_info);
extern spio_t_error_code spio_async_poll_for_completion_win32(spio_t_async_info *async_info);

#endif  /* SPIO_ASYNC_INTERNAL */

/* OS-specific (file-)operations */

/* All spio_async_win32_... takes SPIO_ASYNC_WIN32_OPTION_... */
#define SPIO_ASYNC_WIN32_OPTION_OPENED_OVERLAPPED SPIO_BIT(0)
#define SPIO_ASYNC_WIN32_OPTION_OBEY_FILE_OFFSET SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_OPENED_OVERLAPPED)

/* device type */
#define SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_FILE SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_OBEY_FILE_OFFSET)
#define SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_PIPE SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_FILE)
#define SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_CONSOLE SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_PIPE)
#define SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_SOCKET SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_CONSOLE)
#define SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_UNKNOWN SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_SOCKET)

/* FIXME: use this */
#define SPIO_ASYNC_WIN32_OPTION_SMALL_BUF SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_UNKNOWN) /* DBG */

/* These options should be passed on between all spio_async_win32_... */
#define SPIO_ASYNC_WIN32_OPTION_COMMON_MASK ( 0                                             \
                                              | SPIO_ASYNC_WIN32_OPTION_OPENED_OVERLAPPED   \
                                              | SPIO_ASYNC_WIN32_OPTION_OBEY_FILE_OFFSET    \
                                              | SPIO_ASYNC_WIN32_OPTION_SMALL_BUF           \
                                              | SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_FILE    \
                                              | SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_PIPE    \
                                              | SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_CONSOLE \
                                              | SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_SOCKET  \
                                              | SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_UNKNOWN \
                                             )


/* private options for spio_async_win32_... to use */
#define SPIO_ASYNC_WIN32_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_DEVICE_TYPE_UNKNOWN)
#define SPIO_ASYNC_WIN32_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_PRIVATE_1)
#define SPIO_ASYNC_WIN32_OPTION_PRIVATE_3 SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_PRIVATE_2)
#define SPIO_ASYNC_WIN32_OPTION_PRIVATE_4 SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_PRIVATE_3)
#define SPIO_ASYNC_WIN32_OPTION_PRIVATE_5 SPIO_NEXT_BIT(SPIO_ASYNC_WIN32_OPTION_PRIVATE_4)


extern spio_t_error_code spio_async_win32_read(spio_t_async_info **pasync_info, spio_t_offset file_offset, spio_t_os_file_handle hFile, spio_t_event *event, spio_t_bits options);

extern spio_t_error_code spio_async_win32_write(spio_t_async_info **pasync_info,  spio_t_buf *buf, spio_t_offset file_offset, spio_t_os_file_handle hFile, spio_t_event *event, spio_t_bits options);
extern spio_t_error_code spio_async_win32_fsync(spio_t_async_info **pasync_info,  spio_t_os_file_handle hFile, spio_t_event *event, spio_t_bits options);

extern spio_t_error_code spio_async_suspend_win32(spio_t_async_info * const list[], int nent, spio_t_timespec* timeout);
extern spio_t_error_code spio_async_cancel_win32(spio_t_async_info *async_info);

extern spio_t_error_code spio_init_async_win32(spio_t_bits options);

#endif  /* SPIO_ASYNC_WIN32_H_INCLUDED */
