#ifndef SPIO_LAYER_ASYNC_WIN32_H
#define SPIO_LAYER_ASYCN_WIN32_H 1
#include "spio_types.h"
#include "spio_layer_async.h"

#define SPIO_LAYER_ASYNC_WIN32_OPEN_OPTION_OPENED_OVERLAPPED SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_1
/* replaced by SPIO_OPEN_OPTION_OBEY_FILE_OFFSET */
#if 0
#define SPIO_LAYER_ASYNC_WIN32_OPEN_OPTION_OBEY_FILE_OFFSET SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_2
#endif

/* extern spio_t_error_code spio_layer_async_win32_close_file_handle(spio_t_os_file_handle hFile); */
extern spio_t_error_code spio_layer_async_win32_open(spio_t_funcs *funcs, SPIO *s, spio_t_arglist args, spio_t_bits options);
extern spio_t_error_code spio_layer_async_win32_ioctl(SPIO *s, spio_t_ioctl_operation operation, void *ioctl_param, spio_t_bits options);

extern spio_t_error_code spio_layer_async_win32_open_file_handle(SPIO *s, spio_t_os_file_handle hFile, spio_t_bits open_options);

#endif  /* SPIO_LAYER_ASYCN_WIN32_H */

