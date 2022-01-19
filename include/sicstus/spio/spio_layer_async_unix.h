#ifndef SPIO_LAYER_ASYNC_UNIX_H_INCLUDED
#define SPIO_LAYER_ASYNC_UNIX_H_INCLUDED 1

#include "spio_types.h"
#include "spio_layer_async.h"

#define SPIO_LAYER_ASYNC_UNIX_OPEN_OPTION_THREAD SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_1

/* extern spio_t_error_code spio_layer_async_unix_close_file_handle(spio_t_os_file_handle hFile); */
extern spio_t_error_code spio_layer_async_unix_open(spio_t_funcs *funcs, SPIO *s, spio_t_arglist args, spio_t_bits options);
extern spio_t_error_code spio_layer_async_unix_ioctl(SPIO *s, spio_t_ioctl_operation operation, void *ioctl_param, spio_t_bits options);

#endif  /* SPIO_LAYER_ASYNC_UNIX_H_INCLUDED */
