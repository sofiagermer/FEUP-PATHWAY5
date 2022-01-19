#ifndef SPIO_LAYER_ZLIB_H_INCLUDED
#define SPIO_LAYER_ZLIB_H_INCLUDED 1

#include "spio_types.h"
#include "spio.h"

#define SPIO_LAYER_ZLIB_OPTION_DEVICE SPIO_PRIVATE_OPTION_1

#define SPIO_LAYER_ZLIB_PUSH_OPTION_DEVICE  SPIO_PRIVATE_OPTION_1
#define SPIO_LAYER_ZLIB_PUSH_OPTION_READ    SPIO_PRIVATE_OPTION_2
#define SPIO_LAYER_ZLIB_PUSH_OPTION_WRITE   SPIO_PRIVATE_OPTION_3

/* The write should go at end of buffer, not be passed to underlying layer */
#define SPIO_LAYER_ZLIB_WRITE_OPTION_EXTEND   SPIO_PRIVATE_OPTION_1

extern spio_t_error_code spio_init_layer_zlib(spio_t_bits options);


#endif  /* SPIO_LAYER_ZLIB_H_INCLUDED */
