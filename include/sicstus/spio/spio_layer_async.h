#ifndef SPIO_LAYER_ASYNC_H_INCLUDED
#define SPIO_LAYER_ASYNC_H_INCLUDED 1

#include "spio_types.h"
#include "spio_async.h"

#define SPIO_LAYER_ASYNK_OPEN_OPTION_SMALL_BUF SPIO_PRIVATE_OPTION_1 /* dbg */

#define SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_1 SPIO_PRIVATE_OPTION_3
#define SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_2 SPIO_PRIVATE_OPTION_4
#define SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_3 SPIO_PRIVATE_OPTION_5
#define SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_4 SPIO_PRIVATE_OPTION_6
#define SPIO_LAYER_ASYNK_OPEN_OPTION_PRIVATE_OPTION_5 SPIO_PRIVATE_OPTION_7

/* FIXME: clean up the flag/option names and meanings */

#define SPIO_LAYER_ASYNK_FLAG_FD_OPEN_READ  SPIO_LAYER_FLAG_PRIVATE_1
#define SPIO_LAYER_ASYNK_FLAG_FD_OPEN_WRITE SPIO_LAYER_FLAG_PRIVATE_2
#define SPIO_LAYER_ASYNK_FLAG_SMALL_BUF SPIO_LAYER_FLAG_PRIVATE_3 /* debug: read/write as little as possible */

/* for fd opened with O_APPEND (unavailable on Windows) */
#define SPIO_LAYER_ASYNK_FLAG_FD_O_APPEND SPIO_LAYER_FLAG_PRIVATE_5

/* if set then read and write positions cannot be moved independently (static property) */
#define SPIO_LAYER_ASYNC_FLAG_SINGLE_POS SPIO_LAYER_FLAG_PRIVATE_6
/* if set then any pre-read is invalid (dynamic property) */
#define SPIO_LAYER_ASYNC_FLAG_LAST_WAS_WRITE SPIO_LAYER_FLAG_PRIVATE_7

/* FIXME: This extra nested structure is pointless it is only used as part of spio_t_layer_async_asynk */
typedef struct spio_t_layer_asynk_ spio_t_layer_asynk;
struct spio_t_layer_asynk_ {
  spio_t_layer layer;
  spio_t_os_file_handle hFile;
  spio_t_device_type device_type;
  /* ... */
};



typedef struct spio_t_layer_async_asynk_ spio_t_layer_async_asynk;
struct spio_t_layer_async_asynk_ {
  spio_t_layer_asynk layer_asynk;
  spio_t_bits async_options;    /* options common to all "methods" */
  spio_t_async_info *read_async_info;
  spio_t_async_info *write_async_info;
  spio_t_buf write_buf;
  spio_t_async_info *fsync_async_info;
  spio_t_offset read_pos;       /* not including pending reads */
  /* [PM] 4.0.2 The idea is to let write_pos reflect the file position
     under the assumption that any pending write will succeed. I.e.,
     it is incremented as soon as a write is issued.
     
     write_buf_write_pos is the write_pos when write_buf last became
     non-empty. Normally write_pos will then be write_buf_write_pos +
     write_buf.extent but if this is not true it means that seek has
     changed the write_pos. If seek has happen the next write will
     have to do a write_sync to ensure write_buf is empty before write
     can add more data to it (and adjust write_buf_write_pos to
     reflect the new write_pos).
   */
  spio_t_offset write_pos;      /* [PM] 4.0.2 including data in write_buf ([PM] 4.0.2 no longer needed for partial writes) */
  spio_t_offset write_buf_write_pos; /* [PM] 4.0.2 the write_pos corresponding to the first byte of write_buf */

  spio_t_event *read_event;
  spio_t_event *write_event;

};

extern void spio_layer_asynk_destroy(spio_t_layer *layer);

extern spio_t_error_code spio_init_layer_async(spio_t_bits options);

/* only for spio_layer_async_{unix,win32} */
extern spio_t_error_code spio_layer_async_ioctl_default(SPIO *s, spio_t_ioctl_operation operation, void *ioctl_param, spio_t_bits options);


#endif  /* SPIO_LAYER_ASYNC_H_INCLUDED */
