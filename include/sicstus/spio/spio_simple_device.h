#ifndef SPIO_SIMPLE_DEVICE_H_INCLUDED
#define SPIO_SIMPLE_DEVICE_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio.h"

#define SPIO_DEVICE_READ_OPTION_NONBLOCKING SPIO_BIT(0)
#define SPIO_DEVICE_READ_OPTION_BINARY SPIO_NEXT_BIT(SPIO_DEVICE_READ_OPTION_NONBLOCKING)
#define SPIO_DEVICE_READ_OPTION_TEXT SPIO_NEXT_BIT(SPIO_DEVICE_READ_OPTION_BINARY)

typedef spio_t_error_code SPIO_CDECL
spio_t_simple_device_read(void *user_data,
                          void *buf,
                          size_t *pbuf_size,
                          spio_t_bits read_options
                          );

#define SPIO_DEVICE_WRITE_OPTION_NONBLOCKING SPIO_BIT(0)
#define SPIO_DEVICE_WRITE_OPTION_BINARY SPIO_NEXT_BIT(SPIO_DEVICE_WRITE_OPTION_NONBLOCKING)
#define SPIO_DEVICE_WRITE_OPTION_TEXT SPIO_NEXT_BIT(SPIO_DEVICE_WRITE_OPTION_BINARY)

typedef spio_t_error_code SPIO_CDECL
spio_t_simple_device_write(void *user_data,
                           void const *buf,
                           size_t *pbuf_size,
                           spio_t_bits write_options
                           );


#define SPIO_DEVICE_FLUSH_OPTION_NONBLOCKING SPIO_BIT(0)

typedef spio_t_error_code SPIO_CDECL
spio_t_simple_device_flush_output(void *user_data,
                                  spio_t_bits flush_options
                                  );

typedef spio_t_error_code SPIO_CDECL
spio_t_simple_device_seek(void *user_data,
                          spio_t_offset offset,
                          spio_t_offset *new_pos,
                          void *reserved,
                          spio_t_bits seek_options
                          );

#define SPIO_DEVICE_CLOSE_OPTION_READ SPIO_BIT(0)
#define SPIO_DEVICE_CLOSE_OPTION_WRITE SPIO_NEXT_BIT(SPIO_DEVICE_CLOSE_OPTION_READ)
#define SPIO_DEVICE_CLOSE_OPTION_FORCE SPIO_NEXT_BIT(SPIO_DEVICE_CLOSE_OPTION_WRITE)

typedef spio_t_error_code SPIO_CDECL
spio_t_simple_device_close(void **puser_data,
                           spio_t_bits close_options
                           );

typedef void SPIO_CDECL
spio_t_simple_device_interrupt(void *user_data,
                               spio_t_bits interrupt_options
                               );

typedef spio_t_error_code SPIO_CDECL
spio_t_simple_device_ioctl(void *user_data,
                           spio_t_ioctl_operation operation,
                           void *param,
                           spio_t_bits ioctl_options
                           );


#define SPIO_CREATE_DEVICE_OPTION_BINARY SPIO_BIT(0)
#define SPIO_CREATE_DEVICE_OPTION_TEXT SPIO_NEXT_BIT(SPIO_CREATE_DEVICE_OPTION_BINARY)


extern spio_t_error_code SPIO_CDECL
spio_create_device(SPIO *s,
                   void *user_data,
                   void const *user_class,
                   spio_t_simple_device_read *user_read,
                   spio_t_simple_device_write *user_write,
                   spio_t_simple_device_flush_output *user_flush_output,
                   spio_t_simple_device_seek *user_seek,
                   spio_t_simple_device_close *user_close,
                   spio_t_simple_device_interrupt *user_interrupt,
                   spio_t_simple_device_ioctl *user_ioctl,
                   spio_t_bits create_options
                   );

extern spio_t_error_code spio_get_device_user_data(SPIO const *s, void const *user_class, void **puser_data, spio_t_bits options);


#endif  /* SPIO_SIMPLE_DEVICE_H_INCLUDED */
