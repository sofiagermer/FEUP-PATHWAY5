#ifndef SPIO_ASYNC_H_INCLUDED
#define SPIO_ASYNC_H_INCLUDED 1

#include "spio_types.h"
#include "spio_buf.h"

typedef struct spio_t_async_info_ spio_t_async_info;


#define SPIO_ASYNC_IN_USE(ASYNC_INFO) (((ASYNC_INFO) != NULL) && spio_async_in_use((ASYNC_INFO)))

extern int spio_async_in_use(spio_t_async_info *async_info);

extern spio_t_error_code spio_async_error(spio_t_async_info *async_info, spio_t_error_code *perrorno);

extern spio_t_error_code spio_async_return(spio_t_async_info **pasync_info, spio_t_offset *pfile_pos, size_t *pnbytes);
extern spio_t_error_code spio_async_return_buf(spio_t_async_info **pasync_info, spio_t_buf *buf);

extern void spio_async_free(spio_t_async_info *async_info);

extern spio_t_error_code spio_async_suspend(spio_t_async_info * const list[], int nent, spio_t_timespec* timeout);

#define SPIO_WAIT_FOR_ASYNC_INFO_OPTION_CANCEL SPIO_BIT(0)
#define SPIO_WAIT_FOR_ASYNC_INFO_OPTION_NONBLOCKING SPIO_NEXT_BIT(SPIO_WAIT_FOR_ASYNC_INFO_OPTION_CANCEL)
extern spio_t_error_code wait_for_async_info(spio_t_async_info *async_info, spio_t_bits options);

extern spio_t_error_code spio_init_async(spio_t_bits options);

#if SPIO_ASYNC_INTERNAL   /* internal defs */

#define SPIO_ASYNC_FLAG_IN_USE SPIO_BIT(0)
#define SPIO_ASYNC_FLAG_OWNS_BUF SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_IN_USE)
#define SPIO_ASYNC_FLAG_PRIVATE_1 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_OWNS_BUF)
#define SPIO_ASYNC_FLAG_PRIVATE_2 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_1)

#define SPIO_ASYNC_FLAG_PRIVATE_3 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_2)
#define SPIO_ASYNC_FLAG_PRIVATE_4 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_3)
#define SPIO_ASYNC_FLAG_PRIVATE_5 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_4)
#define SPIO_ASYNC_FLAG_PRIVATE_6 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_5)

#define SPIO_ASYNC_FLAG_PRIVATE_7 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_6)
#define SPIO_ASYNC_FLAG_PRIVATE_8 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_7)
#define SPIO_ASYNC_FLAG_PRIVATE_9 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_8)
#define SPIO_ASYNC_FLAG_PRIVATE_10 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_9)

#define SPIO_ASYNC_FLAG_PRIVATE_11 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_10)
#define SPIO_ASYNC_FLAG_PRIVATE_12 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_11)
#define SPIO_ASYNC_FLAG_PRIVATE_13 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_12)
#define SPIO_ASYNC_FLAG_PRIVATE_14 SPIO_NEXT_BIT(SPIO_ASYNC_FLAG_PRIVATE_13)


struct spio_t_async_info_ {
  spio_t_bits flags;
  spio_t_error_code code;       /* for spio_async_error, initialized to SPIO_E_ASYNC_INPROGRESS when operation issued */
  /* write data from and read data into buf->buf[buf_offset] to buf->buf[buf_offset+nbytes-1]
     If SPIO_ASYNC_FLAG_OWNS_BUF buf should be freed on spio_async_free etc.
   */
  size_t buf_offset;
  size_t nbytes;
  spio_t_buf *buf;              /* owned by spio_t_async_info_ if SPIO_ASYNC_FLAG_OWNS_BUF */
  spio_t_offset file_offset;
  spio_t_event *event;          /* not owned by spio_t_async_info_ */

  /* ... private data */
};

#define SPIO_ASYNC_INIT_OPTION_PRIVATE_1 SPIO_BIT(0)
#define SPIO_ASYNC_INIT_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_ASYNC_INIT_OPTION_PRIVATE_1)

spio_t_error_code spio_async_init(spio_t_async_info *async_info, spio_t_buf *buf, size_t buf_offset, size_t nbytes, spio_t_offset file_offset, spio_t_event *event, spio_t_bits async_options);
extern spio_t_error_code spio_async_reinit(spio_t_async_info *async_info);
extern void spio_async_deinit(spio_t_async_info *async_info);

#endif  /* SPIO_ASYNC_INTERNAL */


#if SPIO_WIN32
#include "spio_async_win32.h"
#define spio_async_free_os spio_async_free_win32
#define spio_async_poll_for_completion_os spio_async_poll_for_completion_win32
#define spio_async_suspend_os spio_async_suspend_win32
#define spio_init_async_os spio_init_async_win32
#define spio_async_cancel_os spio_async_cancel_win32


#elif SPIO_UNIX
#include "spio_async_unix.h"
#define spio_async_free_os spio_async_free_unix
#define spio_async_poll_for_completion_os spio_async_poll_for_completion_unix
#define spio_async_suspend_os spio_async_suspend_unix
#define spio_init_async_os spio_init_async_unix
#define spio_async_cancel_os spio_async_cancel_unix

#else
#error "NYI"
#endif

#endif  /* SPIO_ASYNC_H_INCLUDED */
