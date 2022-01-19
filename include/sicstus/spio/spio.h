#ifndef SPIO_H_INCLUDED
#define SPIO_H_INCLUDED
#include "spio_types.h"
#include "spio_buf.h"
#include "spio_event.h"
#include "spio_utils.h"
#include "spio_errors.h"

#include <stddef.h>

#define SPIO_OPTION_NONE 0
/*

  Reserve some bits for use by layers methods etc. These will never be
  used by SPIO itself (but, of course, by individual SPIO layers).

   That is, layer implementations should define its options like:
   #define MY_FIRST_LAYER_METHOD_OPTION SPIO_PRIVATE_OPTION_1
   #define MY_SECOND_LAYER_METHOD_OPTION SPIO_PRIVATE_OPTION_2
   ...
 */
#define SPIO_OPTION_PRIVATE_BIT_OFFSET 20
#define SPIO_OPTION_PUBLIC_MASK ((0x1U << SPIO_OPTION_PRIVATE_BIT_OFFSET)-1)
#define SPIO_OPTION_PRIVATE_MASK (~ SPIO_OPTION_PUBLIC_MASK)

#define SPIO_PRIVATE_OPTION_0_  (1U << (SPIO_OPTION_PRIVATE_BIT_OFFSET+ 0)) /* only use as SPIO_OPTION_USE_PRIVATE */
#define SPIO_OPTION_USE_PRIVATE SPIO_PRIVATE_OPTION_0_

/* These are free to to use for private options (BUT NOT (anymore) FOR (layer-)FLAGS ETC) */
#define SPIO_PRIVATE_OPTION_1  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 1)
#define SPIO_PRIVATE_OPTION_2  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 2)
#define SPIO_PRIVATE_OPTION_3  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 3)
#define SPIO_PRIVATE_OPTION_4  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 4)
#define SPIO_PRIVATE_OPTION_5  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 5)
#define SPIO_PRIVATE_OPTION_6  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 6)
#define SPIO_PRIVATE_OPTION_7  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 7)
#define SPIO_PRIVATE_OPTION_8  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 8)
#define SPIO_PRIVATE_OPTION_9  SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+ 9)
#define SPIO_PRIVATE_OPTION_10 SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+10)
#define SPIO_PRIVATE_OPTION_11 SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+11)
/* #define SPIO_PRIVATE_OPTION_12 SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+12) */
/* #define SPIO_PRIVATE_OPTION_13 SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+13) */
/* #define SPIO_PRIVATE_OPTION_14 SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+14) */
/* #define SPIO_PRIVATE_OPTION_15 SPIO_BIT(SPIO_OPTION_PRIVATE_BIT_OFFSET+15) */

#define SPIO_OPTION_DEINIT SPIO_BIT(0)


/* forward declaration */
typedef struct spio_t_layer_ spio_t_layer;

/* 
   Should we make this *SPIO and pass SPIO *s to routines or use
   **SPIO to make it fully abstract and pass SPIO s to routines? One
   advantage with *SPIO, so that routines takes SPIO *s, is the
   similarity to how FILE *f is used in stdio-based code. The syntax
   SPIO *s also makes it clearer that the called routine can reach out
   via the pointer s and change things.

*/
typedef spio_t_layer *SPIO;     /* all routines should be passed a SPIO *s. */

enum spio_t_ioctl_operation_ {
  SPIO_IOCTL_UNDEFINED = 0,

  SPIO_IOCTL_READABLE_EVENT_,
#define SPIO_IOCTL_READABLE_EVENT_ SPIO_IOCTL_READABLE_EVENT_
#define SPIO_IOCTL_READABLE_EVENT ((spio_t_ioctl_operation)SPIO_IOCTL_READABLE_EVENT_)
  SPIO_IOCTL_WRITABLE_EVENT_,
#define SPIO_IOCTL_WRITABLE_EVENT_ SPIO_IOCTL_WRITABLE_EVENT_
#define SPIO_IOCTL_WRITABLE_EVENT ((spio_t_ioctl_operation)SPIO_IOCTL_WRITABLE_EVENT_)

  SPIO_IOCTL_DEVICE_TYPE_,
#define SPIO_IOCTL_DEVICE_TYPE_ SPIO_IOCTL_DEVICE_TYPE_
#define SPIO_IOCTL_DEVICE_TYPE ((spio_t_ioctl_operation)SPIO_IOCTL_DEVICE_TYPE_)

  /* Get name of character encoding (see spio_t_layer_coder for one implementation) */
  SPIO_IOCTL_GET_ENCODING_NAME_,
#define SPIO_IOCTL_GET_ENCODING_NAME_ SPIO_IOCTL_GET_ENCODING_NAME_
#define SPIO_IOCTL_GET_ENCODING_NAME ((spio_t_ioctl_operation)SPIO_IOCTL_GET_ENCODING_NAME_)

  SPIO_IOCTL_ENCODING_FALLBACK_,
#define SPIO_IOCTL_ENCODING_FALLBACK_ SPIO_IOCTL_ENCODING_FALLBACK_
#define SPIO_IOCTL_ENCODING_FALLBACK ((spio_t_ioctl_operation)SPIO_IOCTL_ENCODING_FALLBACK_)

  SPIO_IOCTL_SIZE_,
#define SPIO_IOCTL_SIZE_ SPIO_IOCTL_SIZE_
#define SPIO_IOCTL_SIZE ((spio_t_ioctl_operation)SPIO_IOCTL_SIZE_)

  SPIO_IOCTL_EVENT_LISTENER_,
#define SPIO_IOCTL_EVENT_LISTENER_ SPIO_IOCTL_EVENT_LISTENER_
#define SPIO_IOCTL_EVENT_LISTENER ((spio_t_ioctl_operation)SPIO_IOCTL_EVENT_LISTENER_)

  SPIO_IOCTL_GET_CODER_READ_POSITION_,
#define SPIO_IOCTL_GET_CODER_READ_POSITION_ SPIO_IOCTL_GET_CODER_READ_POSITION_
#define SPIO_IOCTL_GET_CODER_READ_POSITION ((spio_t_ioctl_operation)SPIO_IOCTL_GET_CODER_READ_POSITION_)

  SPIO_IOCTL_SET_CODER_READ_POSITION_,
#define SPIO_IOCTL_SET_CODER_READ_POSITION_ SPIO_IOCTL_SET_CODER_READ_POSITION_
#define SPIO_IOCTL_SET_CODER_READ_POSITION ((spio_t_ioctl_operation)SPIO_IOCTL_SET_CODER_READ_POSITION_)


  
  SPIO_IOCTL_LAST_PREDEFINED_,
#define SPIO_IOCTL_LAST_PREDEFINED_ SPIO_IOCTL_LAST_PREDEFINED_
#define SPIO_IOCTL_LAST_PREDEFINED ((spio_t_ioctl_operation)SPIO_IOCTL_LAST_PREDEFINED_)


#if 0
  SPIO_IOCTL_PRIVATE_ = 0x1000,
#define SPIO_IOCTL_PRIVATE_ SPIO_IOCTL_PRIVATE_
  SPIO_IOCTL_PRIVATE1,
#define SPIO_IOCTL_PRIVATE1 SPIO_IOCTL_PRIVATE1
  SPIO_IOCTL_PRIVATE2,
#define SPIO_IOCTL_PRIVATE2 SPIO_IOCTL_PRIVATE2
  SPIO_IOCTL_PRIVATE3,
#define SPIO_IOCTL_PRIVATE3 SPIO_IOCTL_PRIVATE3
#endif  /* 0 */
  SPIO_IOCTL_PRIVATE_LAST_
  };

#if 1
typedef void * spio_t_ioctl_operation;
#else  /* 0 */
typedef enum spio_t_ioctl_operation_ spio_t_ioctl_operation;
#endif  /* 0 */

typedef struct spio_t_funcs_ spio_t_funcs;
struct spio_t_funcs_ {
  size_t sizeof_spio_t_funcs;   /* version check */
  char const *name;
  size_t layer_size;            /* size of layer, i.e., >= sizeof(spio_t_layer) */
  /* void *id; */

#if 0                           /* do we need this */
#define SPIO_FUNCS_FEATURE_DEVICE 0x00000001 /* can be opened as lowest layer */
#endif
/* FIXME: CAN_RDONLY, CAN_WRONLY, CAN_RW, various seekability features */
  spio_t_bits features;       /* capabilites of layers of this class */
  spio_t_bits reserved;       /* pad */

  /* instance methods */
  /* read,write,seek,... */

#define SPIO_READ_OPTION_BINARY SPIO_BIT(0)
#define SPIO_READ_OPTION_TEXT SPIO_NEXT_BIT(SPIO_READ_OPTION_BINARY)

#define SPIO_READ_OPTION_NONBLOCKING SPIO_NEXT_BIT(SPIO_READ_OPTION_TEXT)
  spio_t_error_code (*read)(SPIO *s, spio_t_buf *buf, spio_t_bits options);

#define SPIO_WRITE_OPTION_BINARY SPIO_BIT(0)
#define SPIO_WRITE_OPTION_TEXT SPIO_NEXT_BIT(SPIO_WRITE_OPTION_BINARY)

#define SPIO_WRITE_OPTION_NONBLOCKING SPIO_NEXT_BIT(SPIO_WRITE_OPTION_TEXT)
#define SPIO_WRITE_OPTION_FORCE SPIO_NEXT_BIT(SPIO_WRITE_OPTION_NONBLOCKING) /* write even if !SPIO_LAYER_FLAG_CANWRITE */
#define SPIO_WRITE_OPTION_EOF SPIO_NEXT_BIT(SPIO_WRITE_OPTION_FORCE) /* this is the last block of data (tells layers to not wait for more lookahead) */
  spio_t_error_code (*write)(SPIO *s, spio_t_buf *buf, size_t *poffset, spio_t_bits options);

/* Do not write partial buffer except on error. If an error occurs
   update *pbuf_offset and return the error code.
   This is handled internally in spio_write(). Layer methods never see it.
*/
#define SPIO_WRITE_OPTION_DISALLOW_PARTIAL SPIO_NEXT_BIT(SPIO_WRITE_OPTION_EOF)

#define SPIO_SYNC_OPTION_NONBLOCKING SPIO_BIT(0)
#define SPIO_SYNC_OPTION_READ    SPIO_NEXT_BIT(SPIO_SYNC_OPTION_NONBLOCKING)
#define SPIO_SYNC_OPTION_WRITE   SPIO_NEXT_BIT(SPIO_SYNC_OPTION_READ)
#define SPIO_SYNC_OPTION_RECURSE SPIO_NEXT_BIT(SPIO_SYNC_OPTION_WRITE) /* call spio_sync on next layer */
#define SPIO_SYNC_OPTION_CLOSE   SPIO_NEXT_BIT(SPIO_SYNC_OPTION_RECURSE) /* called from close (so read-sync not needed, but write-sync should try really hard). Always combined with SPIO_SYNC_OPTION_RECURSE */
#define SPIO_SYNC_OPTION_POP SPIO_NEXT_BIT(SPIO_SYNC_OPTION_CLOSE) /* called from pop (used by pop methods when calling spio_sync) */
#define SPIO_SYNC_OPTION_BEST_EFFORT SPIO_NEXT_BIT(SPIO_SYNC_OPTION_POP) /* [PM] SP 4.0.2 sync fast rather than thoroughly (for SP autoflush) (only with SPIO_SYNC_OPTION_WRITE) */
#define SPIO_SYNC_OPTION_NO_FSYNC SPIO_NEXT_BIT(SPIO_SYNC_OPTION_BEST_EFFORT) /* [PM] SP 4.4.2 Do not bother waiting until the OS has written to disk (e.g. no fsync()). */

  spio_t_error_code (*sync)(SPIO *s, spio_t_bits options);

#define SPIO_SEEK_OPTION_SET SPIO_BIT(0)                          /* corresponds to SEEK_SET */
#define SPIO_SEEK_OPTION_CUR SPIO_NEXT_BIT(SPIO_SEEK_OPTION_SET)  /* corresponds to SEEK_CUR */
#define SPIO_SEEK_OPTION_END SPIO_NEXT_BIT(SPIO_SEEK_OPTION_CUR)  /* corresponds to SEEK_END */
#define SPIO_SEEK_OPTION_DATA SPIO_NEXT_BIT(SPIO_SEEK_OPTION_END) /* Like .._CUR (offset non-positive) using data from buf->extent backwards */

#if !SPIO_BETA_VERSION
#error "We need a SPIO_SEEK_OPTION_NONBLOCKING (e.g., for layers, like spio_layer_seek, that needs to seek forward by reading)"
#endif  /* !SPIO_BETA_VERSION */

#define SPIO_SEEK_OPTION_READ SPIO_NEXT_BIT(SPIO_SEEK_OPTION_DATA)  /* seek in read direction */
#define SPIO_SEEK_OPTION_WRITE SPIO_NEXT_BIT(SPIO_SEEK_OPTION_READ) /* seek in write direction */

  spio_t_error_code (*seek)(SPIO *s, spio_t_offset offset, spio_t_offset *new_pos, spio_t_buf *buf, spio_t_bits options);

#define SPIO_CLOSE_OPTION_ABORT SPIO_BIT(0)
#define SPIO_CLOSE_OPTION_READ SPIO_NEXT_BIT(SPIO_CLOSE_OPTION_ABORT)
#define SPIO_CLOSE_OPTION_WRITE SPIO_NEXT_BIT(SPIO_CLOSE_OPTION_READ)
#define SPIO_CLOSE_OPTION_NONBLOCKING SPIO_NEXT_BIT(SPIO_CLOSE_OPTION_WRITE)
#define SPIO_CLOSE_OPTION_NO_FSYNC SPIO_NEXT_BIT(SPIO_CLOSE_OPTION_NONBLOCKING)

  spio_t_error_code (*close)(SPIO *s, spio_t_bits options);

#define SPIO_PUSH_OPTION_POPPABLE      SPIO_BIT(0) /* Try to ensure layer can be popped (e.g., read-synced) */
#define SPIO_PUSH_OPTION_SEEKABLE SPIO_NEXT_BIT(SPIO_PUSH_OPTION_POPPABLE) /* Try to ensure layer can be seeked (expensive/impossible in general) */

#if 0
   #define SPIO_PUSH_OPTION_READ_EOL_AUTO SPIO_NEXT_BIT(SPIO_PUSH_OPTION_SEEKABLE)
   #define SPIO_PUSH_OPTION_READ_EOL_CRLF SPIO_NEXT_BIT(SPIO_PUSH_OPTION_READ_EOL_AUTO)
   #define SPIO_PUSH_OPTION_READ_EOL_OS SPIO_NEXT_BIT(SPIO_PUSH_OPTION_READ_EOL_CRLF)
   #define SPIO_PUSH_OPTION_READ_EOL_MASK (SPIO_PUSH_OPTION_READ_EOL_AUTO|SPIO_PUSH_OPTION_READ_EOL_CRLF|SPIO_PUSH_OPTION_READ_EOL_OS)
   #define SPIO_PUSH_OPTION_WRITE_EOL_AUTO SPIO_NEXT_BIT(SPIO_PUSH_OPTION_READ_EOL_OS)
   #define SPIO_PUSH_OPTION_WRITE_EOL_CRLF SPIO_NEXT_BIT(SPIO_PUSH_OPTION_WRITE_EOL_AUTO)
   #define SPIO_PUSH_OPTION_WRITE_EOL_OS SPIO_NEXT_BIT(SPIO_PUSH_OPTION_WRITE_EOL_CRLF)
   #define SPIO_PUSH_OPTION_WRITE_EOL_MASK (SPIO_PUSH_OPTION_WRITE_EOL_AUTO|SPIO_PUSH_OPTION_WRITE_EOL_CRLF|SPIO_PUSH_OPTION_WRITE_EOL_OS)
#endif  /* 0 */

  spio_t_error_code (*push)(spio_t_funcs *funcs, SPIO *s, spio_t_arglist args, spio_t_bits options);

  /* Not yet
#define SPIO_POP_OPTION_FORCE SPIO_BIT(0)
  */
  spio_t_error_code (*pop)(spio_t_funcs *funcs, SPIO *s, spio_t_bits options);

  void (*destroy)(spio_t_layer *layer);

  /* static methods */
  /* open,... */
#define SPIO_OPEN_OPTION_READ   SPIO_BIT(0)
#define SPIO_OPEN_OPTION_WRITE  SPIO_NEXT_BIT(SPIO_OPEN_OPTION_READ)
#define SPIO_OPEN_OPTION_APPEND SPIO_NEXT_BIT(SPIO_OPEN_OPTION_WRITE)
/* Only if opening in direction write (and read-write): Barf with SPIO_E_FILE_EXISTS if the file already exists. */
#define SPIO_OPEN_OPTION_NEW SPIO_NEXT_BIT(SPIO_OPEN_OPTION_APPEND)

/* close should not close the file handle (this option is only passed
   to lower-level versions of spio_open). It corresponds to
   SPIO_LAYER_FLAG_KEEP_FILE_HANDLE */
#define SPIO_OPEN_OPTION_KEEP_FILE_HANDLE SPIO_NEXT_BIT(SPIO_OPEN_OPTION_NEW)
/* Let the file handle keep track of the file position (for inherited
   file handles like STDIN_FILENO). (only passed to lower-level
   versions of spio_open). */
#define SPIO_OPEN_OPTION_OBEY_FILE_OFFSET SPIO_NEXT_BIT(SPIO_OPEN_OPTION_KEEP_FILE_HANDLE)
#if 0
#define SPIO_OPEN_OPTION_READWRITE (SPIO_OPEN_OPTION_READ | SPIO_OPEN_OPTION_WRITE)
#endif
#if 0                           /* do we need this? */
#define SPIO_OPEN_OPTION_DEVICE ... /* layer will be acting as device (bottom-most layer) */
#endif
#if 0                           /* not used yet */
#define SPIO_OPEN_OPTION_SEEKABLE_FORWARD ...
#define SPIO_OPEN_OPTION_SEEKABLE_BACK ...
#endif

  spio_t_error_code (*open)(spio_t_funcs *funcs, SPIO *s, spio_t_arglist args, spio_t_bits options);

  /* [PM] 4.0 I know how K&R felt. Having to add ioctl feels like a design failure. */
  spio_t_error_code (*ioctl)(SPIO *s, spio_t_ioctl_operation operation, void *ioctl_param, spio_t_bits options);

  /* private class members */
  /* ... */
};


struct spio_t_layer_ {
  spio_t_funcs *funcs;          /* open must set this */
  spio_t_layer *next;           /* open must set this */

  /*
    State of a layer as represented by its flags:

    SPIO_LAYER_FLAG_READ      -- this is a read layer

      This is a static property, it tells whether this is, or ever
      was, a read layer even if it is now closed in the read
      direction.
      
      If this is false then it should be treated much like a false
      SPIO_LAYER_FLAG_OPEN_READ.

    SPIO_LAYER_FLAG_CANREAD -- it is possible to read from this layer
    
      Implies SPIO_LAYER_FLAG_READ and SPIO_LAYER_FLAG_OPEN_READ and
      !SPIO_LAYER_FLAG_CLOSING_READ

      This is the only flag you need to look at to determine whether
      reading from the layer makes sense.

       
      If this is false then, in effect, the read direction is
      closed. In reality the read direction may be in the processes of
      closing. In either case, spio_read refuses to read.

      We could simply clear SPIO_LAYER_FLAG_READ but it is
      advantageous to have a persistent flag that tells whether the
      layer is a read layer or not. If nothing else it makes for more
      informative errors; trying to read from a closed read-stream is
      different from trying to read from a write-only stream.


    SPIO_LAYER_FLAG_OPEN_READ -- this layer is open in direction READ

      If the layer is not open then the spio_read will refuse to read
      from the layer.

      Implies SPIO_LAYER_FLAG_READ. immediately after open it also
      implies SPIO_LAYER_FLAG_CANREAD.

      

    SPIO_LAYER_FLAG_CLOSING_READ -- this layer has started closing 
      
      Mostly this should be treated as if SPIO_LAYER_FLAG_OPEN_READ
      was false. It is used to tell spio_close how far along the close
      procedure the layer chain has proceeded.

      Implies !SPIO_LAYER_FLAG_CANREAD and SPIO_LAYER_FLAG_OPEN_READ

      
    Pushing a (non-device) read layer is only allowed on a layer that
    is SPIO_LAYER_FLAG_CANREAD. Pushing a write layer is possible,
    provided the corresponing WRITE flags are set.
FIXME: should this be enforced or should individual push methods check
this?
    
   
    SPIO_LAYER_FLAG_WRITE
    SPIO_LAYER_FLAG_CANWRITE
    SPIO_LAYER_FLAG_OPEN_WRITE
    SPIO_LAYER_FLAG_CLOSING_WRITE

      Similar for direction write.
  

    Special cases:

    SPIO_LAYER_FLAG_READ|SPIO_LAYER_FLAG_WRITE -- bidirectional layer
    SPIO_LAYER_FLAG_OPEN_READ|SPIO_LAYER_FLAG_OPEN_WRITE -- open in both directions
    SPIO_LAYER_FLAG_CLOSING_READ|SPIO_LAYER_FLAG_CLOSING_WRITE -- partially closed in both directions

    !(SPIO_LAYER_FLAG_OPEN_READ|SPIO_LAYER_FLAG_OPEN_WRITE) -- illegal
       If both directions are closed then the layer should be removed (popped) from the layer chain.

    (SPIO_LAYER_FLAG_OPEN_READ && !SPIO_LAYER_FLAG_READ) -- illegal

       Only a read layer can be open in the read direction.

    
   */

#if 0                           /* not used */
#define SPIO_LAYER_FLAG_DEVICE     SPIO_BIT(0)
#endif

#define SPIO_LAYER_FLAG_READ       SPIO_BIT(0) /* read direction exists */
#define SPIO_LAYER_FLAG_CANREAD    SPIO_NEXT_BIT(SPIO_LAYER_FLAG_READ) /* can read in read directions */
#define SPIO_LAYER_FLAG_WRITE      SPIO_NEXT_BIT(SPIO_LAYER_FLAG_CANREAD)
#define SPIO_LAYER_FLAG_CANWRITE   SPIO_NEXT_BIT(SPIO_LAYER_FLAG_WRITE)
#define SPIO_LAYER_FLAG_OPEN_READ  SPIO_NEXT_BIT(SPIO_LAYER_FLAG_CANWRITE) /* still open (i.e., not fully closed) in read direction */
#define SPIO_LAYER_FLAG_OPEN_WRITE SPIO_NEXT_BIT(SPIO_LAYER_FLAG_OPEN_READ)
/* do not close the OS file handle when this (device-) layer is closed. */
#define SPIO_LAYER_FLAG_KEEP_FILE_HANDLE SPIO_NEXT_BIT(SPIO_LAYER_FLAG_OPEN_WRITE)
/* layer (-chain) has started closing */
#define SPIO_LAYER_FLAG_CLOSING_READ  SPIO_NEXT_BIT(SPIO_LAYER_FLAG_KEEP_FILE_HANDLE)
#define SPIO_LAYER_FLAG_CLOSING_WRITE SPIO_NEXT_BIT(SPIO_LAYER_FLAG_CLOSING_READ) /* closing in progress for read direction */

/* seekability (do we really need these? Consider making them private in spio_layer_unix.c) YES: need SPIO_LAYER_FLAG_SEEKABLE_DATA (or make it mandatory) */
#define SPIO_LAYER_FLAG_SEEKABLE_OFFSET SPIO_NEXT_BIT(SPIO_LAYER_FLAG_CLOSING_WRITE)
/* [PM] 4.0.5 Note: SPIO_LAYER_FLAG_SEEKABLE_DATA is implied by
   SPIO_LAYER_FLAG_SEEKABLE_OFFSET so you need to check for both! */
#define SPIO_LAYER_FLAG_SEEKABLE_DATA SPIO_NEXT_BIT(SPIO_LAYER_FLAG_SEEKABLE_OFFSET)

#define SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_1 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_SEEKABLE_DATA)
#define SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_2 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_1)
#define SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_4 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_2)

#define SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_1 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_4)
#define SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_2 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_1)
#define SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_4 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_2)


#define SPIO_LAYER_FLAG_PRIVATE_1 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_4)
#define SPIO_LAYER_FLAG_PRIVATE_2 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_PRIVATE_1)
#define SPIO_LAYER_FLAG_PRIVATE_3 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_PRIVATE_2)
#define SPIO_LAYER_FLAG_PRIVATE_4 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_PRIVATE_3)
#define SPIO_LAYER_FLAG_PRIVATE_5 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_PRIVATE_4)
#define SPIO_LAYER_FLAG_PRIVATE_6 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_PRIVATE_5)
#define SPIO_LAYER_FLAG_PRIVATE_7 SPIO_NEXT_BIT(SPIO_LAYER_FLAG_PRIVATE_6)

#define SPIO_LAYER_FLAG_LAST_ SPIO_LAYER_FLAG_PRIVATE_7

#define SPIO_READ_GRANULARITY(FLAGS) (size_t)(                  \
   (((FLAGS) & SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_1) ? 1 : 0) \
   +                                                            \
   (((FLAGS) & SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_2) ? 2 : 0) \
   +                                                            \
   (((FLAGS) & SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_4) ? 4 : 0) \
   )

#define SPIO_SET_READ_GRANULARITY(FLAGS,GRANULARITY) (                \
   (FLAGS) |=                                                         \
   (((GRANULARITY) & 1) ? SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_1 : 0) \
   |                                                                  \
   (((GRANULARITY) & 2) ? SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_2 : 0) \
   |                                                                  \
   (((GRANULARITY) & 4) ? SPIO_LAYER_FLAG_READ_GRANULARITY_BIT_4 : 0) \
   )

#define SPIO_WRITE_GRANULARITY(FLAGS) (size_t)(                         \
   (((FLAGS) & SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_1) ? 1 : 0)        \
   +                                                                    \
   (((FLAGS) & SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_2) ? 2 : 0)        \
   +                                                                    \
   (((FLAGS) & SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_4) ? 4 : 0)        \
   )

#define SPIO_SET_WRITE_GRANULARITY(FLAGS,GRANULARITY) (                \
   (FLAGS) |=                                                          \
   (((GRANULARITY) & 1) ? SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_1 : 0) \
   |                                                                   \
   (((GRANULARITY) & 2) ? SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_2 : 0) \
   |                                                                   \
   (((GRANULARITY) & 4) ? SPIO_LAYER_FLAG_WRITE_GRANULARITY_BIT_4 : 0) \
   )



  spio_t_bits flags;          /* open must set this */
  spio_t_bits pad;
  /* [PM] 4.2 Update spio_layer_init(), spio_layer_deinit() if fields are added */
  /* private data */
  /* ... */
};

extern void spio_layer_init(spio_t_layer *layer, spio_t_funcs *funcs, spio_t_bits flags);
extern void spio_layer_deinit(spio_t_layer *layer);

#if 0                           /* [PM] SPIO_PRIVATE_OPTION_<n> should no longer be used for private flags. */
extern int SPIO_LAYER_FLAG_LAST_range_assertion[(SPIO_LAYER_FLAG_LAST_ < SPIO_PRIVATE_OPTION_0_) ? 1 : -1]; /* booby trap */
#endif

#if 0
typedef struct spio_t_layer_buffered_ spio_t_layer_buffered;
struct spio_t_layer_buffered_ {
  spio_t_layer layer;
  spio_t_buf *read_buf;        /* binary entities (e.g., binary bytes) */
  spio_t_buf *write_buf;       /* binary entities (e.g., binary bytes) */
};

/* should we add a new struct (that inherits from spio_t_layer?) 
   for the top-level of a spio stream?

   Consider: Having separate binary and character buffers that point
   at the real spio_t_layer_buffered buffers or empty dummy buffer for
   the variant that does not apply. I.e., a character stream would
   have the binary buffers point at empty dummy buffers to ensure
   binary read/write goes through the slow path which can detect the
   mode mismatch. Similarly binary stream would have empty character
   buffers.

  UPDATE: It makes a lot of sense to do as PerlIO and have most
  routines that takes a spio stream take it as a **layer. That way it
  is possible to get at the location that refers to this layer (so
  this layer can be removed) even though that location is not a layer
  itself (e.g., the top-most layer may be some struct used by the
  client to represent its streams). Thus *f->prev will be NULL for the
  top-most layer. Early SFIO solved the problem of inserting/removing
  topmost layer by copying the contents of the layer struct into the
  top-most struct, more recent SFIO use a dummy top-most layer that is
  never removed. It would seem that the PerlIO method of using an
  extra indirection is better.

*/
#endif

/* pass SPIO_OPTION_DEINIT to deinit */
extern spio_t_error_code spio_init(spio_t_allocator_hook *allocator, void *cookie, spio_t_init_options const *init_options, spio_t_bits options);

#if SPIO_DEBUG
#define spio_alloc(SIZE) spio_alloc_((SIZE) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_realloc(PTR, SIZE) spio_realloc_((PTR), (SIZE) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_free(PTR) spio_free_((PTR) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_alloc_string(STRING) spio_alloc_string_((STRING) SPIO_ALLOCATOR_DEBUG_ARGS)
#else  /* !SPIO_DEBUG */
#define spio_alloc_ spio_alloc
#define spio_realloc_ spio_realloc
#define spio_free_ spio_free
#define spio_alloc_string_ spio_alloc_string
#endif  /* !SPIO_DEBUG */

extern void *spio_alloc_(size_t size SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

extern void *spio_alloc_string_(char const *string SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

extern void *spio_realloc_(void *, size_t size SPIO_ALLOCATOR_DEBUG_ARGS_DECL);
extern void spio_free_(void * SPIO_ALLOCATOR_DEBUG_ARGS_DECL);



/* layer registration */
extern spio_t_error_code spio_register_layer(spio_t_funcs *funcs, spio_t_bits options);
extern spio_t_error_code spio_find_layer(char const *name, spio_t_funcs **funcs, spio_t_bits options);

/* layer functions */
extern spio_t_error_code spio_open(spio_t_funcs *funcs, SPIO *s, spio_t_arglist args, spio_t_bits options);
extern spio_t_error_code spio_open_by_name(char const *layer_name, SPIO *s, spio_t_arglist args, spio_t_bits options);
extern spio_t_error_code spio_close(SPIO *s, spio_t_bits open_options);
extern spio_t_error_code spio_push(spio_t_funcs *funcs, SPIO *s, spio_t_arglist args, spio_t_bits options);
extern spio_t_error_code spio_push_by_name(char const *layer_name, SPIO *s, spio_t_arglist push_args, spio_t_bits push_options);
extern spio_t_error_code spio_pop(spio_t_funcs *funcs, SPIO *s, spio_t_bits options);
extern spio_t_error_code spio_read(SPIO *s, spio_t_buf **pbuf, spio_t_bits options);
extern spio_t_error_code spio_write(SPIO *s, spio_t_buf *buf, size_t *poffset, spio_t_bits options);
extern spio_t_error_code spio_seek(SPIO *s, spio_t_offset byte_offset, spio_t_offset *pnew_pos, spio_t_buf *buf, spio_t_bits options);
extern spio_t_error_code spio_sync(SPIO *s, spio_t_bits options);

#define SPIO_GET_SPIO_LAYER_OPTION_NEXT SPIO_BIT(0)
extern spio_t_error_code spio_get_spio_layer(SPIO const *s, spio_t_funcs *funcs, SPIO *spio_layer, spio_t_bits options);

/* ioctl functions */

#define SPIO_IOCTL_OPTION_RECURSE SPIO_BIT(0)
#define SPIO_IOCTL_OPTION_PRIVATE1 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_RECURSE)
#define SPIO_IOCTL_OPTION_PRIVATE2 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_PRIVATE1)
#define SPIO_IOCTL_OPTION_PRIVATE3 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_PRIVATE2)
#define SPIO_IOCTL_OPTION_PRIVATE4 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_PRIVATE3)
#define SPIO_IOCTL_OPTION_PRIVATE5 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_PRIVATE4)
#define SPIO_IOCTL_OPTION_PRIVATE6 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_PRIVATE5)
#define SPIO_IOCTL_OPTION_PRIVATE7 SPIO_NEXT_BIT(SPIO_IOCTL_OPTION_PRIVATE6)
spio_t_error_code spio_ioctl(SPIO *s, spio_t_ioctl_operation operation, void *param, spio_t_bits options);


/* For implementing SPIO_IOCTL_READABLE_EVENT and SPIO_IOCTL_WRITABLE_EVENT */
typedef struct spio_t_ioctl_spio_set_ spio_t_ioctl_spio_set;
struct spio_t_ioctl_spio_set_ {
  spio_t_ioctl_operation operation;
  SPIO **spios;                 /* spios[nitems] */
  void **ids;                   /* ids[nitems] or ids == NULL */
  size_t nitems;
  void *user_data;              /* operations specific */
};

spio_t_error_code spio_ioctl_readable_event(SPIO *s, spio_t_event **pevent, spio_t_bits options);

spio_t_error_code spio_ioctl_writable_event(SPIO *s, spio_t_event **pevent, spio_t_bits options);

#define SPIO_IOCTL_DEVICE_TYPE_OPTION_READ  SPIO_IOCTL_OPTION_PRIVATE1
#define SPIO_IOCTL_DEVICE_TYPE_OPTION_WRITE SPIO_IOCTL_OPTION_PRIVATE2
spio_t_error_code spio_ioctl_device_type(SPIO *s, spio_t_device_type *ptype, spio_t_bits options);

#define SPIO_IOCTL_ENCODING_FALLBACK_OPTION_READ  SPIO_IOCTL_OPTION_PRIVATE1
#define SPIO_IOCTL_ENCODING_FALLBACK_OPTION_WRITE SPIO_IOCTL_OPTION_PRIVATE2

#define SPIO_IOCTL_GET_ENCODING_NAME_OPTION_READ  SPIO_IOCTL_OPTION_PRIVATE1
#define SPIO_IOCTL_GET_ENCODING_NAME_OPTION_WRITE SPIO_IOCTL_OPTION_PRIVATE2
spio_t_error_code spio_ioctl_get_encoding_name(SPIO *s, char const **pname, spio_t_bits options);

/* See SPIO_CHAR_CODER_ENCODE_OPTION_xxx in spio_char_coder.h for the meaning of these */
#define SPIO_IOCTL_ENCODING_FALLBACK_OPTION_FALLBACK_ON_INVALID SPIO_IOCTL_OPTION_PRIVATE3
/* #define SPIO_IOCTL_ENCODING_FALLBACK_OPTION_FALLBACK_DEFAULT SPIO_IOCTL_OPTION_PRIVATE4 */
#define SPIO_IOCTL_ENCODING_FALLBACK_OPTION_FALLBACK_ERROR SPIO_IOCTL_OPTION_PRIVATE5
#define SPIO_IOCTL_ENCODING_FALLBACK_OPTION_FALLBACK_REPLACE SPIO_IOCTL_OPTION_PRIVATE6
spio_t_error_code spio_ioctl_encoding_fallback(SPIO *s, spio_t_bits options);

#define SPIO_IOCTL_SIZE_OPTION_READ SPIO_IOCTL_OPTION_PRIVATE1
#define SPIO_IOCTL_SIZE_OPTION_WRITE SPIO_IOCTL_OPTION_PRIVATE2
/* #define SPIO_IOCTL_SIZE_OPTION_TRY_HARDER SPIO_IOCTL_OPTION_PRIVATE3 (for char coder layers) */
spio_t_error_code spio_ioctl_size(SPIO *s, spio_t_offset *psize, spio_t_bits options);

spio_t_error_code spio_ioctl_set_event_listener(SPIO *s, spio_t_event_listener *listener, void *data, spio_t_bits options);

/* helpers */
extern void spio_layer_pop(SPIO *s);
extern void spio_layer_push(SPIO *s, spio_t_layer *layer);
extern void spio_layer_destroy(spio_t_layer *layer);

/* dbg */
extern spio_t_error_code spio_write_dbg(SPIO *s, char const *text);


/* interrupting */
#define SPIO_INTERRUPT_OPTION_1 SPIO_BIT(0)
#define SPIO_INTERRUPT_OPTION_SOFT SPIO_INTERRUPT_OPTION_1
#define SPIO_INTERRUPT_OPTION_2 SPIO_NEXT_BIT(SPIO_INTERRUPT_OPTION_1)
#define SPIO_INTERRUPT_OPTION_HARD SPIO_INTERRUPT_OPTION_2
#define SPIO_INTERRUPT_OPTION_3 SPIO_NEXT_BIT(SPIO_INTERRUPT_OPTION_2)
#define SPIO_INTERRUPT_OPTION_4 SPIO_NEXT_BIT(SPIO_INTERRUPT_OPTION_3)
#define SPIO_INTERRUPT_OPTION_5 SPIO_NEXT_BIT(SPIO_INTERRUPT_OPTION_4)
#define SPIO_INTERRUPT_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_INTERRUPT_OPTION_5)
extern void spio_interrupt(spio_t_bits options); /* schedules all interrupt hooks to run (async-signal safe) */

/* Ensures all interrupt hooks (running in another thread) has run to
   completion before it returns. Should be called by the lowest levels
   before generating a SPIO_E_INTERRUPTED.
*/
extern void spio_ensure_interrupts_done(void);
extern void spio_clear_interrupts(void);

#if SPIO_INTERRUPTED
extern int spio_interrupted(void);
extern void spio_clear_interrupts(void);
#endif

extern int spio_test_and_clear_hard_interrupt(void);
extern spio_t_error_code spio_set_hard_interrupt(void);

#define SPIO_INTERRUPT_HOOK_OPTION_CLEAR_INTERRUPT SPIO_BIT(0)
#define SPIO_INTERRUPT_HOOK_OPTION_HARD_INTERRUPT SPIO_NEXT_BIT(SPIO_INTERRUPT_HOOK_OPTION_CLEAR_INTERRUPT)
typedef void spio_t_interrupt_hook_fun(void *cookie, spio_t_bits options);
extern spio_t_error_code spio_register_interrupt_hook(spio_t_interrupt_hook_fun* fun, void*cookie, spio_t_bits options);

#define SPIO_STANDARD_STREAM_OPTION_STREAM_BITS  SPIO_BIT(0)
#define SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_1 SPIO_NEXT_BIT(SPIO_STANDARD_STREAM_OPTION_STREAM_BITS)
#define SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_2 SPIO_NEXT_BIT(SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_1)
#define SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_MASK (SPIO_STANDARD_STREAM_OPTION_STREAM_BITS|SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_1|SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_2)
#define SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_HIGHEST SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_2

/* stream designators valid for both spio_{set,get}_standard_stream */
#define SPIO_STANDARD_STREAM_OPTION_INPUT_STREAM  SPIO_STANDARD_STREAM_OPTION_STREAM_BITS|0 /* idx 0 */
#define SPIO_STANDARD_STREAM_OPTION_OUTPUT_STREAM SPIO_STANDARD_STREAM_OPTION_STREAM_BITS|SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_1 /* idx 1 */
#define SPIO_STANDARD_STREAM_OPTION_ERROR_STREAM  SPIO_STANDARD_STREAM_OPTION_STREAM_BITS|SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_2 /* idx 2 */
#if 0
  /* bidirectional /dev/null-style stream. */
  #define SPIO_STANDARD_STREAM_OPTION_NULL_STREAM   SPIO_STANDARD_STREAM_OPTION_STREAM_BITS|SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_2|SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_1 /* idx 3 */
#endif  /* 0 */
#define SPIO_STANDARD_STREAM_OPTION_LAST_COMMON_OPTION_ SPIO_STANDARD_STREAM_OPTION_STREAM_BIT_HIGHEST

/* If no standard stream is avaialable then return a "similar" NULL stream.
   This is especially useful on Windows where there typically are no standard streams for a non-console application.
 */
#define SPIO_GET_STANDARD_STREAM_OPTION_FALLBACK_TO_NULL_STREAM SPIO_NEXT_BIT(SPIO_STANDARD_STREAM_OPTION_LAST_COMMON_OPTION_)

/* close these directions of the stream when a new stream is installed */
#define SPIO_SET_STANDARD_STREAM_OPTION_CLOSE_READ SPIO_NEXT_BIT(SPIO_STANDARD_STREAM_OPTION_LAST_COMMON_OPTION_)
#define SPIO_SET_STANDARD_STREAM_OPTION_CLOSE_WRITE SPIO_NEXT_BIT(SPIO_SET_STANDARD_STREAM_OPTION_CLOSE_READ)

extern spio_t_error_code spio_set_standard_stream(SPIO *s, spio_t_bits options);

#if 0
  /* if the requested stream does not exist, return a null stream instead */
  #define SPIO_GET_STANDARD_STREAM_OPTION_FALLBACK_TO_NULL_STREAM SPIO_NEXT_BIT(SPIO_STANDARD_STREAM_OPTION_LAST_COMMON_OPTION_)
#endif  /* 0 */

extern spio_t_error_code spio_get_standard_stream(SPIO *s, spio_t_bits options);

#if SPIO_WIN32
extern spio_t_error_code spio_open_win32_handle(SPIO *s, void *handle, spio_t_bits options);
#endif  /* SPIO_WIN32 */

#if SPIO_UNIX
extern spio_t_error_code spio_open_file_descriptor(SPIO *s, int fd, spio_t_bits options);
#endif  /* SPIO_UNIX */

extern spio_t_error_code spio_owns_biglock(void);

#if SPIO_USE_LOCK
extern spio_t_error_code spio_biglock_lock(void);
extern spio_t_error_code spio_biglock_unlock(void);

/* This must be a recursive lock */
#define SPIO_LOCK() (void)spio_biglock_lock()
#define SPIO_UNLOCK() (void)spio_biglock_unlock()
#else                 /* !SPIO_USE_LOCK */
#define SPIO_LOCK()             /* empty */
#define SPIO_UNLOCK()           /* empty */
#endif                          /* !SPIO_USE_LOCK */
#endif  /* SPIO_H_INCLUDED */
