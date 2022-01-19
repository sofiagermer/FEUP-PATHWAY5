#ifndef SICSTUS_SPIO_H_INCLUDED
#define SICSTUS_SPIO_H_INCLUDED 1

#if !INCLUDED_FROM_RUNTIME
#if !defined(SPIO_ASSERTIONS_ENABLED)
/* This avoids referring to the external symbol spio_assertions for
   doing a runtime test on whether assertions should be
   disabled. Instead it makes assertion always on (provided they are
   enabled at compile-time, with SPIO_ASSERTIONS).
*/
#define SPIO_ASSERTIONS_ENABLED 1
#endif	/* !defined(SPIO_ASSERTIONS_ENABLED) */
#ifndef SPIO_ASSERT_FAILURE_
#define SPIO_ASSERT_FAILURE_ sp_spio_assert_failure
#endif  /* SPIO_ASSERT_FAILURE_ */
#ifndef SPIO_DEBUG_BREAK_
#define SPIO_DEBUG_BREAK_ sp_spio_debug_break
#endif  /* SPIO_DEBUG_BREAK_ */
#ifndef SPIO_ERROR_NAME_
#define SPIO_ERROR_NAME_ sp_spio_error_name
#endif  /* SPIO_ERROR_NAME_ */
#endif  /* !INCLUDED_FROM_RUNTIME */
#ifndef SPIO_TRACE_LINE_
#define SPIO_TRACE_LINE_ sp_spio_trace_line
#endif  /* SPIO_TRACE_LINE_ */

#define SP_ASSERT(TEST) SPIO_ASSERT1(1,(TEST))
#define SP_SOFT_ASSERT(TEST) SPIO_SOFT_ASSERT1(1,(TEST))

/* [PM] 4.0.5 _after_ defining new defaults for SPIO_ASSERT_FAILURE_ et al above */
#include "spio/spio_debug.h"

#ifndef SPIO_DEBUG_H_INCLUDED
#error "expected spio_debug.h to be included here"
#endif
#if SPIO_ASSERTIONS
#define SP_ASSERTIONS 1         /* for testing whether assertions are no-ops */
#endif  /* SPIO_ASSERTIONS */

/* not <spio/spio_simple_device.h> */
#include "spio/spio_simple_device.h"

#include "spio/spio_char_coder.h"
#include "spio/spio_char_coder_utf8.h"/* SPIO_CHAR_CODER_OPEN_OPTION_UTF8_TCL */

typedef struct sp_t_spio_stream_node_ SP_stream; /* forward declaration */

typedef struct sp_t_stream_buf_info_binary_ sp_t_stream_buf_info_binary;
struct sp_t_stream_buf_info_binary_ {
  unsigned char *ptr;
  unsigned char *limit;
};

/* SP_uint32 is defined in the non-public part of config.h
   Luckily int is 32 bit on all our (32 and 64 bit) platforms
*/
#ifndef SP_uint32
typedef unsigned int SP_uint32;
#define SP_uint32 SP_uint32
#endif

typedef struct sp_t_stream_buf_info_text_ sp_t_stream_buf_info_text;
struct sp_t_stream_buf_info_text_ {
  SP_uint32 *ptr;
  SP_uint32 *limit;
};

typedef struct sp_t_public_stream_fields_ sp_t_public_stream_fields;
struct sp_t_public_stream_fields_ {
  sp_t_stream_buf_info_binary input_binary;
  sp_t_stream_buf_info_binary output_binary;
  sp_t_stream_buf_info_text input_text;
  sp_t_stream_buf_info_text output_text;
};

#define SP_CAST_STREAM_(PTR) ((sp_t_public_stream_fields *)(PTR))

#define SP_GET_BYTE_HELPER_OPTION_NONBLOCKING      0x0001
#define SP_GET_BYTE_HELPER_OPTION_PEEK             0x0002
#define SP_GET_BYTE_HELPER_OPTION_NO_RESTART       0x0004

#define SP_GET_BYTE_OPTION_NONBLOCKING SP_GET_BYTE_HELPER_OPTION_NONBLOCKING
#define SP_GET_BYTE_OPTION_NO_RESTART SP_GET_BYTE_HELPER_OPTION_NO_RESTART

#define SP_has_buffered_bytes_(STREAM) \
  (SP_CAST_STREAM_((STREAM))->input_binary.ptr < SP_CAST_STREAM_((STREAM))->input_binary.limit)

#define SP_get_buffered_byte_(STREAM) \
  ((spio_t_error_code)(*(SP_CAST_STREAM_((STREAM))->input_binary.ptr++)))

#define SP_peek_buffered_byte_(STREAM) \
  ((spio_t_error_code)(*(SP_CAST_STREAM_((STREAM))->input_binary.ptr)))

#define SP_get_byte1(STREAM, OPTIONS)                                   \
  (SP_has_buffered_bytes_((STREAM)) ?                                   \
   SP_get_buffered_byte_((STREAM))                                      \
   :                                                                    \
   sp_get_byte_helper((STREAM), (OPTIONS))                              \
   )
#define SP_get_byte(STREAM) SP_get_byte1((STREAM), SPIO_OPTION_NONE)

#define SP_GET_CODE_HELPER_OPTION_NONBLOCKING      0x0001
#define SP_GET_CODE_HELPER_OPTION_PEEK             0x0002
#define SP_GET_CODE_HELPER_OPTION_NO_RESTART       0x0004

#define SP_GET_CODE_OPTION_NONBLOCKING SP_GET_CODE_HELPER_OPTION_NONBLOCKING
#define SP_GET_CODE_OPTION_NO_RESTART SP_GET_CODE_HELPER_OPTION_NO_RESTART

#define SP_has_buffered_codes_(STREAM) \
  (SP_CAST_STREAM_((STREAM))->input_text.ptr < SP_CAST_STREAM_((STREAM))->input_text.limit)

#define SP_get_buffered_code_(STREAM) \
  ((spio_t_error_code)(*(SP_CAST_STREAM_((STREAM))->input_text.ptr++)))

#define SP_peek_buffered_code_(STREAM) \
  ((spio_t_error_code)(*(SP_CAST_STREAM_((STREAM))->input_text.ptr)))

#define SP_get_code1(STREAM, OPTIONS)                                   \
  (SP_has_buffered_codes_((STREAM)) ?                                   \
   SP_get_buffered_code_((STREAM))                                      \
   :                                                                    \
   sp_get_code_helper((STREAM), (OPTIONS))                              \
   )
#define SP_get_code(STREAM) SP_get_code1((STREAM), SPIO_OPTION_NONE)

#define SP_has_bytes_buffer_(STREAM) \
  ((SP_CAST_STREAM_((STREAM))->output_binary.ptr < SP_CAST_STREAM_((STREAM))->output_binary.limit))

#define SP_has_codes_buffer_(STREAM) \
  ((SP_CAST_STREAM_((STREAM))->output_text.ptr < SP_CAST_STREAM_((STREAM))->output_text.limit))

#define SP_put_buffer_byte_(STREAM, BYTE) \
  ((spio_t_error_code)(*(SP_CAST_STREAM_((STREAM))->output_binary.ptr++) = ((BYTE))))

#define SP_put_buffer_code_(STREAM, CODE) \
  ((spio_t_error_code)(*(SP_CAST_STREAM_((STREAM))->output_text.ptr++) = ((CODE))))

#define SP_put_byte1(STREAM, BYTE, OPTIONS)                             \
  (SP_has_bytes_buffer_((STREAM)) ?                                     \
   SP_put_buffer_byte_((STREAM),(BYTE))                                 \
   :                                                                    \
   sp_put_byte_helper((STREAM), (BYTE), (OPTIONS))                      \
   )

#define SP_put_byte(STREAM, BYTE) SP_put_byte1((STREAM), (BYTE), SPIO_OPTION_NONE)

#define SP_put_code1(STREAM, CODE, OPTIONS)                             \
  (SP_has_codes_buffer_((STREAM)) ?                                     \
   SP_put_buffer_code_((STREAM),(CODE))                                 \
   :                                                                    \
   sp_put_code_helper((STREAM), (CODE), (OPTIONS))                      \
   )

#define SP_put_code(STREAM, CODE) SP_put_code1((STREAM), (CODE), SPIO_OPTION_NONE)

#define SP_CREATE_STREAM_OPTION_BINARY       0x0001
#define SP_CREATE_STREAM_OPTION_TEXT         0x0002
#define SP_CREATE_STREAM_OPTION_INTERACTIVE  0x0004
#define SP_CREATE_STREAM_OPTION_AUTOFLUSH    0x0008
#define SP_CREATE_STREAM_OPTION_EOF_ON_EOF   0x0010
#define SP_CREATE_STREAM_OPTION_RESET_ON_EOF 0x0020
      


#define SP_FOPEN_OPTION_READ                  0x000001
#define SP_FOPEN_OPTION_WRITE                 0x000002
#define SP_FOPEN_OPTION_APPEND                0x000004
#define SP_FOPEN_OPTION_BINARY                0x000008
#define SP_FOPEN_OPTION_TEXT                  0x000010
#define SP_FOPEN_OPTION_INTERACTIVE           0x000020
#define SP_FOPEN_OPTION_SEEK                  0x000040
#define SP_FOPEN_OPTION_AUTOFLUSH             0x000080
#define SP_FOPEN_OPTION_NOEXPAND              0x000100
#define SP_FOPEN_OPTION_NEW                   0x000200
#define SP_FOPEN_OPTION_URL                   0x000400
#define SP_FOPEN_OPTION_RESERVED1_            0x002000
#define SP_FOPEN_OPTION_RESERVED2_            0x004000
#define SP_FOPEN_OPTION_RESERVED3_            0x008000
#define SP_FOPEN_OPTION_RESERVED4_            0x010000
/* [PM] 4.3 do not do case-normaliation (e.g. downcase on Win32) of the file name before creating the file. */
#define SP_FOPEN_OPTION_NO_CASE_NORMALIZATION 0x020000
#define SP_FOPEN_OPTION_NO_FSYNC              0x040000

#define SP_FCLOSE_OPTION_FORCE        0x0001
#define SP_FCLOSE_OPTION_READ         0x0002
#define SP_FCLOSE_OPTION_WRITE        0x0004
#define SP_FCLOSE_OPTION_NONBLOCKING  0x0008
#define SP_FCLOSE_OPTION_USER_STREAMS 0x0010
#define SP_FCLOSE_OPTION_RESERVED1_   0x0020
#define SP_FCLOSE_OPTION_RESERVED2_   0x0040
#define SP_FCLOSE_OPTION_NO_FSYNC     0x0080

#define SP_FLUSH_OUTPUT_OPTION_NONBLOCKING 0x0001
/* only flush if stream has autoflush enabled (i.e., is interactive) */
#define SP_FLUSH_OUTPUT_OPTION_AUTOFLUSH   0x0002
#define SP_FLUSH_OUTPUT_OPTION_RESERVED1_  0x0004
#define SP_FLUSH_OUTPUT_OPTION_RESERVED2_  0x0008
#define SP_FLUSH_OUTPUT_OPTION_NO_FSYNC    0x0010

#define SP_PUT_ENCODED_STRING_OPTION_NONBLOCKING 0x0001
#define SP_PUT_ENCODED_STRING_OPTION_RESERVED1_  0x0002
#define SP_PUT_ENCODED_STRING_OPTION_RESERVED2_  0x0004

#define SP_PUT_BYTES_OPTION_NONBLOCKING 0x0001
#define SP_PUT_BYTES_OPTION_PRIVATE_1   0x1000
#define SP_PUT_BYTES_OPTION_PRIVATE_2   0x2000
#define SP_PUT_BYTES_OPTION_PRIVATE_3   0x4000


#define SP_PUT_CODES_OPTION_NONBLOCKING 0x0001
#define SP_PUT_CODES_OPTION_PRIVATE_1   0x1000
#define SP_PUT_CODES_OPTION_PRIVATE_2   0x2000
#define SP_PUT_CODES_OPTION_PRIVATE_3   0x4000


#define SP_CREATE_OS_STREAM_OPTION_READ                         0x00001
#define SP_CREATE_OS_STREAM_OPTION_WRITE                        0x00002
#define SP_CREATE_OS_STREAM_OPTION_BINARY                       0x00004
#define SP_CREATE_OS_STREAM_OPTION_TEXT                         0x00008
#define SP_CREATE_OS_STREAM_OPTION_OWN                          0x00010
#define SP_CREATE_OS_STREAM_OPTION_KEEP_FILE_HANDLE             0x00020
#define SP_CREATE_OS_STREAM_OPTION_OBEY_FILE_OFFSET             0x00040
#define SP_CREATE_OS_STREAM_OPTION_AUTOFLUSH                    0x00080     
/* xref SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_xxx */
#define SP_CREATE_OS_STREAM_OPTION_ENCODING_FALLBACK_ON_INVALID 0x00100
/* #define SP_CREATE_OS_STREAM_OPTION_ENCODING_FALLBACK_DEFAULT    0x00200 */
#define SP_CREATE_OS_STREAM_OPTION_ENCODING_FALLBACK_ERROR      0x00400
#define SP_CREATE_OS_STREAM_OPTION_ENCODING_FALLBACK_REPLACE    0x00800

#define SP_CREATE_OS_STREAM_OPTION_BOM_READ                     0x01000
#define SP_CREATE_OS_STREAM_OPTION_BOM_READ_OPTIONAL_           0x02000 /* [PM] 4.0.7 not used */
#define SP_CREATE_OS_STREAM_OPTION_BOM_WRITE                    0x04000
#define SP_CREATE_OS_STREAM_OPTION_BOM_WRITE_OPTIONAL           0x08000
#define SP_CREATE_OS_STREAM_OPTION_SEEKABLE                     0x10000 /* [PM] 4.0.6 a.k.a reposition(true) */
#define SP_CREATE_OS_STREAM_OPTION_OS_CHARSET                   0x20000
#define SP_CREATE_OS_STREAM_OPTION_ARGS_TERM                    0x40000
#define SP_CREATE_OS_STREAM_OPTION_ARGS_TERM_REF                0x60000
/* Character encodings */

typedef struct spio_t_encoding_ spio_t_encoding; /* opaque */
typedef struct spio_t_encoding_state_ spio_t_encoding_state; /* opaque */

#define SP_ENCODING_OPEN_OPTION_DECODE SPIO_CHAR_CODER_OPEN_OPTION_DECODE
#define SP_ENCODING_OPEN_OPTION_EOL_CRLF SPIO_CHAR_CODER_OPEN_OPTION_EOL_CRLF
#define SP_ENCODING_OPEN_OPTION_EOL_AUTO SPIO_CHAR_CODER_OPEN_OPTION_EOL_AUTO

#define SP_ENCODING_OPEN_OPTION_UTF8_TCL SPIO_CHAR_CODER_UTF8_OPEN_OPTION_TCL
#define SP_ENCODING_OPEN_OPTION_UTF8_JAVA SPIO_CHAR_CODER_UTF8_OPEN_OPTION_JAVA


#define SP_ENCODE_CODES_OPTION_START SPIO_BIT(0)
#define SP_ENCODE_CODES_OPTION_END   SPIO_NEXT_BIT(SP_ENCODE_CODES_OPTION_START)
#define SP_ENCODE_CODES_OPTION_ABORT SPIO_NEXT_BIT(SP_ENCODE_CODES_OPTION_END)

#define SP_ENCODE_CODES_OPTION_EOL_CRLF SPIO_NEXT_BIT(SP_ENCODE_CODES_OPTION_ABORT)
#define SP_ENCODE_CODES_OPTION_EOL_AUTO SPIO_NEXT_BIT(SP_ENCODE_CODES_OPTION_EOL_CRLF)
#define SP_ENCODE_CODES_OPTION_LAST_ SP_ENCODE_CODES_OPTION_EOL_AUTO

/*
  SP_ENCODE_CODES_OPTION_{RESET,SEEKABLE,TRANSACT,ROLLBACK,COMMIT,...}
*/

#define SP_INSTALL_IDLE_HOOK_OPTION_REMOVE SPIO_BIT(0)

/* async events */

/* opaque, allocated/freed by SP_alloc_async_event()/SP_free_async_event() */
typedef struct sp_t_async_event_ sp_t_async_event;

typedef spio_t_error_code SPCDECL sp_t_async_event_fun(sp_t_async_event *, spio_t_bits);


#endif  /* SICSTUS_SPIO_H_INCLUDED */
