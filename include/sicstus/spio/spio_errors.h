#ifndef SPIO_ERRORS_H_INCLUDED
#define SPIO_ERRORS_H_INCLUDED

enum spio_t_error_code_ {

#define SPIO_FAILED(CODE) SPIO_UNLIKELY((CODE) < 0) /* tell GCC that failures codes are unlikely */
#define SPIO_SUCCEEDED(CODE) (!SPIO_FAILED((CODE)))

  /* free for user code to use. Should never be returned from public functions or methods. (Hmm?) */
  SPIO_S_PRIVATE_ERROR_0   = 30000,
  SPIO_S_PRIVATE_ERROR_1   = SPIO_S_PRIVATE_ERROR_0+1,
  SPIO_S_PRIVATE_ERROR_2   = SPIO_S_PRIVATE_ERROR_1+1,
  SPIO_S_PRIVATE_ERROR_3   = SPIO_S_PRIVATE_ERROR_2+1,
  SPIO_S_PRIVATE_ERROR_4   = SPIO_S_PRIVATE_ERROR_3+1,
  SPIO_S_PRIVATE_ERROR_5   = SPIO_S_PRIVATE_ERROR_4+1,
  SPIO_S_PRIVATE_ERROR_6   = SPIO_S_PRIVATE_ERROR_5+1,

  SPIO_S_NOERR             =   0,
  SPIO_E_NOERR             =   0, /* (DUPLICATE) FIXME: Obsolete */


  SPIO_S_TRUE              =     1, /* boolean true as an error code (DUPLICATE) */
  SPIO_S_FALSE             =     0, /* (DUPLICATE) */

  SPIO_S_OPERATION_FAILED  =     1, /* spio_aio_return succeeded but the I/O operation reported an error */
  SPIO_S_DEALLOCATED       =     2, /* the object was closed/freed/deallocated (SP_fclose uses this) */
  SPIO_S_NOT_SUPPORTED     =     3, /* the operation was ignored but this is not fatal (used by spio_ioctl_encoding_fallback) */
  SPIO_S_DONE              =     4, /* the operation is done and need not be called again. */
  SPIO_S_SUPPRESSED_INTERRUPT   =  5, /* A SPIO_E_INTERRUPTED happened but could not be handled. Avoid needing this, at all costs. */
  SPIO_S_NORMALIZED        = 6, /* [PM] 4.3 spio_open_directory() entries will have normalized names */

  SPIO_E_ERROR             = -10001, /* generic error */
  SPIO_E_END_OF_FILE       = -1, /* special value for backward compatibility */
  SPIO_E_OUT_OF_MEMORY     = -10003,
  SPIO_E_INSUFFICIENT_BUFFER = SPIO_E_OUT_OF_MEMORY, /* (DUPLICATE) */
  SPIO_E_PARAMETER_ERROR   = -10004,
  SPIO_E_VERSION_ERROR     = -10005,
  SPIO_E_NOT_IMPLEMENTED   = -10006,
  SPIO_E_NOT_SUPPORTED     = -10007,
  SPIO_E_NOT_FOUND         = -10008, /* hash table etc: key/object not present/found */
  SPIO_E_FOUND             = -10009, /* hash table etc: key already present */
  SPIO_E_NOT_READ          = -10010,
  SPIO_E_NOT_WRITE         = -10011,

  SPIO_E_NOT_OPEN_READ     = -10012,
  SPIO_E_NOT_OPEN_WRITE    = -10013,
  
  SPIO_E_INTERRUPTED       = -10014, /* operation interrupted (cf EINTR) */

  SPIO_E_IMPOSSIBLE_ERROR  = -10015, /* OS should have ensured that this could not happen */
  SPIO_E_INTERNAL_ERROR    = -10016, /* SPIO should have ensured that this could not happen */
  SPIO_E_OS_ERROR          = -10017, /* Error from some OS/system -call */
  SPIO_E_TIMEOUT           = -10018, /* Operation timed out (e.g., Event wait) */
  SPIO_E_WOULD_BLOCK       = -10019, /* operation would block (for non-blocking read/write/flush) */
  SPIO_E_TRY_AGAIN         = SPIO_E_WOULD_BLOCK, /* (DUPLICATE) */

  SPIO_E_PERMISSION_ERROR  = -10020, /* EPERM */
  SPIO_E_NO_MORE_ENTRIES   = -10021, /* spio_read_directory() after last entry */

  SPIO_E_IO_ERROR = -10022,     /* [PM] 4.0.2+ for mapping POSIX EIO */
  
  SPIO_E_INVALID_STATE = -10023, /* [PM] 4.0.5 spider Invalid state for this call */

  SPIO_E_SHUTDOWN = -10024, /* [PM] 4.1.3 SPIO is shutting down and the call cannot be processed. */

  SPIO_E_LAYER_            = -10100, /* starting point for layer errors  */

  SPIO_E_OPEN_ERROR        =  SPIO_E_LAYER_-1, /* generic error when opening */
  SPIO_E_READ_ERROR        =  SPIO_E_OPEN_ERROR-1, /* generic error when reading */
  SPIO_E_WRITE_ERROR       =  SPIO_E_READ_ERROR-1, /* generic error when writing */
  SPIO_E_SEEK_ERROR        =  SPIO_E_WRITE_ERROR-1, /* generic error when seeking (or requesting seekability in open/push) */
  SPIO_E_CLOSE_ERROR       =  SPIO_E_SEEK_ERROR-1, /* generic error when closing */
  SPIO_E_SYNC_ERROR        =  SPIO_E_CLOSE_ERROR-1, /* generic error when syncing */
  SPIO_E_PUSH_ERROR        =  SPIO_E_SYNC_ERROR-1, /* generic error when pushing */
  SPIO_E_TOO_MANY_OPEN_FILES = SPIO_E_PUSH_ERROR-1, /* too many open files (EMFILE) */
  SPIO_E_OVERFLOW          = SPIO_E_TOO_MANY_OPEN_FILES-1, /* (arithmetic) overflow, e.g. in offset calculation */

  /* character encodings */
  SPIO_E_CHAR_CODER_       = -10200, /* starting point for char coder errors */
  SPIO_E_GRANULARITY_ERROR = SPIO_E_CHAR_CODER_-1,
  SPIO_E_NOT_ENOUGH_DATA   = SPIO_E_GRANULARITY_ERROR-1,


  SPIO_E_SYNTAX_ERROR      = SPIO_E_NOT_ENOUGH_DATA-1,
  SPIO_E_ENCODING_ERROR_    = SPIO_E_SYNTAX_ERROR, /* (DUPLICATE) */

  SPIO_E_ENCODING_UNMAPPABLE = SPIO_E_ENCODING_ERROR_-1,
  SPIO_E_ENCODING_UNASSIGNED = SPIO_E_ENCODING_UNMAPPABLE, /* (DUPLICATE) */
  SPIO_E_ENCODING_INVALID    = SPIO_E_ENCODING_UNASSIGNED-1,

  SPIO_E_CHARSET_NOT_FOUND = SPIO_E_ENCODING_INVALID-1,


  /* threads */
  SPIO_E_THREAD_           = -10300, /* starting point for thread errors */
  SPIO_E_THREAD_ERROR      = SPIO_E_THREAD_-1,
  SPIO_E_THREAD_RUNNING    = SPIO_E_THREAD_ERROR-1, /* thread is still running */

  /* AIO */
  SPIO_E_AIO_              = -10400, /* starting point for AIO errors */
  SPIO_E_AIO_ERROR         = SPIO_E_AIO_-1, /* generic AIO error (spio_aio) */
  SPIO_E_ASYNC_ERROR = SPIO_E_AIO_ERROR, /* (spio_async) (DUPLICATE) */
  SPIO_E_AIO_INPROGRESS    = SPIO_E_AIO_ERROR-1, /* (spio_aio) */
  SPIO_E_ASYNC_INPROGRESS = SPIO_E_AIO_INPROGRESS, /* (spio_async) (DUPLICATE) */
  SPIO_E_INPROGRESS = SPIO_E_ASYNC_INPROGRESS, /* (sockets) (DUPLICATE) */
  SPIO_E_CANCELED   = SPIO_E_AIO_INPROGRESS-1, /*  */

  SPIO_E_AIO_CANCELED      = SPIO_E_CANCELED, /* (DUPLICATE) */
  SPIO_E_AIO_EROFS         = SPIO_E_AIO_CANCELED-1, /* (aio_fsync errno=EROFS, read-only file system ) */
  SPIO_E_AIO_EINVAL        = SPIO_E_AIO_EROFS-1, /* (aio_fsync errno=EINVAL, file does not support fsync) */

  /* file access */
  SPIO_E_FILE_              = -10500, /* starting point for file errors */
  SPIO_E_FILE_NOT_FOUND     = -10501,
  SPIO_E_FILE_ACCESS        = -10502,
  SPIO_E_FILE_IS_DIRECTORY  = -10503, /* file is a directory */
  SPIO_E_FILE_NOT_DIRECTORY = -10504, /* file is not a directory */
  SPIO_E_FILE_NOT_ABSOLUTE  = -10505, /* path is not absolute */
  SPIO_E_FILE_EXISTS        = -10506, /* file already exists (xref SPIO_OPEN_OPTION_NEW) or directory not empty */
  SPIO_E_DIR_NOT_EMPTY      = -10507, /* directory is not empty (rmdir) xref SPIO_E_FILE_EXISTS spio_delete_file always returns this for non-empty dir */
  SPIO_E_INVALID_NAME       = -10508, /* invalid (file/url/...) name */
  SPIO_E_NOSPACE            = -10509, /* ENOSPC */

  SPIO_E_NET_           = -10600, /* starting point for network errors (DUPLICATE) */
  SPIO_E_NET_ERROR      = SPIO_E_NET_, /* generic network related error */
  SPIO_E_NET_CONNREFUSED = SPIO_E_NET_-1,
  SPIO_E_NET_ADDRINUSE = SPIO_E_NET_CONNREFUSED-1,
  SPIO_E_NET_CONNRESET = SPIO_E_NET_ADDRINUSE-1,
  SPIO_E_NET_TIMEDOUT = SPIO_E_NET_CONNRESET-1, /* ETIMEDOUT, WSATIMEDOUT */
  SPIO_E_NET_HOST_NOT_FOUND = SPIO_E_NET_TIMEDOUT-1, /* WSAHOST_NOT_FOUND, EAI_NODATA, EAI_AGAIN, EAI_NONAME */
  SPIO_E_NET_ACCESS = SPIO_E_FILE_ACCESS,  /* (DUPLICATE) */
  
  /* Used for package specific errors. */
  SPIO_E_MISC_ERROR_0   = -20000,
  SPIO_E_MISC_ERROR_1   = SPIO_E_MISC_ERROR_0-1,
  SPIO_E_MISC_ERROR_2   = SPIO_E_MISC_ERROR_1-1,
  SPIO_E_MISC_ERROR_3   = SPIO_E_MISC_ERROR_2-1,
  SPIO_E_MISC_ERROR_4   = SPIO_E_MISC_ERROR_3-1,
  SPIO_E_MISC_ERROR_5   = SPIO_E_MISC_ERROR_4-1,
  SPIO_E_MISC_ERROR_6   = SPIO_E_MISC_ERROR_5-1,


  /* free for user code to use. Should never be returned from public functions or methods. (Hmm?) */
  SPIO_E_PRIVATE_ERROR_0   = -30000,
  SPIO_E_PRIVATE_ERROR_1   = SPIO_E_PRIVATE_ERROR_0-1,
  SPIO_E_PRIVATE_ERROR_2   = SPIO_E_PRIVATE_ERROR_1-1,
  SPIO_E_PRIVATE_ERROR_3   = SPIO_E_PRIVATE_ERROR_2-1,
  SPIO_E_PRIVATE_ERROR_4   = SPIO_E_PRIVATE_ERROR_3-1,
  SPIO_E_PRIVATE_ERROR_5   = SPIO_E_PRIVATE_ERROR_4-1,
  SPIO_E_PRIVATE_ERROR_6   = SPIO_E_PRIVATE_ERROR_5-1,

  SPIO_E_LAST_ERROR_CODE        /* not used */
};
typedef enum spio_t_error_code_ spio_t_error_code;

extern char const *spio_error_name(spio_t_error_code code);

#if SPIO_UNIX
extern spio_t_error_code spio_map_unix_error(int posix_errno, spio_t_error_code default_error);
#endif  /* SPIO_UNIX */

#if SPIO_WIN32
#if SPIO_INCLUDE_OS_TYPES
extern spio_t_error_code spio_map_win32_error(DWORD last_error, spio_t_error_code default_code);
#endif  /* SPIO_INCLUDE_OS_TYPES */
#endif  /* SPIO_WIN32 */

#endif  /* SPIO_ERRORS_H_INCLUDED */
