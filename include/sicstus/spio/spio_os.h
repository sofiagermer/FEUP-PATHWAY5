#ifndef SPIO_OS_H_INCLUDED
#define SPIO_OS_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#if SPIO_WIN32
#include "spio_win32.h"
#endif  /* SPIO_WIN32 */
#if SPIO_UNIX
#include "spio_unix.h"
#endif  /* SPIO_UNIX */


#define SPIO_OS_FILE_HANDLE_CLOSE_OPTION_ABORTIVE SPIO_BIT(0)
#define SPIO_OS_FILE_HANDLE_CLOSE_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_OS_FILE_HANDLE_CLOSE_OPTION_ABORTIVE)

/* Allowed (but not required) to return input argument as is (caller must check for this) */
#define SPIO_NAME_FROM_OS_OPTION_ALLOW_REUSE SPIO_BIT(0)
/* Whether encoding based on locale should be used instead of UTF-8. (not currently supported) */
#define SPIO_NAME_FROM_OS_OPTION_USE_LOCALE SPIO_NEXT_BIT(SPIO_NAME_FROM_OS_OPTION_ALLOW_REUSE)
/* If string is not already valid UTF-8 then treat it as Latin-1 */
#define SPIO_NAME_FROM_OS_OPTION_FALLBACK_TO_DEFAULT_ENCODING SPIO_NEXT_BIT(SPIO_NAME_FROM_OS_OPTION_USE_LOCALE)

extern spio_t_error_code spio_name_from_os(char const *senc_string, size_t senc_string_size, char **putf8_string, size_t *putf8_string_size, spio_t_bits options);

#if SPIO_WIN32
#define spio_os_file_handle_close spio_os_file_handle_close_win32
#define spio_os_default_charset spio_win32_default_charset
#endif  /* SPIO_WIN32 */

#if SPIO_UNIX
#define spio_os_file_handle_close spio_os_file_handle_close_unix
#define spio_os_default_charset spio_unix_default_charset
#endif  /* SPIO_UNIX */

#endif  /* SPIO_OS_H_INCLUDED */
