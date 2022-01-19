#ifndef SPIO_UNIX_H_INCLUDED
#define SPIO_UNIX_H_INCLUDED 1

#include "spio_types.h"
#include "spio_process.h"
#include "spio_file.h"

#include <sys/types.h>          /* mode_t */

typedef int spio_t_os_error;

extern spio_t_error_code spio_unix_stat(char const *path, spio_t_stat *info, spio_t_bits options);

extern spio_t_error_code spio_stat_unix(char const *path, spio_t_stat *info, spio_t_bits options);


/* Allowed (but not required) to return input argument as as (caller must check for this) */
#define SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_ALLOW_REUSE SPIO_BIT(0)
/* Whether file name encoding based on locale should be used instead of UTF-8. (not currently supported) */
#define SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_USE_LOCALE SPIO_NEXT_BIT(SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_ALLOW_REUSE)
/* If path is not already valid UTF-8 then treat it as Latin-1 */
#define SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_FALLBACK_TO_LATIN1 SPIO_NEXT_BIT(SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_USE_LOCALE)
#define SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_FILE_NAME_FROM_OS_UNIX_OPTION_FALLBACK_TO_LATIN1)
extern spio_t_error_code spio_file_name_for_os_unix(char const *utf8_name, char **pos_name, spio_t_bits options);

extern spio_t_error_code spio_file_name_from_os_unix(char const *os_name, char **putf8_name, spio_t_bits options);

extern spio_t_device_type spio_unix_device_type(spio_t_os_file_handle hFile);

extern spio_t_error_code spio_init_unix_standard_streams(spio_t_bits options); /* defined in spio_layer_async_unix.c */

/* same args as POSIX open(2) */
extern spio_t_error_code spio_unix_open_file(char const *utf8_pathname, int oflag, mode_t mode, spio_t_os_file_handle *ph);
extern spio_t_error_code spio_unix_file_size(spio_t_os_file_handle hFile, spio_t_offset *psize, spio_t_bits options);
extern spio_t_error_code spio_unix_file_pos(spio_t_os_file_handle hFile, spio_t_offset *ppos, spio_t_bits options);
#define SPIO_OS_FILE_HANDLE_CLOSE_UNIX_OPTION_RAW SPIO_OS_FILE_HANDLE_CLOSE_OPTION_PRIVATE_1
extern spio_t_error_code spio_os_file_handle_close_unix(spio_t_os_file_handle h, spio_t_bits options);

spio_t_error_code spio_unix_process_create(char const *cmd_utf8,
                                           spio_t_property const *params_utf8[],
                                           char const *cwd_utf8,
                                           spio_t_property const *structured_env_utf8[],
                                           spio_t_os_file_handle *pstdin_parent_fd_wr,
                                           spio_t_os_file_handle *pstdout_parent_fd_rd,
                                           spio_t_os_file_handle *pstderr_parent_fd_rd,
                                           spio_t_os_process_handle *processp,
                                           spio_t_bits options);

spio_t_error_code spio_unix_process_kill(spio_t_os_process_handle process, int signo, spio_t_bits options);

#define SPIO_UNIX_PROCESS_WAIT_OPTION_EINTR SPIO_PROCESS_WAIT_OPTION_PRIVATE_1
#define SPIO_UNIX_PROCESS_WAIT_OPTION_NO_ZOMBIES SPIO_PROCESS_WAIT_OPTION_PRIVATE_2
extern spio_t_error_code spio_unix_process_wait(spio_t_os_process_handle process, spio_t_timespec *timeout, spio_t_process_exit_status *exit_status, spio_t_bits options);

/* fails with SPIO_E_TRY_AGAIN if the process handle could not be released immediately (i.e. the process is still running) */
extern spio_t_error_code spio_unix_process_release(spio_t_os_process_handle process, spio_t_bits options);

extern spio_t_error_code spio_unix_process_id(spio_t_os_process_handle process, spio_t_pid *pid);
extern spio_t_error_code spio_unix_process_self_id(spio_t_pid *pid);
extern spio_t_error_code spio_unix_process_from_id(spio_t_pid pid, spio_t_os_process_handle *process);
extern spio_t_error_code spio_unix_create_directory(char const *utf8_dir, spio_t_bits options);
extern spio_t_error_code spio_unix_temp_directory(char **putf8_dir, spio_t_bits options);

extern spio_t_error_code spio_get_working_directory_unix(char **pdir, spio_t_bits options);
extern spio_t_error_code spio_set_working_directory_unix(char const *new_dir, spio_t_bits options);
extern spio_t_error_code spio_get_user_directory_unix(char const *name, char **pdir, spio_t_bits options);


extern spio_t_error_code spio_open_directory_unix(char const *path, spio_t_dir **pdir, spio_t_bits options);
extern void spio_close_directory_unix(spio_t_dir *dir);
extern spio_t_error_code spio_read_directory_unix(spio_t_dir *dir, spio_t_dirent **pent, spio_t_bits options);
extern spio_t_error_code spio_dirent_name_unix(spio_t_dirent *ent, char const **name);

extern spio_t_error_code spio_unix_delete_file(char const *utf8_file, spio_t_bits options);
extern spio_t_error_code spio_unix_rename_file(char const *utf8_oldfile, char const *utf8_newfile, spio_t_bits options);

extern spio_t_error_code spio_unix_test_file_access(char const *utf8_path, spio_t_bits options);
extern spio_t_error_code spio_unix_path_root_len(char const *utf8_path);



extern spio_t_error_code spio_user_id_name_unix(spio_t_uid uid, char **name, spio_t_bits options);
extern spio_t_error_code spio_group_id_name_unix(spio_t_gid gid, char **name, spio_t_bits options);

extern spio_t_error_code spio_init_process_unix(spio_t_bits options);

extern spio_t_error_code spio_unix_default_charset(char **pcharset, spio_t_bits options);

extern spio_t_error_code spio_unix_getenv(char const *key, void *buf, size_t *psize, spio_t_bits options);

extern int spio_unix_compare_environment_keys(char const *key1, char const *key2);
extern spio_t_error_code spio_unix_copy_current_os_environ(char ***penv, size_t *psize, spio_t_bits options);

typedef struct spio_t_macos_version_ {spio_t_int32 major; spio_t_int32 minor; spio_t_int32 revision;} spio_t_macos_version;

#if __APPLE__
/* The DARWIN(!) version if running on macOS. Fails on other OSes,
   including the various variants of iPhone OS. The version argument
   is ignored if NULL. */
extern spio_t_error_code spio_macos_version(spio_t_macos_version *version);
#endif	/* __APPLE__ */

#endif  /* SPIO_UNIX_H_INCLUDED */
