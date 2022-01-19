#ifndef SPIO_WIN32_H_INCLUDED
#define SPIO_WIN32_H_INCLUDED 1

#include "spio_types.h"
#include "spio_process.h"
#include "spio_file.h"

#ifndef STRICT
#define STRICT 1
#endif
/* [PM] 4.0 Always include winsock2 instead of windows to prevent winsock.h from making subsequent inclusion of winsock2.h impossible */
#include <winsock2.h>

typedef DWORD spio_t_os_error;

extern spio_t_error_code spio_stat_win32(char const *path, spio_t_stat *info, spio_t_bits options);

extern spio_t_error_code spio_file_name_for_os_win32W(char const *utf8_name, LPWSTR *lppWideCharStr, spio_t_bits options);
extern spio_t_device_type spio_win32_device_type(spio_t_os_file_handle hFile);
extern spio_t_error_code spio_win32_file_size(spio_t_os_file_handle hFile, spio_t_offset *psize, spio_t_bits options);
extern spio_t_error_code spio_win32_file_pos(spio_t_os_file_handle hFile, spio_t_offset *ppos, spio_t_bits options);
extern spio_t_error_code spio_timespec_to_win32_timeout_ms(spio_t_timespec *timespec, DWORD *pdwMilliseconds);

extern spio_t_error_code spio_init_win32_standard_streams(spio_t_bits options); /* defined in spio_layer_async_win32.c */

spio_t_error_code spio_get_standard_handle_duplicate(DWORD nStdHandle, HANDLE *pHandle);

extern spio_t_error_code spio_win32_process_create(char const *cmd_utf8,
                                                   spio_t_property const *params_utf8[],
                                                   char const *cwd_utf8,
                                                   spio_t_property const *structured_env_utf8[],
                                                   spio_t_os_file_handle *phStdInWr,
                                                   spio_t_os_file_handle *phStdOutRd,
                                                   spio_t_os_file_handle *phStdErrRd,
                                                   spio_t_os_process_handle *phProcess,
                                                   spio_t_bits options);


#define SPIO_WIN32_PROCESS_KILL_OPTION_SIGKILL SPIO_BIT(0)
spio_t_error_code spio_win32_process_kill(spio_t_os_process_handle process, int signo, spio_t_bits options);

extern spio_t_error_code spio_win32_process_wait(spio_t_os_process_handle process, spio_t_timespec *timeout, spio_t_process_exit_status *exit_status, spio_t_bits options);

extern spio_t_error_code spio_win32_process_release(spio_t_os_process_handle process, spio_t_bits options);

extern spio_t_error_code spio_win32_process_id(spio_t_os_process_handle process, spio_t_pid *pid);
extern spio_t_error_code spio_win32_process_self_id(spio_t_pid *pid);
extern spio_t_error_code spio_win32_process_from_id(spio_t_pid pid, spio_t_os_process_handle *process);

extern spio_t_error_code spio_win32_thread_id(HANDLE hThread, spio_t_thread_id *p_tid);

extern spio_t_error_code spio_os_file_handle_close_win32(spio_t_os_file_handle hFile, spio_t_bits options);
extern spio_t_error_code spio_win32_create_directory(char const *utf8_dir, spio_t_bits options);
extern spio_t_error_code spio_win32_temp_directory(char **putf8_dir, spio_t_bits options);

extern spio_t_error_code spio_open_directory_win32(char const *path, spio_t_dir **pdir, spio_t_bits options);
extern void spio_close_directory_win32(spio_t_dir *dir);
extern spio_t_error_code spio_read_directory_win32(spio_t_dir *dir, spio_t_dirent **pent, spio_t_bits options);
extern spio_t_error_code spio_dirent_name_win32(spio_t_dirent *ent, char const **name);

extern spio_t_error_code spio_win32_delete_file(char const *utf8_file, spio_t_bits options);
extern spio_t_error_code spio_win32_rename_file(char const *utf8_oldfile, char const *utf8_newfile, spio_t_bits options);

extern spio_t_error_code spio_win32_test_file_access(char const *utf8_path, spio_t_bits options);
extern spio_t_error_code spio_path_root_len_win32(char const *utf8_path);

#define SPIO_GET_WORKING_DIRECTORY_OPTION_WIN32_NORMALCASE SPIO_GET_WORKING_DIRECTORY_OPTION_PRIVATE_1
extern spio_t_error_code spio_get_working_directory_win32(char **pdir, spio_t_bits options);

extern spio_t_error_code spio_set_working_directory_win32(char const *new_dir, spio_t_bits options);


#define SPIO_SID_NAME_OPTION_SID_STRING SPIO_BIT(0)
#define SPIO_SID_NAME_OPTION_SID_STRING_FALLBACK SPIO_NEXT_BIT(SPIO_SID_NAME_OPTION_SID_STRING) /* fall back to S- string SID */

#define SPIO_USER_ID_NAME_WIN32_OPTION_SID_STRING SPIO_USER_ID_NAME_OPTION_PRIVATE_1 /* get S-... string SID */
extern spio_t_error_code spio_user_id_name_win32(spio_t_uid uid, char **name, spio_t_bits options);

#define SPIO_GROUP_ID_NAME_WIN32_OPTION_SID_STRING SPIO_GROUP_ID_NAME_OPTION_PRIVATE_1 /* get S-... string SID */
extern spio_t_error_code spio_group_id_name_win32(spio_t_gid gid, char **name, spio_t_bits options);

#ifdef _UNICODE
#define spio_lookup_dll_function spio_lookup_dll_functionW
#else   /* !_UNICODE */
#define spio_lookup_dll_function spio_lookup_dll_functionA
#endif  /* !_UNICODE */

extern spio_t_error_code spio_lookup_dll_function(LPCSTR lpProcName, LPCTSTR lpModuleName,
                                                  FARPROC *ppProc, HMODULE *phModule);

extern spio_t_error_code spio_get_function_module(void *pfn, HMODULE *phModule, spio_t_bits options);

/* [0..191] only changes case at A-Z (192 is LATIN CAPITAL LETTER A WITH GRAVE) */
#define SPIO_DOWNCASE_FILENAME_WIN32_FAST_UPPER_LIMIT_INCLUSIVE ((wchar_t)191)
extern spio_t_error_code spio_normalcase_filename_win32(char const *name, char **pdowncased);

extern spio_t_error_code spio_win32_init_process(spio_t_bits options);
extern spio_t_error_code spio_win32_getenv(char const *key, void *buf, size_t *psize, spio_t_bits options);
extern spio_t_error_code spio_win32_default_charset(char **pcharset, spio_t_bits options);
extern int spio_win32_compare_environment_keys(char const *key1, char const *key2);
extern spio_t_error_code spio_win32_copy_current_os_environ(char ***envp, size_t *psize, spio_t_bits options);

#endif  /* SPIO_WIN32_H_INCLUDED */
