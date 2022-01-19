#ifndef SPIO_PROCESS_H_INCLUDED
#define SPIO_PROCESS_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#define SPIO_PROCESS_EXIT_STATUS_NORMAL SPIO_BIT(0) /* CLD_EXITED */
#define SPIO_PROCESS_EXIT_STATUS_SIGNALED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_NORMAL) /* CLD_KILLED */
#define SPIO_PROCESS_EXIT_STATUS_COREDUMP SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_SIGNALED) /* CLD_DUMPED (SPIO_PROCESS_EXIT_STATUS_SIGNALED will be set too)  */
#define SPIO_PROCESS_EXIT_STATUS_STOPPED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_COREDUMP) /* CLD_STOPPED */
#define SPIO_PROCESS_EXIT_STATUS_CONTINUED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_STOPPED) /* CLD_CONTINUED */
#define SPIO_PROCESS_EXIT_STATUS_TRAPPED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_CONTINUED) /* CLD_TRAPPED */

struct spio_t_process_exit_status_ {
  spio_t_bits flags;
  spio_t_int32 value;           /* value, interpreted according to flags */
};
typedef struct spio_t_process_exit_status_ spio_t_process_exit_status;

#define SPIO_PROCESS_CREATE_OPTION_STDIN_STD SPIO_BIT(0)
#define SPIO_PROCESS_CREATE_OPTION_STDIN_NUL SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDIN_STD)
#define SPIO_PROCESS_CREATE_OPTION_STDIN_PIPE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDIN_NUL)

#define SPIO_PROCESS_CREATE_OPTION_STDOUT_STD SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDIN_PIPE)
#define SPIO_PROCESS_CREATE_OPTION_STDOUT_NUL SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDOUT_STD)
#define SPIO_PROCESS_CREATE_OPTION_STDOUT_PIPE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDOUT_NUL)

#define SPIO_PROCESS_CREATE_OPTION_STDERR_STD SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDOUT_PIPE)
#define SPIO_PROCESS_CREATE_OPTION_STDERR_NUL SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDERR_STD)
#define SPIO_PROCESS_CREATE_OPTION_STDERR_PIPE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDERR_NUL)

#define SPIO_PROCESS_CREATE_OPTION_NEED_HANDLE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDERR_PIPE)

#define SPIO_PROCESS_CREATE_OPTION_DETACHED SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_NEED_HANDLE)
#define SPIO_PROCESS_CREATE_OPTION_DAEMON SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_DETACHED)
#define SPIO_PROCESS_CREATE_OPTION_USE_PATH_ SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_DAEMON) /* not supported */
#define SPIO_PROCESS_CREATE_OPTION_NEW_WINDOW SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_USE_PATH_)
#define SPIO_PROCESS_CREATE_OPTION_COMMAND_LINE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_NEW_WINDOW)
#define SPIO_PROCESS_CREATE_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_COMMAND_LINE)
#define SPIO_PROCESS_CREATE_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_PRIVATE_1)
#define SPIO_PROCESS_CREATE_OPTION_EXPLICIT_ENV SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_PRIVATE_2)
#define SPIO_PROCESS_CREATE_OPTION_EXCLUDE_SPIO_ENV_ SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_EXPLICIT_ENV) /* unused */
#define SPIO_PROCESS_CREATE_OPTION_EXCLUDE_OS_ENV SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_EXCLUDE_SPIO_ENV_)



extern spio_t_error_code spio_process_create(char const *cmd_utf8,
                                             spio_t_property const **params_utf8,
                                             char const *cwd_utf8,
                                             spio_t_property const ** structured_env_utf8,
                                             spio_t_os_file_handle *pstdin_parent_wr,
                                             spio_t_os_file_handle *pstdout_parent_rd,
                                             spio_t_os_file_handle *pstderr_parent_rd,
                                             spio_t_os_process_handle *process_handle,
                                             spio_t_bits options);

extern spio_t_error_code spio_process_kill(spio_t_os_process_handle process, int signal_number, spio_t_bits options);

#define SPIO_PROCESS_WAIT_OPTION_NONBLOCKING SPIO_BIT(0)
#define SPIO_PROCESS_WAIT_OPTION_RELEASE SPIO_NEXT_BIT(SPIO_PROCESS_WAIT_OPTION_NONBLOCKING) /* do not leave process waitable */
#define SPIO_PROCESS_WAIT_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_PROCESS_WAIT_OPTION_RELEASE)
#define SPIO_PROCESS_WAIT_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_PROCESS_WAIT_OPTION_PRIVATE_1)
extern spio_t_error_code spio_process_wait(spio_t_os_process_handle process, spio_t_timespec *timeout, spio_t_process_exit_status *status, spio_t_bits options);

#define SPIO_PROCESS_RELEASE_OPTION_FORCE       SPIO_BIT(0)
#define SPIO_PROCESS_RELEASE_OPTION_HANDLE_ONLY SPIO_NEXT_BIT(SPIO_PROCESS_RELEASE_OPTION_FORCE)

/* Fails with SPIO_E_TRY_AGAIN if the process handle could not be
   released immediately (i.e. the (UNIX) process is still running)
   unless SPIO_PROCESS_RELEASE_OPTION_FORCE is set */
extern spio_t_error_code spio_process_release(spio_t_os_process_handle process, spio_t_bits options);

extern spio_t_error_code spio_process_id(spio_t_os_process_handle process, spio_t_pid *pid);

extern spio_t_error_code spio_process_self_id(spio_t_pid *pid);


extern spio_t_error_code spio_process_from_id(spio_t_pid pid, spio_t_os_process_handle *process);

extern spio_t_error_code spio_init_process(spio_t_bits options);


#define SPIO_GET_SYSTEM_PROPERTIES_OPTION_KEEP_DUPLICATES SPIO_BIT(0)
#define SPIO_GET_SYSTEM_PROPERTIES_OPTION_EXCLUDE_OS_ENV SPIO_NEXT_BIT(SPIO_GET_SYSTEM_PROPERTIES_OPTION_KEEP_DUPLICATES)
#define SPIO_GET_SYSTEM_PROPERTIES_OPTION_EXCLUDE_SPIO_ENV SPIO_NEXT_BIT(SPIO_GET_SYSTEM_PROPERTIES_OPTION_EXCLUDE_OS_ENV)
#define SPIO_GET_SYSTEM_PROPERTIES_OPTION_RESULT_EQUAL_SIGN_SEPARATED SPIO_NEXT_BIT(SPIO_GET_SYSTEM_PROPERTIES_OPTION_EXCLUDE_SPIO_ENV)
#define SPIO_GET_SYSTEM_PROPERTIES_OPTION_REFRESH SPIO_NEXT_BIT(SPIO_GET_SYSTEM_PROPERTIES_OPTION_RESULT_EQUAL_SIGN_SEPARATED) /* avoid this */
#if 0
/* Read current value from environ. The default is to use the cached
   initial value (for safety, POSIX environ is BBD). */
#define SPIO_GET_SYSTEM_PROPERTIES_OPTION_CURRENT ...
#endif  /* 0 */
extern spio_t_error_code spio_get_system_properties(char ***penv, size_t *psize, spio_t_bits options);

#define SPIO_OS_GETENV_OPTION_NEVER_CURRENT SPIO_BIT(0)
#define SPIO_OS_GETENV_OPTION_CURRENT SPIO_NEXT_BIT(SPIO_OS_GETENV_OPTION_NEVER_CURRENT)


#define SPIO_MAKE_COMPACT_ENV_OPTION_INPUT_EQUAL_SIGN_SEPARATED SPIO_BIT(0)
/* Make a compact NUL separated (unsorted) environment from an arbitrary environment. */
extern spio_t_error_code spio_make_compact_env(char const * const *non_compact_env, char ***penv, size_t *psize, spio_t_bits options);

extern int spio_compare_environment_keys(char const *key1, char const *key2);

#define SPIO_MERGE_COMPACT_ENVS_OPTION_KEEP_DUPLICATES SPIO_BIT(0)
#define SPIO_MERGE_COMPACT_ENVS_OPTION_SORT_BY_KEY SPIO_NEXT_BIT(SPIO_MERGE_COMPACT_ENVS_OPTION_KEEP_DUPLICATES)
#define SPIO_MERGE_COMPACT_ENVS_OPTION_SORT_BY_ADDRESS SPIO_NEXT_BIT(SPIO_MERGE_COMPACT_ENVS_OPTION_SORT_BY_KEY)

/* Merge NUL separated compact environments, optionally keeping duplicates. */
extern spio_t_error_code spio_merge_compact_envs(char **env1, size_t size1, char **env2, size_t size2, char ***penv, size_t *psize, spio_t_bits options);

#define SPIO_GET_SYSTEM_PROPERTY_OPTION_EXCLUDE_ENVIRONMENT SPIO_BIT(0)
#define SPIO_GET_SYSTEM_PROPERTY_OPTION_EXCLUDE_PROPERTIES SPIO_NEXT_BIT(SPIO_GET_SYSTEM_PROPERTY_OPTION_EXCLUDE_ENVIRONMENT)

extern spio_t_error_code spio_get_system_property(char const *key, char *buf, size_t *psize, spio_t_bits options);
extern spio_t_error_code spio_set_system_property(char const *key, char const *value, spio_t_bits options);
extern spio_t_error_code spio_set_initial_options(spio_t_init_options const *initial_options, spio_t_bits options);

/* Return the spio_alloc-ed value for key, or NULL if not found or error. */
extern char * spio_get_system_property_allocate(char const *key, spio_t_bits options);

extern int spio_get_boolean_system_property(char const *name, int default_value);
extern int spio_get_boolean_system_property1(char const *name, int default_value, spio_t_bits get_system_property_options);
extern spio_t_ssize spio_get_ssize_system_property(char const *name, spio_t_ssize default_value);


/* Return a pseudo-static reference to the environment instead of a copy */
#define SPIO_OS_GET_ENVIRON_OPTION_ALLOC_STATIC SPIO_BIT(0)
/* Return current OS environment. Best to avoid. */
#define SPIO_OS_GET_ENVIRON_OPTION_CURRENT SPIO_NEXT_BIT(SPIO_OS_GET_ENVIRON_OPTION_ALLOC_STATIC)
/* Refresh our copy from the process environ. Should go away. */
#define SPIO_OS_GET_ENVIRON_OPTION_REFRESH SPIO_NEXT_BIT(SPIO_OS_GET_ENVIRON_OPTION_CURRENT)
/* Separate key and value with '\0' instead of '=' (NUL-separation is the prefered way) */
#define SPIO_OS_GET_ENVIRON_OPTION_SEPARATOR_NUL SPIO_NEXT_BIT(SPIO_OS_GET_ENVIRON_OPTION_REFRESH)

/* Use '\0' instead of '=' between key and value. */
#define SPIO_OS_COPY_CURRENT_OS_ENVIRON_OPTION_NUL_SEPARATED SPIO_BIT(0)


extern char const * const * spio_get_initial_environ_NUL_separated_(void);
#endif  /* SPIO_PROCESS_H_INCLUDED */
