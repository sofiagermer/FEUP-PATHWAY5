#ifndef SPIO_FILE_H_INCLUDED
#define SPIO_FILE_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#if SPIO_UNIX
typedef uid_t spio_t_uid;       /* user id */
typedef gid_t spio_t_gid;       /* group id */
#endif  /* SPIO_UNIX */

#if SPIO_WIN32
typedef struct spio_t_sid_ const *spio_t_sid; /* really a SID* */
typedef spio_t_sid spio_t_uid;       /* user SID. Note that this must be deallocated */
typedef spio_t_sid spio_t_gid;       /* group SID Note that this must be deallocated */
#endif /* SPIO_WIN32 */

typedef struct spio_t_stat_ spio_t_stat;

typedef void spio_t_stat_deinit_fun(spio_t_stat *stat, void *cookie);

struct spio_t_stat_ {
#define SPIO_STAT_FLAG_NEED_DEINIT SPIO_BIT(0) /* see SPIO_STAT_OPTION_WILL_DEINIT */
#define SPIO_STAT_FLAG_RESERVED_1 SPIO_NEXT_BIT(SPIO_STAT_FLAG_NEED_DEINIT)
#define SPIO_STAT_FLAG_RESERVED_2 SPIO_NEXT_BIT(SPIO_STAT_FLAG_RESERVED_1)
#define SPIO_STAT_FLAG_RESERVED_3 SPIO_NEXT_BIT(SPIO_STAT_FLAG_RESERVED_2)
#define SPIO_STAT_FLAG_RESERVED_4 SPIO_NEXT_BIT(SPIO_STAT_FLAG_RESERVED_3)

#if SPIO_WIN32
#define SPIO_STAT_MODE_HAVE_OWNER_SID SPIO_STAT_FLAG_RESERVED_1
#define SPIO_STAT_MODE_HAVE_GROUP_SID SPIO_STAT_FLAG_RESERVED_2
#endif  /* SPIO_WIN32 */

  spio_t_bits flags;

#define SPIO_STAT_MODE_S_IRUSR SPIO_BIT(0)
#define SPIO_STAT_MODE_S_IWUSR SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IRUSR)
#define SPIO_STAT_MODE_S_IXUSR SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IWUSR)
#define SPIO_STAT_MODE_S_SEARCH_USR SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IXUSR)

#define SPIO_STAT_MODE_S_IRGRP SPIO_NEXT_BIT(SPIO_STAT_MODE_S_SEARCH_USR)
#define SPIO_STAT_MODE_S_IWGRP SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IRGRP)
#define SPIO_STAT_MODE_S_IXGRP SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IWGRP)
#define SPIO_STAT_MODE_S_SEARCH_GRP SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IXGRP)

#define SPIO_STAT_MODE_S_IROTH SPIO_NEXT_BIT(SPIO_STAT_MODE_S_SEARCH_GRP)
#define SPIO_STAT_MODE_S_IWOTH SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IROTH)
#define SPIO_STAT_MODE_S_IXOTH SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IWOTH)
#define SPIO_STAT_MODE_S_SEARCH_OTH SPIO_NEXT_BIT(SPIO_STAT_MODE_S_IXOTH)

#define SPIO_STAT_MODE_S_ISUID SPIO_NEXT_BIT(SPIO_STAT_MODE_S_SEARCH_OTH) /* POSIX-only */
#define SPIO_STAT_MODE_S_ISGID SPIO_NEXT_BIT(SPIO_STAT_MODE_S_ISUID) /* POSIX-only */
#define SPIO_STAT_MODE_S_ISVTX SPIO_NEXT_BIT(SPIO_STAT_MODE_S_ISGID) /* POSIX-only */

#define SPIO_STAT_MODE_CAN_READ SPIO_NEXT_BIT(SPIO_STAT_MODE_S_ISVTX) /* effective access  */
#define SPIO_STAT_MODE_CAN_WRITE SPIO_NEXT_BIT(SPIO_STAT_MODE_CAN_READ)
#define SPIO_STAT_MODE_CAN_EXECUTE SPIO_NEXT_BIT(SPIO_STAT_MODE_CAN_WRITE)
#define SPIO_STAT_MODE_CAN_SEARCH SPIO_NEXT_BIT(SPIO_STAT_MODE_CAN_EXECUTE)

  spio_t_bits mode;             /* if SPIO_STAT_OPTION_MODE set ( MODE_CAN_.. only if SPIO_STAT_MODE_CAN) */

#define SPIO_STAT_TYPE_DIR SPIO_BIT(0)
  spio_t_bits type;             /* if SPIO_STAT_OPTION_TYPE set */


  spio_t_time access_time;      /* if SPIO_STAT_OPTION_TIME set */
  spio_t_time modify_time;      /* if SPIO_STAT_OPTION_TIME set */
  spio_t_time create_time;      /* if SPIO_STAT_OPTION_TIME set */
  spio_t_uint64 size;           /* if SPIO_STAT_OPTION_SIZE set */
#if SPIO_UNIX
  spio_t_uid owner;             /* if SPIO_STAT_OPTION_OWNER set */
  spio_t_gid group;             /* if SPIO_STAT_OPTION_GROUP set */
#elif SPIO_WIN32
  /* These must be deinit by spio_stat_deinit so will only be set if
     SPIO_STAT_OPTION_WILL_DEINIT is passed in addition to the
     individual field options 

     They have different name from their UNIX counterpart to make it
     clear that they are special and harder to use.
  */
  spio_t_uid owner_sid;         /* if SPIO_STAT_OPTION_OWNER_SID set (and SPIO_STAT_OPTION_WILL_DEINIT) */
  spio_t_gid group_sid;         /* if SPIO_STAT_OPTION_GROUP_SID set (and SPIO_STAT_OPTION_WILL_DEINIT) */
#endif  /* SPIO_WIN32 */

  size_t deiniters_count;       /* number of entries in deiniters[] */
  struct {
    spio_t_stat_deinit_fun *fun;
    void *cookie;
  } *deiniters; /* list of deinit functions for spio_stat_deinit to call if SPIO_STAT_OPTION_WILL_DEINIT was used */
};

/*
  You must call this to reclaim resource for a spio_t_stat structure
  that was created with SPIO_STAT_OPTION_WILL_DEINIT.
  
  It does not hurt to call this even when SPIO_STAT_OPTION_WILL_DEINIT
  was not specified.
*/
extern void spio_stat_deinit(spio_t_stat *pstat);

/* for implementors of spio_stat_os */
extern spio_t_error_code spio_stat_add_deiniter(spio_t_stat *pstat, spio_t_stat_deinit_fun *fun, void *cookie);

#define SPIO_STAT_OPTION_WILL_DEINIT SPIO_BIT(0) /* caller promises to call spio_stat_deinit to free the spio_t_stat */
#define SPIO_STAT_OPTION_DIR SPIO_NEXT_BIT(SPIO_STAT_OPTION_WILL_DEINIT) /* require directory */
#define SPIO_STAT_OPTION_FILE SPIO_NEXT_BIT(SPIO_STAT_OPTION_DIR) /* require non-directory (FIXME: should we require REGULAR file?) */
#define SPIO_STAT_OPTION_MODE_S SPIO_NEXT_BIT(SPIO_STAT_OPTION_FILE) /* set MODE_S bits in mode field */
#define SPIO_STAT_OPTION_MODE_CAN SPIO_BIT(SPIO_STAT_OPTION_MODE_S) /* set MODE_CAN bits in mode field */
#define SPIO_STAT_OPTION_TYPE SPIO_NEXT_BIT(SPIO_STAT_OPTION_MODE_CAN) /* set type field */
#define SPIO_STAT_OPTION_TIME SPIO_NEXT_BIT(SPIO_STAT_OPTION_TYPE) /* set ..._time fields */
#define SPIO_STAT_OPTION_SIZE SPIO_NEXT_BIT(SPIO_STAT_OPTION_TIME) /* set size field */
#define SPIO_STAT_OPTION_RESERVED_1 SPIO_NEXT_BIT(SPIO_STAT_OPTION_SIZE)
#define SPIO_STAT_OPTION_RESERVED_2 SPIO_NEXT_BIT(SPIO_STAT_OPTION_RESERVED_1)
#define SPIO_STAT_OPTION_RESERVED_3 SPIO_NEXT_BIT(SPIO_STAT_OPTION_RESERVED_2)
#if SPIO_UNIX
#define SPIO_STAT_OPTION_OWNER SPIO_STAT_OPTION_RESERVED_1 /* set owner field */
#define SPIO_STAT_OPTION_GROUP SPIO_STAT_OPTION_RESERVED_2 /* set group field */
#elif SPIO_WIN32
/* Note: these options require that SPIO_STAT_OPTION_WILL_DEINIT is
   also set and thus that the caller ensures that spio_stat_deinit
   will be called on the spio_t_stat struct
*/
#define SPIO_STAT_OPTION_OWNER_SID SPIO_STAT_OPTION_RESERVED_1 /* set owner SID field */
#define SPIO_STAT_OPTION_GROUP_SID SPIO_STAT_OPTION_RESERVED_2 /* set group SID field */
#endif  /* SPIO_WIN32 */

extern spio_t_error_code spio_stat(char const *path, spio_t_stat *pstat, spio_t_bits options);

extern spio_t_error_code spio_create_directory(char const *utf8_dir, spio_t_bits options);

#define SPIO_TEMP_DIRECTORY_OPTION_WIN32_NORMALCASE SPIO_BIT(0)
extern spio_t_error_code spio_temp_directory(char **putf8_dir, spio_t_bits options);

#define SPIO_DELETE_FILE_OPTION_DIRECTORY SPIO_BIT(0)
#define SPIO_DELETE_FILE_OPTION_FORCE   SPIO_NEXT_BIT(SPIO_DELETE_FILE_OPTION_DIRECTORY) /* ignore errors (only matters if recurse) */
/* NOTE: We must not recurse through reparse points/symlinks. */
#define SPIO_DELETE_FILE_OPTION_RECURSE SPIO_NEXT_BIT(SPIO_DELETE_FILE_OPTION_FORCE) /* recursively delete dir */
extern spio_t_error_code spio_delete_file(char const *utf8_file, spio_t_bits options);

extern spio_t_error_code spio_rename_file(char const *utf8_oldfile, char const *utf8_newfile, spio_t_bits options);

/* #define SPIO_TEST_FILE_ACCESS_OPTION_X_OK */
#define SPIO_TEST_FILE_ACCESS_OPTION_X_OK SPIO_BIT(0)
#define SPIO_TEST_FILE_ACCESS_OPTION_W_OK SPIO_NEXT_BIT(SPIO_TEST_FILE_ACCESS_OPTION_X_OK)
#define SPIO_TEST_FILE_ACCESS_OPTION_R_OK SPIO_NEXT_BIT(SPIO_TEST_FILE_ACCESS_OPTION_W_OK)
#define SPIO_TEST_FILE_ACCESS_OPTION_CREATE_OK SPIO_NEXT_BIT(SPIO_TEST_FILE_ACCESS_OPTION_R_OK)
                                 
#define SPIO_TEST_FILE_ACCESS_OPTION_SEARCH_OK SPIO_NEXT_BIT(SPIO_TEST_FILE_ACCESS_OPTION_CREATE_OK)
#define SPIO_TEST_FILE_ACCESS_OPTION_DIR SPIO_NEXT_BIT(SPIO_TEST_FILE_ACCESS_OPTION_SEARCH_OK)
#define SPIO_TEST_FILE_ACCESS_OPTION_REG SPIO_NEXT_BIT(SPIO_TEST_FILE_ACCESS_OPTION_DIR)
extern spio_t_error_code spio_test_file_access(char const *utf8_path, spio_t_bits options);

extern spio_t_error_code spio_path_root_len(char const *path);

/* Return cached value, i.e. cwd at startup or at last SPIO_SET_WORKING_DIRECTORY_OPTION_CACHED. */
#define SPIO_GET_WORKING_DIRECTORY_OPTION_CACHED SPIO_BIT(0)
/* Ask OS for current (process global) working directory. */
#define SPIO_GET_WORKING_DIRECTORY_OPTION_CURRENT SPIO_NEXT_BIT(SPIO_GET_WORKING_DIRECTORY_OPTION_CACHED)
/* If getcwd() et al do not return a valid UTF-8 then treat it as
   Latin-1 and return SPIO_S_OPERATION_FAILED. */
#define SPIO_GET_WORKING_DIRECTORY_OPTION_FALLBACK_TO_LATIN1 SPIO_NEXT_BIT(SPIO_GET_WORKING_DIRECTORY_OPTION_CURRENT)
#define SPIO_GET_WORKING_DIRECTORY_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_GET_WORKING_DIRECTORY_OPTION_FALLBACK_TO_LATIN1)
extern spio_t_error_code spio_get_working_directory(char **pdir, spio_t_bits options);

/* Set the cached value. This is what SPIO use for resolving relative paths etc. */
#define SPIO_SET_WORKING_DIRECTORY_OPTION_CACHED SPIO_BIT(0)
/* Set the OS (process global) working directory. _NOT_ a good idea,
   process global working directory is broken by design. */
#define SPIO_SET_WORKING_DIRECTORY_OPTION_CURRENT SPIO_NEXT_BIT(SPIO_SET_WORKING_DIRECTORY_OPTION_CACHED)
extern spio_t_error_code spio_set_working_directory(char const *new_dir, spio_t_bits options);


#define SPIO_OPEN_DIRECTORY_OPTION_DIR SPIO_BIT(0) /* list only dirs */
#define SPIO_OPEN_DIRECTORY_OPTION_FILE SPIO_NEXT_BIT(SPIO_OPEN_DIRECTORY_OPTION_DIR) /* list only files */
#define SPIO_OPEN_DIRECTORY_OPTION_WIN32_NORMALCASE SPIO_NEXT_BIT(SPIO_OPEN_DIRECTORY_OPTION_FILE) /* normalize file names on Win32 */
extern spio_t_error_code spio_open_directory(char const *path, spio_t_dir **pdir, spio_t_bits options);

extern void spio_close_directory(spio_t_dir *dir);
extern spio_t_error_code spio_read_directory(spio_t_dir *dir, spio_t_dirent **pent, spio_t_bits options);
extern spio_t_error_code spio_dirent_name(spio_t_dirent *ent, char const **name);

#define SPIO_USER_ID_NAME_OPTION_PRIVATE_1 SPIO_BIT(0)
#define SPIO_USER_ID_NAME_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_USER_ID_NAME_OPTION_PRIVATE_1)
extern spio_t_error_code spio_user_id_name(spio_t_uid uid, char **name, spio_t_bits options);

#define SPIO_GROUP_ID_NAME_OPTION_PRIVATE_1 SPIO_BIT(0)
#define SPIO_GROUP_ID_NAME_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_GROUP_ID_NAME_OPTION_PRIVATE_1)
extern spio_t_error_code spio_group_id_name(spio_t_gid gid, char **name, spio_t_bits options);

extern spio_t_error_code spio_init_file(spio_t_bits options);
#endif  /* SPIO_FILE_H_INCLUDED */
