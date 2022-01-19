/* -*- Mode:C; coding:iso-8859-1; indent-tabs-mode:nil; tab-width:8; -*- */
/* Copyright (C) 2021, RISE Research Institutes of Sweden AB. */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sicstus/sicstus.h>
#include "lmdb.h"

#define TERMSNAME "terms"     /* the TERMREF -> TERM mapping */
#define INDEXNAME "index"     /* the hash code -> TERMREF mapping */
#define ADMINNAME "admin"     /* the next TERMREF & the speclist */
#define NREFKEY 0	      /* key for the next available TERMREF */
#define SPECKEY 1	      /* key for the speclist */

typedef struct db_struct_ db_struct;
typedef struct db_iterator_ iterator;
typedef struct db_cursor_ db_cursor;

struct db_struct_ {
  MDB_env *env;			/* env == 0 if outside any env */
  MDB_txn *txn;
  MDB_dbi terms_dbi, index_dbi, admin_dbi;
  iterator *next_iterator;
  db_cursor *next_cursor;
  char const *path;
  db_struct *next;
};

struct db_iterator_ {
  MDB_cursor *cursor;
  int nkeys;                    /* total number of keys */
  int ckey;                     /* current key */
  iterator *next_iterator;
  /* FIXME: Make key spio_t_uint32? */
  SP_integer key[1];            /* can be larger than 1! */
};

struct db_cursor_ {
  MDB_cursor *cursor;
  db_cursor *next_cursor;
};

#include "lmdb_glue.h"

#if MULTI_SP
#error "LMDB cannot be used more than once in the entire _process_, so MULTI_SP is tricky."
#endif  /* MULTI_SP */
/* Linked list of all open db_struct objects. Mainly for detecting
   attempts at opening a DB path when it is already open (which LMDB
   disallows but quitly does the wrong thing fore). NOTE: This
   requirement must hold for the entire process, which would make
   MULTI_SP tricky to implement. */
static db_struct *db_structs = NULL;

/* The address of a next-field pointing at a db_struct that has the
   path open, if any. Otherwise the address of a NULL next-field.  The
   returned value is never null.
*/
static db_struct ** find_db_for_path(char const *path)
{
  db_struct **p = &db_structs;
  while (*p != NULL) {
    if (strcmp(path, (*p)->path) == 0) {
      return p;
    }
    p = &(*p)->next;
  }
  return p;
}

/* The argument if it is a known open db, otherwise NULL.
   Returns NULL if the argument is NULL.
*/
static db_struct * find_open_db(db_struct *db)
{
  db_struct *p = db_structs;
  while (p != NULL) {
    if (p == db) return p;
    p = p->next;
  }
  return NULL;
}

static void link_db(db_struct *db)
{
  SP_ASSERT(find_open_db(db) == NULL);
  SP_ASSERT(*find_db_for_path(db->path) == NULL);
  db->next = db_structs;
  db_structs = db;
  SP_ASSERT(*find_db_for_path(db->path) == db);
  SP_ASSERT(find_open_db(db) == db);
}

static void unlink_db(db_struct *db)
{
  SP_ASSERT(find_open_db(db) == db);
  SP_ASSERT(*find_db_for_path(db->path) == db);
  *find_db_for_path(db->path) = db->next;
  db->next = NULL;
  SP_ASSERT(*find_db_for_path(db->path) == NULL);
  SP_ASSERT(find_open_db(db) == NULL);
}

/* The address of a next_iterator-field pointing at the iterator, if
   any. Otherwise the address of a NULL next_iterator-field.  The
   returned value is never null.
*/
static iterator ** find_iterator_link(db_struct *db, iterator *it)
{
  iterator **p = &db->next_iterator;
  while (*p != NULL) {
    if (it == (*p)) {
      return p;
    }
    p = &(*p)->next_iterator;
  }
  return p;
}

/* The argument if it is a known iterator for the db, otherwise NULL.
   Returns NULL if either argument is NULL.
*/
static iterator * find_iterator(db_struct *db, iterator *it)
{
  if (db != NULL) {
    iterator *p = db->next_iterator;
    while (p != NULL) {
      if (p == it) return p;
      p = p->next_iterator;
    }
  }
  return NULL;
}

static void link_iterator(db_struct *db, iterator *it)
{
  SP_ASSERT(*find_iterator_link(db, it) == NULL);
  SP_ASSERT(find_iterator(db, it) == NULL);
  it->next_iterator = db->next_iterator;
  db->next_iterator = it;
  SP_ASSERT(find_iterator(db, it) == it);
  SP_ASSERT(*find_iterator_link(db, it) == it);
}

static void unlink_iterator(db_struct *db, iterator *it)
{
  SP_ASSERT(find_iterator(db, it) == it);
  SP_ASSERT(*find_iterator_link(db, it) == it);
  *find_iterator_link(db, it) = it->next_iterator;
  it->next_iterator = NULL;
  SP_ASSERT(*find_iterator_link(db, it) == NULL);
  SP_ASSERT(find_iterator(db, it) == NULL);
}

/* The address of a next_cursor-field pointing at the cursor, if
   any. Otherwise the address of a NULL next_cursor-field.  The
   returned value is never null.
*/
static db_cursor ** find_cursor_link(db_struct *db, db_cursor *cu)
{
  db_cursor **p = &db->next_cursor;
  while (*p != NULL) {
    if (cu == (*p)) {
      return p;
    }
    p = &(*p)->next_cursor;
  }
  return p;
}

/* The argument if it is a known cursor for the db, otherwise NULL.
   Returns NULL if either argument is NULL.
*/
static db_cursor * find_cursor(db_struct *db, db_cursor *cu)
{
  if (db != NULL) {
    db_cursor *p = db->next_cursor;
    while (p != NULL) {
      if (p == cu) return p;
      p = p->next_cursor;
    }
  }
  return NULL;
}

static void link_cursor(db_struct *db, db_cursor *cu)
{
  SP_ASSERT(*find_cursor_link(db, cu) == NULL);
  SP_ASSERT(find_cursor(db, cu) == NULL);
  cu->next_cursor = db->next_cursor;
  db->next_cursor = cu;
  SP_ASSERT(find_cursor(db, cu) == cu);
  SP_ASSERT(*find_cursor_link(db, cu) == cu);
}

static void unlink_cursor(db_struct *db, db_cursor *cu)
{
  SP_ASSERT(find_cursor(db, cu) == cu);
  SP_ASSERT(*find_cursor_link(db, cu) == cu);
  *find_cursor_link(db, cu) = cu->next_cursor;
  cu->next_cursor = NULL;
  SP_ASSERT(*find_cursor_link(db, cu) == NULL);
  SP_ASSERT(find_cursor(db, cu) == NULL);
}


/*
  Database functions
 */
static int db_open_files(db_struct *db, unsigned int flags)
{
  int err = 0;
  unsigned int dbi_flags = ((flags & MDB_CREATE) | MDB_INTEGERKEY);

  if ((err = mdb_dbi_open(db->txn, TERMSNAME, dbi_flags, &db->terms_dbi)))
    return err;
  if ((err = mdb_dbi_open(db->txn, ADMINNAME, dbi_flags, &db->admin_dbi)))
    return err;
  if ((err = mdb_dbi_open(db->txn, INDEXNAME, dbi_flags|MDB_DUPSORT|MDB_INTEGERDUP, &db->index_dbi)))
    return err;

  return err;
}

static int new_db_struct(char const *path, db_struct **pdb)
{
  db_struct *db;
  SP_atom path_atom;

  if (*find_db_for_path(path) != NULL) {
    /* Already open. We must not accept this since it leads to memory
       corruption and whatnot. */
    return EPERM;
  }
  path_atom = SP_atom_from_string(path);
  SP_ASSERT(path_atom != 0);
  if (path_atom == 0) {
    /* This "cannot" happen ince the path is supposed to be the printname of an atom. */
    return EPERM;               /* Good enough. */
  }

  db = (db_struct *)SP_malloc(sizeof(db_struct));
  if (db == NULL) return ENOMEM;

  memset(db, 0, sizeof(db_struct));
  SP_register_atom(path_atom); /* We do not bother unregistering. */
  db->path = SP_string_from_atom(path_atom);
  db->next = NULL;
  db->next_iterator = NULL;
  db->next_cursor = NULL;
  link_db(db);
  /* Now db is safe to close */
  *pdb = db;
  return 0;
}

#define CLOSE_DB_OPTION_NONE  0x0000
#define CLOSE_DB_OPTION_ABORT 0x0001

static int close_db_int(db_struct *db, unsigned int close_flags)
{
  int err = 0;
  unsigned int flags;
  int abortive = ((close_flags & CLOSE_DB_OPTION_ABORT) != 0);

  unlink_db(db);

  // Free the iterator structs.  Cursors are closed/deallocated
  // automatically in some cases, so best to ignore the cursor but
  // unlink and deallocate the iterator struct itself.
  {
    iterator *next_iterator = db->next_iterator;
    db->next_iterator = NULL;
    while (next_iterator != NULL) {
      iterator *it = next_iterator;
      next_iterator = next_iterator->next_iterator;
      SP_free(it);
    }
  }
  // Free the db_cursor structs.  Here to we do not dare closing the cursor itself.
  {
    db_cursor *next_cursor = db->next_cursor;
    db->next_cursor = NULL;
    while (next_cursor != NULL) {
      db_cursor *cursor = next_cursor;
      next_cursor = next_cursor->next_cursor;
      SP_free(cursor);
    }
  }

  if (db->env) {
    mdb_env_get_flags(db->env, &flags);
    if (db->txn) {
      if ((flags & MDB_RDONLY) || abortive)
        mdb_txn_abort(db->txn);
      else if ((err = mdb_txn_commit(db->txn)))
        return err;
    }

  /* lmdb.h says many contradictory things about mdb_dbi_open() and
     mdb_env_close(), but one thing that cannot be misinterpreted is
     the documentation for mdb_dbi_close(), which says: "Closing a
     database handle is not necessary", so we do not.

     if (db->terms_dbi) mdb_dbi_close(db->env, db->terms_dbi);
     if (db->index_dbi) mdb_dbi_close(db->env, db->index_dbi);
     if (db->admin_dbi) mdb_dbi_close(db->env, db->admin_dbi);
   */

    mdb_env_close(db->env);
  }
  SP_free(db);
  return 0;
}


SP_integer SPCDECL create_db(char SP_FLI_CONST *path,
                             SP_integer mapsize, /* in MiB */
                             SP_integer lspec,
                             SP_integer specsize,
                             SP_integer permission)
{
  db_struct *db = NULL;
  spio_t_uint32 k, r = 0;
  MDB_val key, data;
  int err;
  if ((err = new_db_struct(path, &db)))
    goto error;
  if ((err = mdb_env_create(&db->env)))
    goto error;
  if ((err = mdb_env_set_mapsize(db->env, (size_t)mapsize << 20)))
    goto error;
  if ((err = mdb_env_set_maxdbs(db->env, 3)))
    goto error;
  if ((err = mdb_env_open(db->env, path, 0, (mdb_mode_t)permission)))
    goto error;
  if ((err = mdb_txn_begin(db->env, NULL, 0, &db->txn)))
    goto error;
  if ((err = db_open_files(db, MDB_CREATE)))
    goto error;

  k = NREFKEY;                /* store the next available TERMREF */
  key.mv_data = &k;
  key.mv_size = sizeof(k);
  data.mv_data = &r;
  data.mv_size = sizeof(r);

  if ((err = mdb_put(db->txn, db->admin_dbi, &key, &data, 0)))
    goto error;
  k = SPECKEY;                /* store the speclist */
  key.mv_data = &k;
  data.mv_data = (void *)lspec;
  data.mv_size = specsize;
  if ((err = mdb_put(db->txn, db->admin_dbi, &key, &data, 0)) != 0)
    goto error;
  if ((err = mdb_txn_commit(db->txn)))
    goto error;
  db->txn = NULL;

 error:
  close_db_int(db, (err != 0 ? CLOSE_DB_OPTION_ABORT :  CLOSE_DB_OPTION_NONE));
  SP_ASSERT(find_open_db(db) == NULL);
  return err;
}

/* Note: lmdb.h says "Do not have open an LMDB database twice in the
   same process at the same time.", which we guards against here. */
SP_integer SPCDECL open_db(char const *path,
                           SP_integer mapsize, /* in MiB */
                           SP_integer permission,
                           char const *mode, /* in [read,write] */
                           db_struct **dbp)
{
  unsigned int envflags = 0;
  unsigned int dbiflags = 0;
  int err;
  db_struct *db = NULL;

  if ((err = new_db_struct(path, &db)))
    goto error;

  if (!strcmp(mode, "read")) {
    envflags |= MDB_RDONLY;
    dbiflags |= MDB_RDONLY;
  }
  if ((err = mdb_env_create(&db->env)))
    goto error;
  if ((err = mdb_env_set_mapsize(db->env, (size_t)mapsize << 20)))
    goto error;
  if ((err = mdb_env_set_maxdbs(db->env, 3)))
    goto error;
  if ((err = mdb_env_open(db->env, path, envflags, (mdb_mode_t)permission)))
    goto error;
  if ((err = mdb_txn_begin(db->env, NULL, dbiflags, &db->txn)))
    return err;
  if ((err = db_open_files(db, dbiflags)))
    goto error;

  SP_ASSERT(find_open_db(db) == db);
  *dbp = db;
  return 0;

 error:
  *dbp = NULL;
  if (db != NULL) close_db_int(db, CLOSE_DB_OPTION_ABORT);
  return err;
}

// Do not call this directly, except from Prolog. Use close_db_int() instead.
SP_integer SPCDECL close_db(db_struct *db_untrusted)
{
  db_struct *db = find_open_db(db_untrusted);
  if (db == NULL) return EINVAL;

  return close_db_int(db, CLOSE_DB_OPTION_NONE);
}

SP_integer SPCDECL read_spec(db_struct *db_untrusted, SP_integer *lspeclist)
{
  int err;
  db_struct *db = find_open_db(db_untrusted);
  if (db == NULL) return EINVAL;

  if (db->admin_dbi) {
    spio_t_uint32 k = SPECKEY;
    MDB_val key, data;

    key.mv_data = &k;
    key.mv_size = sizeof(k);
    if ((err = mdb_get(db->txn, db->admin_dbi, &key, &data)))
      return err;
    *lspeclist = (SP_integer)data.mv_data;
    return 0;
  }
  return EINVAL;
}

SP_integer SPCDECL next_termref(db_struct *db_untrusted, SP_integer *termref)
{
  int err;
  spio_t_uint32 k = NREFKEY, d;
  MDB_val key, data;
  db_struct *db = find_open_db(db_untrusted);
  if (db == NULL) return EINVAL;

  key.mv_data = &k;
  key.mv_size = sizeof(k);
  if ((err = mdb_get(db->txn, db->admin_dbi, &key, &data)))
    return err;
  *termref = d = *(spio_t_uint32 *)data.mv_data;

  if (++d == 0)
    return ERANGE;              /* Result too large */
  data.mv_data = &d;
  data.mv_size = sizeof(d);
  if ((err = mdb_put(db->txn, db->admin_dbi, &key, &data, 0)))
    return err;
  return err;
}

SP_integer SPCDECL store_termref(db_struct *db_untrusted, SP_integer hc, SP_integer termref)
{
  int err;
  spio_t_uint32 k = (spio_t_uint32)hc;
  spio_t_uint32 d = (spio_t_uint32)termref;
  MDB_val key, data;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;
  key.mv_data = &k;
  key.mv_size = sizeof(k);
  data.mv_data = &d;
  data.mv_size = sizeof(d);
  if ((err = mdb_put(db->txn, db->index_dbi, &key, &data, 0)))
    return err;
  return err;
}

SP_integer SPCDECL delete_termref(db_struct *db_untrusted, SP_integer hc, SP_integer termref)
{
  int err;
  spio_t_uint32 k = (spio_t_uint32)hc;
  spio_t_uint32 d = (spio_t_uint32)termref;
  MDB_val key, data;
  MDB_cursor *cursor;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;
  if ((err = mdb_cursor_open(db->txn, db->index_dbi, &cursor)))
    return err;
  key.mv_data = &k;
  key.mv_size = sizeof(k);
  for (err = mdb_cursor_get(cursor, &key, &data, MDB_SET);
       err == 0;
       err = mdb_cursor_get(cursor, &key, &data, MDB_NEXT_DUP)) {
    if (d == *(spio_t_uint32 *)data.mv_data) {
      err = mdb_cursor_del(cursor, 0);
      break;
    }
  }
  mdb_cursor_close(cursor);

  if (err != MDB_NOTFOUND && err != 0)
    return err;

  return 0;
}

SP_integer SPCDECL store_term(db_struct *db_untrusted, SP_integer termref, SP_integer lterm, SP_integer termsize)
{
  int err;
  spio_t_uint32 k = (spio_t_uint32)termref;
  MDB_val key, data;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;
  key.mv_data = &k;
  key.mv_size = sizeof(k);
  data.mv_data = (void *)lterm;
  data.mv_size = termsize;
  if ((err = mdb_put(db->txn, db->terms_dbi, &key, &data, 0)))
    return err;
  return err;
}

SP_integer SPCDECL delete_term(db_struct *db_untrusted, SP_integer termref)
{
  int err;
  spio_t_uint32 k = (spio_t_uint32)termref;
  MDB_val key;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;
  key.mv_data = &k;
  key.mv_size = sizeof(k);
  if ((err = mdb_del(db->txn, db->terms_dbi, &key, NULL)))
    return err;
  return err;
}

SP_integer SPCDECL fetch_term(db_struct *db_untrusted, SP_integer termref, SP_integer *lterm)
{
  int err;
  spio_t_uint32 k = (spio_t_uint32)termref;
  MDB_val key, data;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;
  key.mv_data = &k;
  key.mv_size = sizeof(k);
  if ((err = mdb_get(db->txn, db->terms_dbi, &key, &data)))
    return err;
  *lterm = (SP_integer)data.mv_data;
  return 0;
}

/*
  If there are no more solutions the iterator functions succeed but set
  term to NULL.
 */
SP_integer SPCDECL global_iterator(db_struct *db_untrusted, db_cursor **cup)
{
  int err;
  db_struct *db = find_open_db(db_untrusted);
  db_cursor *cu;

  if (db == NULL) return EINVAL;

  cu = (db_cursor*) SP_malloc(sizeof(db_cursor));
  if (cu == NULL) return ENOMEM;

  cu->cursor = NULL;
  cu->next_cursor = NULL;

  if ((err = mdb_cursor_open(db->txn, db->terms_dbi, &cu->cursor))) {
    *cup = NULL;
    SP_free(cu);
    return err;
  }
  link_cursor(db, cu);

  *cup = cu;
  return 0;
}


SP_integer SPCDECL global_iterator_next(db_struct *db_untrusted, db_cursor *cu_untrusted, SP_integer *lterm, SP_integer *termref)
{
  int err;
  MDB_val key, data;
  db_struct *db = find_open_db(db_untrusted);
  db_cursor *cu = find_cursor(db, cu_untrusted);
  if (cu == NULL) return EINVAL;

  if ((err = mdb_cursor_get(cu->cursor, &key, &data, MDB_NEXT))) {
    if (err == MDB_NOTFOUND) { /* no more solutions */
      *lterm = (SP_integer)NULL;
      *termref = 0; /* Do not pass garbage to Prolog on success. Also, valgrind complained. */
      return 0;
    }
    return err;
  }

  *lterm = (SP_integer)data.mv_data;
  *termref = *(spio_t_uint32 *)key.mv_data;

  return 0;
}

SP_integer SPCDECL global_iterator_done(db_struct *db_untrusted, db_cursor *cu_untrusted)
{
  db_struct *db = find_open_db(db_untrusted);
  db_cursor *cu = find_cursor(db, cu_untrusted);
  if (cu == NULL) return EINVAL;

  mdb_cursor_close(cu->cursor);
  unlink_cursor(db, cu);
  SP_free(cu);

  return 0;
}

SP_integer SPCDECL term_iterator(db_struct *db_untrusted, SP_term_ref hclist, iterator **itp)
{
  int err;
  iterator *it;
  SP_term_ref ref = SP_new_term_ref();
  int hcnum;
  SP_integer *keyptr;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;

  /* Count the length of the list */
  hcnum = 0;
  SP_put_term(ref, hclist);
  while (SP_is_list(ref)) {
    hcnum++;
    SP_get_arg(2, ref, ref);
  }

  if (hcnum <= 0) {
    /* The hcnum list should never be empty (and it would break the size calculation below).*/
    return EINVAL;
  }

  it = (iterator *)SP_malloc(sizeof(iterator) + (hcnum-1)*sizeof(SP_integer));
  if (it == NULL) {
    *itp = NULL;
    return ENOMEM;
  }

  it->next_iterator = NULL;
  it->nkeys = hcnum;
  it->ckey = -1;
  for (keyptr = &it->key[0];
       SP_is_list(hclist);
       SP_get_arg(2, hclist, hclist), ++keyptr) {
    SP_get_arg(1, hclist, ref);
    SP_get_integer(ref, keyptr);
  }
  if ((err = mdb_cursor_open(db->txn, db->index_dbi, &it->cursor))) {
    SP_free(it);
    *itp = NULL;
    return err;
  }
  SP_ASSERT(it->cursor != NULL);
  link_iterator(db, it);
  *itp = it;
  return 0;
}

SP_integer SPCDECL term_iterator_next(db_struct *db_untrusted, iterator *it_untrusted, SP_integer *lterm, SP_integer *termref)
{
  int err;
  MDB_val key, data, tdata;
  spio_t_uint32 k;
  MDB_txn *txn;
  db_struct *db = find_open_db(db_untrusted);
  iterator *it = find_iterator(db, it_untrusted);
  if (it == NULL) return EINVAL;

  if (it->ckey == -1) goto newhc; /* is this the very first time? */

next:                 /* find the next termref under the current hash code */
  if ((err = mdb_cursor_get(it->cursor, &key, &data, MDB_NEXT_DUP))) {
    if (err == MDB_NOTFOUND) goto newhc; /* maybe the next key */
    else return err;          /* real error */
  }
fetch:                          /* MDB_SET or MDB_NEXT_DUP succeeded */
  txn = mdb_cursor_txn(it->cursor);
  err = mdb_get(txn, db->terms_dbi, &data, &tdata);
  if (err == 0) {             /* success */
    *lterm = (SP_integer)tdata.mv_data;
    *termref = *(spio_t_uint32 *)data.mv_data;

    return 0;
  }
  if (err != MDB_NOTFOUND) return err; /* real error */
  goto next;

newhc:                          /* a new hash code */
  if (++it->ckey >= it->nkeys) { /* no more keys to try */
    *lterm = (SP_integer)NULL;
    *termref = 0; /* Do not pass garbage to Prolog on success. Also, valgrind complained. */
    return 0;
  }

  k = (spio_t_uint32)it->key[it->ckey];
  key.mv_data = &k;
  key.mv_size = sizeof(spio_t_uint32);
  err = mdb_cursor_get(it->cursor, &key, &data, MDB_SET);
  if (err == 0) goto fetch;
  if (err == MDB_NOTFOUND) goto newhc; /* maybe the next key */
  return err;                 /* real error */
}

SP_integer SPCDECL term_iterator_done(db_struct *db_untrusted, iterator *it_untrusted)
{
  db_struct *db = find_open_db(db_untrusted);
  iterator *it = find_iterator(db, it_untrusted);
  if (it == NULL) return EINVAL;

  {
    MDB_cursor *cursor = it->cursor;
    unlink_iterator(db, it);
    SP_free(it);
    mdb_cursor_close(cursor);
    return 0;
  }
}

SP_integer SPCDECL property(db_struct *db_untrusted, char SP_FLI_CONST *key, SP_term_ref value)
{
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;

  if (!strcmp(key, "path")) {
    const char *path;
    mdb_env_get_path(db->env, &path);
    SP_put_string(value, path);
  } else if (!strcmp(key, "mapsize")) {
    MDB_envinfo stat;
    mdb_env_info(db->env, &stat);
    SP_put_integer(value, stat.me_mapsize >> 20);
  } else if (!strcmp(key, "rdonly")) {
    unsigned int flags;
    mdb_env_get_flags(db->env, &flags);
    SP_put_integer(value, (flags & MDB_RDONLY) ? 1 : 0);
  } else {
    return EINVAL;
  }
  return 0;
}

SP_integer SPCDECL sync_db(db_struct *db_untrusted)
{
  int err = 0;
  unsigned int flags;
  db_struct *db = find_open_db(db_untrusted);

  if (db == NULL)
    return EINVAL;

  mdb_env_get_flags(db->env, &flags);
  if (!(flags & MDB_RDONLY)) {
    if ((err = mdb_txn_commit(db->txn)))
      return err;
    if ((err = mdb_txn_begin(db->env, NULL, 0, &db->txn)))
      return err;
  }
  return 0;
}

#define HASHADD(H,T) (H=((H<<5)+(H>>27))^T)

static unsigned short tab[] = {
  0x3760, 0xA26C, 0xC31A, 0x6623, 0x3D47, 0x5495, 0xD8F3, 0xAF81,
  0x455C, 0xD463, 0x43DD, 0xFA7A, 0x4641, 0xAF18, 0x07FF, 0x3626,
  0xA8F1, 0x42DF, 0x6EEC, 0xE1DA, 0x4C0F, 0x28D9, 0x7546, 0xC69C,
  0xE94D, 0xF796, 0x9D7F, 0x75E7, 0xE1F0, 0x2904, 0x294C, 0xB235,
  0x5C6A, 0x04DE, 0xE5EA, 0x19CC, 0x8DB4, 0xEEC1, 0x1B7F, 0xAC58,
  0x63F6, 0x8E9A, 0x186F, 0x4E8E, 0x0AD0, 0x6C04, 0x7B20, 0x0169,
  0xE5F9, 0x22FA, 0x7286, 0xF019, 0x86B3, 0x6C49, 0x8988, 0x38C1,
  0x9B3C, 0xDF5B, 0x6030, 0x356F, 0x5371, 0x7268, 0x61BB, 0x0D0C,
  0x8F5E, 0xBBC1, 0x4876, 0xFAFA, 0x968B, 0x6894, 0x4566, 0x9DC9,
  0x4D31, 0x63A5, 0xD3D3, 0x8E98, 0xA231, 0x25BE, 0x5A62, 0xE33A,
  0xD7A2, 0x4555, 0x8E25, 0x3234, 0xD40E, 0xB410, 0x4F4E, 0x86A3,
  0xC2C9, 0xED06, 0xEB1F, 0xC66C, 0x8346, 0x7AFC, 0x797A, 0x3B7A,
  0xD902, 0xAB45, 0xBDB8, 0xD564, 0xEEA5, 0xDCE5, 0xDD4C, 0xDFB2,
  0x6ECA, 0xB59A, 0x3731, 0x3B7F, 0x475F, 0xCCCA, 0xF987, 0xEAD3,
  0xE226, 0x53FD, 0x4404, 0x6CD4, 0x1869, 0xF861, 0x8BC1, 0x4967,
  0x8967, 0x1657, 0x617E, 0xEDFA, 0x89C6, 0x4084, 0xD086, 0x930D,
  0x25EC, 0x3083, 0x374E, 0x50D2, 0xFE5C, 0x4249, 0xA601, 0x3761,
  0x9016, 0xB838, 0xD63D, 0x55DA, 0xCB34, 0x0C1F, 0xA3D4, 0x60C1,
  0xAB4B, 0xD1A1, 0x302A, 0x2AF0, 0xAFED, 0x8D44, 0xA74E, 0x0E7C,
  0x852A, 0x825B, 0xA9D5, 0xDD59, 0x2C56, 0x861C, 0x90B4, 0xEEB7,
  0x76E5, 0x1125, 0x9C02, 0xAE4A, 0xCE3C, 0xA234, 0xBEE7, 0x4354,
  0x02F2, 0xB351, 0x74FE, 0xFE4A, 0xCDA0, 0x5B6A, 0x1B92, 0x5805,
  0xE87F, 0x91A4, 0x533E, 0x7458, 0x0F89, 0x5DC9, 0x27E3, 0x741C,
  0xD716, 0xC5FC, 0xC8F1, 0xE8A9, 0x1784, 0x747B, 0x983A, 0x14D3,
  0x3085, 0x741E, 0x905A, 0x7691, 0x2532, 0x9F07, 0x12D2, 0x5882,
  0xCC08, 0x2FC5, 0xE644, 0xB3E3, 0xAFCC, 0xD692, 0x9B31, 0x7AF3,
  0x3A4D, 0xB119, 0xE039, 0x57E0, 0x1C4A, 0xF00D, 0x3A0F, 0x7504,
  0x4AFD, 0x5C69, 0x235C, 0xA1F1, 0x16DA, 0xD972, 0x509F, 0xE69B,
  0x682E, 0x54B0, 0xFC0F, 0xC924, 0xDBD6, 0xC946, 0x9249, 0x3F35,
  0x9FAB, 0x7391, 0xB2A7, 0xE736, 0x8370, 0x34BB, 0x9ACC, 0xC6FD,
  0x636C, 0x437B, 0x38BD, 0x7A57, 0x8D27, 0xFE1A, 0xA742, 0xF690,
  0x9944, 0xB08D, 0xF227, 0x7FD8, 0x42B6, 0xEC83, 0xD27A, 0x4ED4
  };


/*
  The argument is a list of atomic items.
*/
SP_integer SPCDECL db_term_hash(SP_term_ref term)
{
  register int i;
  register SP_uinteger code = 0;
  SP_term_ref elt = SP_new_term_ref();
  char const *s;

  while (SP_term_type(term) == SP_TYPE_COMPOUND) {
    SP_get_arg(1, term, elt);
    switch (SP_term_type(elt)) {
    case SP_TYPE_ATOM:
      SP_get_string(elt, &s);
      goto hash_codes;
    case SP_TYPE_INTEGER:
    case SP_TYPE_FLOAT:
      SP_get_number_codes(elt, &s);
    hash_codes:
      for (i=0; s[i]; i++)
        HASHADD(code, tab[*(unsigned char *)(s+i)]);
      break;
    }
    SP_get_arg(2, term, term);
  }
  return code & 0xffffffff;
}

#define NA -1                   /* not applicable */
#define NI -2                   /* instantiatedness */

/*
  ixkeys(spec, term, list): list is the list of arguments of term in the
  `+' positions of spec.  Compound terms are represented by their functors.
  The return value is the number of `+' positions in spec or NA if the
  functors of spec and term are not equal or NI if term contains a variable
  on some `+' position.
 */
SP_integer SPCDECL ixkeys(SP_term_ref spec, SP_term_ref term, SP_term_ref list)
{
  SP_atom sname, tname, plus;
  int sarity, tarity, i;
  SP_integer ret = 0;
  SP_term_ref arg = SP_new_term_ref(), tmp = SP_new_term_ref();

  SP_get_functor(spec, &sname, &sarity);
  SP_get_functor(term, &tname, &tarity);
  if (sname != tname || sarity != tarity) return NA;

  plus = SP_atom_from_string("+");
  SP_register_atom(plus);       /* [PM] 4.0.2+ */
  for (i = sarity; i > 0; --i) { /* sarity and i are never used together */
    SP_atom t;

    SP_get_arg(i, spec, arg);
    SP_get_atom(arg, &t);       /* no error checking */
    if (t != plus) continue;

    SP_get_arg(i, term, arg);
    switch (SP_term_type(arg)) {
    case SP_TYPE_VARIABLE:
      return NI;
    case SP_TYPE_COMPOUND:
      SP_get_functor(arg, &tname, &tarity);
      SP_put_integer(tmp, (SP_integer)tarity);
      SP_cons_list(list, tmp, list);
      SP_put_atom(arg, tname);
      break;
    }
    SP_cons_list(list, arg, list);
    ++ret;
  }
  return ret;
}

char SP_FLI_CONST * SPCDECL decode_error(SP_integer err_code)
{
  int err = (int) err_code;

  /* On Windows, mdb_strerror() returns a pointer to the popped part
     of the stack. This is not acceptable. Also, it uses strerror for
     errno codes, but that makes it locale dependent which we never
     want. So, instead, we roll our own except for the MDB_xxx error
     codes we know have hardwired descriptions in mdb.c
  */

  SP_ASSERT(err != 0); /* should only be called for errors */

  /* Only use mdb_strerror() for the MDB error codes. We reproduce the
     logic here an hope it does not change in mdb.c. */
  /* If the number of errors change, reverify the code. */
  SPIO_COMPILETIME_ASSERT(MDB_LAST_ERRCODE > MDB_KEYEXIST && (MDB_LAST_ERRCODE - MDB_KEYEXIST) == 19);
  if (err == 0 /* mdb_strerror() has a case for no-error as well. */
      || (MDB_KEYEXIST <= err && err <= MDB_LAST_ERRCODE)) {
    return mdb_strerror(err);
  }

  /* LMDB reports some of its owb errors using POSIX error codes (!),
     also on Windows (!!), so try to handle those here. */
  switch(err) {

    /* Some error codes we know can happen. */
  case EPERM: return "EPERM"; /* Permission error. We use this when
                                 user attempts to open an already open
                                 database file.. */

    /* The documented error codes in lmdb.h are: */
  case EACCES: return "EACCES"; /* an attempt was made to write in a read-only transaction. (and others) */
  case EAGAIN: return "EAGAIN"; /* the environment was locked by another process. */
  case EINVAL: return "EINVAL"; /* an invalid parameter was specified, or the environment has an active write transaction. */
  case EIO: return "EIO";       /* a low-level I/O error occurred while writing. */
  case ENOENT: return "ENOENT"; /* the directory specified by the path parameter doesn't exist. */
  case ENOMEM: return "ENOMEM"; /* out of memory. */
  case ENOSPC: return "ENOSPC"; /* no more disk space. */

    /* The following additional error codes are explicitly mentioned/used in mdb.c. */
  case EOWNERDEAD: return "EOWNERDEAD"; /* - "Internal error codes, not exposed outside liblmdb" */
  case EPIPE: return "EPIPE"; /* - Not clear if this can escape, but only used in "compacting thread", do we use that? */
  case EROFS: return "EROFS"; /* - Only interally in mdb.c */
  default:
    break;
  }

  SP_SOFT_ASSERT(0); /* Tell me if this happens, so we can handle it. */

  return "UNKNOWN";
}

void SPCDECL db_init(int when)
{
  (void)when;
  db_structs = NULL;
}

void SPCDECL db_deinit(int when)
{
  (void)when;

  /* This will trigger if user halts (or reloads this FR) without
     closing all db_struct objects, which is not a bug in this
     library. */
  /* SP_SOFT_ASSERT(db_structs == NULL); */

  /* Leaving DBs open would circumvent our guard against opening the
     same DB file more than once, in which case LMDB may quietly do
     the (possibly fatally) wrong things. One not very unlikely
     scenario is if user reloads this foreign resources.
  */

  while (db_structs != NULL) {
    close_db_int(db_structs, CLOSE_DB_OPTION_ABORT);
  }
}
