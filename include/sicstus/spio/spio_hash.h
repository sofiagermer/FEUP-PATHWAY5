#ifndef SPIO_HASH_H_INCLUDED
#define SPIO_HASH_H_INCLUDED 1

#include "spio_types.h"

typedef struct spio_t_hash_ spio_t_hash;

#if SPIO_DEBUG
#define spio_hash_new(PTABLE) spio_hash_new_((PTABLE) SPIO_ALLOCATOR_DEBUG_ARGS)
#else  /* !SPIO_DEBUG */
#define spio_hash_new_ spio_hash_new
#endif  /* !SPIO_DEBUG */

extern spio_t_error_code spio_hash_new_(spio_t_hash **ptable SPIO_ALLOCATOR_DEBUG_ARGS_DECL);
extern void spio_hash_free(spio_t_hash *table);

extern spio_t_error_code spio_hash_put(spio_t_hash *table, spio_t_variant *key, spio_t_variant *value);
extern spio_t_error_code spio_hash_get(spio_t_hash *table, spio_t_variant *key, spio_t_variant **value);
extern spio_t_error_code spio_hash_delete(spio_t_hash *table, spio_t_variant *key);

extern spio_t_error_code spio_hash_put_string(spio_t_hash *table, char const *key, spio_t_variant *value);
extern spio_t_error_code spio_hash_get_string(spio_t_hash *table, char const *key, spio_t_variant **value);
extern spio_t_error_code spio_hash_delete_string(spio_t_hash *table, char const *key);


typedef spio_t_error_code spio_t_hash_map_fun (spio_t_variant *key, spio_t_variant *value, void *cookie);

extern spio_t_error_code spio_hash_map(spio_t_hash *table, spio_t_hash_map_fun *fun, void *cookie);
size_t spio_hash_nentries(spio_t_hash *table); /* cannot fail */


#endif  /* SPIO_HASH_H_INCLUDED */
