#ifndef SPIO_WIDTH_TABLE_H_INCLUDED
#define SPIO_WIDTH_TABLE_H_INCLUDED

#include "spio_types.h"
#include "spio_errors.h"

typedef struct spio_t_width_table_ spio_t_width_table;

extern spio_t_error_code spio_width_table_items_to_bytes(spio_t_width_table *tab, spio_t_offset item_offset, spio_t_offset *pbyte_offset);
spio_t_error_code spio_width_table_pop_items(spio_t_width_table *tab, spio_t_offset item_offset);

extern spio_t_error_code spio_width_table_compress(spio_t_width_table *);

/* extern spio_t_error_code spio_width_table_size(spio_t_width_table *tab, spio_t_offset *psize); */

extern spio_t_error_code spio_width_table_reserve(spio_t_width_table *, size_t extra);

extern spio_t_error_code spio_width_table_push(spio_t_width_table *tab, size_t width);
extern spio_t_error_code spio_width_table_push_pending(spio_t_width_table *tab, size_t width); /* "ghost"/suppressed item, attaches to next pushed or topmost if nothing pushed yet */

extern spio_t_error_code spio_width_table_pop(spio_t_width_table *tab, size_t *pwidth);
extern spio_t_error_code spio_new_width_table(spio_t_width_table **ptab);

extern spio_t_error_code spio_width_table_transaction_begin(spio_t_width_table *tab);

#define SPIO_WIDTH_TABLE_TRANSACTION_END_OPTION_COMMIT SPIO_BIT(0)
#define SPIO_WIDTH_TABLE_TRANSACTION_END_OPTION_ROLLBACK SPIO_NEXT_BIT(SPIO_WIDTH_TABLE_TRANSACTION_END_OPTION_COMMIT)
#define SPIO_WIDTH_TABLE_TRANSACTION_END_OPTION_ABORT SPIO_NEXT_BIT(SPIO_WIDTH_TABLE_TRANSACTION_END_OPTION_ROLLBACK)
extern spio_t_error_code spio_width_table_transaction_end(spio_t_width_table *tab, spio_t_bits options);



extern void spio_width_table_free(spio_t_width_table *);

#endif  /* SPIO_WIDTH_TABLE_H_INCLUDED */
