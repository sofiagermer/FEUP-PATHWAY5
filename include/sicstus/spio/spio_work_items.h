#ifndef SPIO_WORK_ITEMS_H_INCLUDED
#define SPIO_WORK_ITEMS_H_INCLUDED 1
#include "spio_types.h"
#include "spio_errors.h"

typedef struct spio_t_work_item_ spio_t_work_item;

#define SPIO_WORK_ITEM_RUN_OPTION_SHUTDOWN SPIO_BIT(0)
/* Only SPIO_S_DEALLOCATED has significance (in which case dealloc will not be called) */
typedef spio_t_error_code spio_t_wi_run(spio_t_work_item *, spio_t_bits);
typedef void spio_t_wi_dealloc(spio_t_work_item *);

typedef struct spio_t_work_item_funcs_ spio_t_work_item_funcs;
struct spio_t_work_item_funcs_ {
  spio_t_wi_run *run;
  spio_t_wi_dealloc *dealloc;
};

struct spio_t_work_item_ {
  spio_t_work_item_funcs *funcs;
  spio_t_bits flags;
  void *cookie;
  spio_t_work_item *next;
};

extern void spio_init_work_item(spio_t_work_item *item, spio_t_work_item_funcs *funcs, void *cookie);
extern void spio_free_work_item(spio_t_work_item *item);
extern spio_t_error_code spio_work_items_pending(spio_t_bits options);
#define SPIO_RUN_WORK_ITEMS_OPTIONS_SHUTDOWN SPIO_BIT(0)
extern spio_t_error_code spio_run_work_items(spio_t_bits options);
/* Do not interrupt I/O, just schedule for next time work items are run  */
#define SPIO_SCHEDULE_WORK_ITEM_OPTION_NO_INTERRUPT SPIO_BIT(0)
extern spio_t_error_code spio_schedule_work_item(spio_t_work_item *item, spio_t_bits options);
extern spio_t_error_code spio_work_items_init(spio_t_bits options);

#endif  /* SPIO_WORK_ITEMS_H_INCLUDED */
