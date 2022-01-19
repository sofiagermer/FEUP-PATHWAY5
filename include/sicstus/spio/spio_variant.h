#ifndef SPIO_VARIANT_H_INCLUDED
#define SPIO_VARIANT_H_INCLUDED 1

/* defines spio_t_variant */
#include "spio_types.h" 

extern spio_t_error_code spio_variant_new(spio_t_variant **pvariant);
extern void spio_variant_free(spio_t_variant *v);

#endif  /* SPIO_VARIANT_H_INCLUDED */
