#ifndef SPIO_UNICODE_H_INCLUDED
#define SPIO_UNICODE_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#define SPIO_UNICODE_MAX_UTF32        (spio_t_wchar)0x7FFFFFFF
#define SPIO_UNICODE_MAX_LEGAL_UTF32  (spio_t_wchar)0x0010FFFF

/* [PM] 4.0.1 SPIO_S_TRUE if code is definitely NFC (as determined by NFC_QC property) */
extern spio_t_error_code spio_unicode_nfc_quick_check_yes(spio_t_wchar wc);

/* [PM] 4.0.1 SPIO_S_TRUE if code is assigned to an abstract character and not a private use char */
extern spio_t_error_code spio_unicode_non_private_abstract_char(spio_t_wchar wc);

#endif  /* SPIO_UNICODE_H_INCLUDED */
