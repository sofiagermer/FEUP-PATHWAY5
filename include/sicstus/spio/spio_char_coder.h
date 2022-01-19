#ifndef SPIO_CHAR_CODER_H_INCLUDED
#define SPIO_CHAR_CODER_H_INCLUDED

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_buf.h"
#include "spio.h"

#if !defined SPIO_CODER_DO_NOT_OVERALLOCATE
#define SPIO_CODER_DO_NOT_OVERALLOCATE 1
#endif /* !defined SPIO_CODER_DO_NOT_OVERALLOCATE */

typedef struct spio_t_coder_ spio_t_coder;

typedef struct spio_t_coder_funcs_ spio_t_coder_funcs;
struct spio_t_coder_funcs_ {
  size_t sizeof_spio_t_coder_funcs; /* version check */
  char const *name;
  char const * const *aliases;  /* NULL terminated vector of alias names or NULL for no aliases */

#define SPIO_CHAR_CODER_FUNCS_FEATURE_CAN_SEEK SPIO_BIT(0) /* [PM] 4.0.6 accepts SPIO_CHAR_CODER_OPEN_OPTION_SEEKABLE (provided EOL==LF !) */

  spio_t_bits features;       /* capabilites of coders of this class */

  spio_t_refcount refcount;     /* (optionally) used by addref/release static methods to manage freeing of funcs vector */

  /* instance methods */

  void(*free)(spio_t_coder *coder);

#define SPIO_CHAR_CODER_ENCODE_OPTION_EOF SPIO_BIT(0) /* this is the last block of data */
/* #define SPIO_CHAR_CODER_ENCODE_OPTION_NONSTRICT SPIO_NEXT_BIT(...) */

/* Fallback also on invalid sequence (not only on unmappable code). Combine with the other fallback options. */
#define SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_ON_INVALID SPIO_NEXT_BIT(SPIO_CHAR_CODER_ENCODE_OPTION_EOF)

/* Fallback to whatever was specified on open (NOT CURRENTLY USED) */
#define SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_DEFAULT_ SPIO_NEXT_BIT(SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_ON_INVALID)

/* Do not fall back, always report error on unmappable and invalid sequence */
#define SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_ERROR SPIO_NEXT_BIT(SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_DEFAULT_)
/* Fallback to substitution char(s)/byte(s) */
#define SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_REPLACE SPIO_NEXT_BIT(SPIO_CHAR_CODER_ENCODE_OPTION_FALLBACK_ERROR)

  spio_t_error_code (*encode)(spio_t_coder *coder, spio_t_buf *from_buf, size_t *pfrom_buf_offset, spio_t_buf *to_buf, spio_t_bits options);

  void (*commit)(spio_t_coder *coder);
  void (*rollback)(spio_t_coder *coder);

  spio_t_error_code (*translate_offset)(spio_t_coder *coder, spio_t_offset from_unicode_byte_offset, spio_t_offset *pto_byte_offset);
  spio_t_error_code (*seek)(spio_t_coder *coder, spio_t_offset unicode_byte_offset, spio_t_bits options);

  /* static methods */

  void (*addref)(spio_t_coder_funcs *); /* use spio_coder_default_addref for default handling */
  void (*release)(spio_t_coder_funcs *); /* use spio_coder_default_release for default handling */
  void (*destroy)(spio_t_coder_funcs *); /* use spio_coder_default_destroy for default handling */

#define SPIO_CHAR_CODER_OPEN_OPTION_SEEKABLE      SPIO_BIT(0)
#define SPIO_CHAR_CODER_OPEN_OPTION_DECODE        SPIO_NEXT_BIT(SPIO_CHAR_CODER_OPEN_OPTION_SEEKABLE) /* decode from bytes to INTERNAL wchar */
#define SPIO_CHAR_CODER_OPEN_OPTION_EOL_CRLF      SPIO_NEXT_BIT(SPIO_CHAR_CODER_OPEN_OPTION_DECODE)
#define SPIO_CHAR_CODER_OPEN_OPTION_EOL_AUTO      SPIO_NEXT_BIT(SPIO_CHAR_CODER_OPEN_OPTION_EOL_CRLF)
#define SPIO_CHAR_CODER_OPEN_OPTION_BOM           SPIO_NEXT_BIT(SPIO_CHAR_CODER_OPEN_OPTION_EOL_AUTO) /* expect bom on read, emit bom (if possible) on write */
#define SPIO_CHAR_CODER_OPEN_OPTION_BOM_OPTIONAL  SPIO_NEXT_BIT(SPIO_CHAR_CODER_OPEN_OPTION_BOM) /* emit only if this is the default for the encoding (e.g., not for UTF-8 but for UTF-16) */

/* #define SPIO_CHAR_CODER_OPEN_OPTION_NONSTRICT     SPIO_NEXT_BIT(SPIO_CHAR_CODER_OPEN_OPTION_EOL_AUTO) */


  spio_t_error_code (*open)(spio_t_coder_funcs *funcs, spio_t_coder **pcoder, spio_t_arglist args, spio_t_bits options);

  /* private class members */
  /* ... */
};


struct spio_t_coder_ {
  spio_t_coder_funcs *funcs;          /* open must set this */

#define SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_1 SPIO_BIT(0)
#define SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_2 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_1)
#define SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_4 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_2)

#define SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_1 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_4)
#define SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_2 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_1)
#define SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_4 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_2)

#define SPIO_CHAR_CODER_FLAG_DECODE SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_4) /* from byte to INTERNAL wchar */
#define SPIO_CHAR_CODER_FLAG_SEEKABLE SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_DECODE)
#define SPIO_CHAR_CODER_FLAG_EOL_CRLF SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_SEEKABLE)
#define SPIO_CHAR_CODER_FLAG_EOL_AUTO SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_EOL_CRLF)
  /* [PM] 4.0.6 The absence of SPIO_CHAR_CODER_FLAG_EOL_CRLF/AUTO is taken to imply fixed-width single-byte EOL convention (for seek) */

#define SPIO_CHAR_CODER_FLAG_BOM SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_EOL_AUTO) /* meaning? */

#define SPIO_CHAR_CODER_FLAG_PRIVATE_1 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_BOM)
#define SPIO_CHAR_CODER_FLAG_PRIVATE_2 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_PRIVATE_1)
#define SPIO_CHAR_CODER_FLAG_PRIVATE_3 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_PRIVATE_2)
#define SPIO_CHAR_CODER_FLAG_PRIVATE_4 SPIO_NEXT_BIT(SPIO_CHAR_CODER_FLAG_PRIVATE_3)

/* #define SPIO_CHAR_CODER_FLAG_NONSTRICT .. */

  spio_t_bits flags;

  /*
    FIXME: Consider adding from/to _fallback chars/bytes here. Also
    consider adding a fallback-callback (needed if we want to
    implement the SWI Prolog feature of using XML or Prolog escapes on
    unmappable codes.)
   */

  /* private data */
};

#if !SPIO_BETA_VERSION
#error "FIXME TODO: Should revise the granularity concept here and for layers. Just have WCHAR and OCTET?"
#endif  /* !SPIO_BETA_VERSION */


#define SPIO_CHAR_CODER_FLAG_TO_GRANULARITY(FLAGS) (                    \
   (((FLAGS) & SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_1) ? 1 : 0) \
   +                                                            \
   (((FLAGS) & SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_2) ? 2 : 0) \
   +                                                            \
   (((FLAGS) & SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_4) ? 4 : 0) \
   )

#define SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY(FLAGS) (                    \
   (((FLAGS) & SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_1) ? 1 : 0) \
   +                                                            \
   (((FLAGS) & SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_2) ? 2 : 0) \
   +                                                            \
   (((FLAGS) & SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_4) ? 4 : 0) \
   )

#define SPIO_SET_CHAR_CODER_FLAG_TO_GRANULARITY(FLAGS,GRANULARITY) (          \
   (FLAGS) |=                                                         \
   (((GRANULARITY) & 1) ? SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_1 : 0) \
   |                                                                  \
   (((GRANULARITY) & 2) ? SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_2 : 0) \
   |                                                                  \
   (((GRANULARITY) & 4) ? SPIO_CHAR_CODER_FLAG_TO_GRANULARITY_BIT_4 : 0) \
   )

#define SPIO_SET_CHAR_CODER_FLAG_FROM_GRANULARITY(FLAGS,GRANULARITY) (          \
   (FLAGS) |=                                                         \
   (((GRANULARITY) & 1) ? SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_1 : 0) \
   |                                                                  \
   (((GRANULARITY) & 2) ? SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_2 : 0) \
   |                                                                  \
   (((GRANULARITY) & 4) ? SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY_BIT_4 : 0) \
   )

#define SPIO_CHAR_CODER_TO_GRANULARITY(CODER) SPIO_CHAR_CODER_FLAG_TO_GRANULARITY((CODER)->flags)
#define SPIO_CHAR_CODER_FROM_GRANULARITY(CODER) SPIO_CHAR_CODER_FLAG_FROM_GRANULARITY((CODER)->flags)
#define SPIO_SET_CHAR_CODER_TO_GRANULARITY(CODER, GRANULARITY) SPIO_SET_CHAR_CODER_FLAG_TO_GRANULARITY((CODER)->flags, (GRANULARITY))

/* coder registration */
extern spio_t_error_code spio_register_charset(char const *name, char const * const aliases[], spio_t_bits options);
extern spio_t_error_code spio_register_coder(char const *name, spio_t_coder_funcs *funcs, spio_t_bits options);

/* ref counting the coder funcs */
extern void spio_coder_funcs_addref(spio_t_coder_funcs *funcs);
extern void spio_coder_funcs_release(spio_t_coder_funcs *funcs);

/* default implementations for the static ref counting methods */
extern void spio_coder_default_addref(spio_t_coder_funcs *funcs);
extern void spio_coder_default_release(spio_t_coder_funcs *funcs);
extern void spio_coder_default_destroy(spio_t_coder_funcs *funcs);

/* Returns NULL if no such charset has been registered. The returned
   string should not be deallocated. */
extern spio_t_error_code spio_charset_canonical_name(char const *name, char const **pcanonical_name);
extern int spio_charset_name_equal(char const *name1, char const *name2);


/* coder functions */

/* creation */
#define SPIO_CHAR_CODER_FIND_OPTION_SEEKABLE SPIO_BIT(0) /* the char coder must support seeking */
extern spio_t_error_code spio_char_coder_find(spio_t_coder_funcs **pfuncs, char const *name, spio_t_arglist args, spio_t_bits options);

extern spio_t_error_code spio_char_coder_open(spio_t_coder_funcs *funcs, spio_t_coder **pcoder, spio_t_arglist args, spio_t_bits options);
extern void spio_char_coder_close(spio_t_coder *coder);

/* encode */
extern spio_t_error_code spio_char_coder_encode(spio_t_coder *coder, spio_t_buf *from_buf, size_t *pfrom_buf_offset, spio_t_buf *to_buf, spio_t_bits options);
extern void spio_char_coder_commit(spio_t_coder *coder);
extern void spio_char_coder_rollback(spio_t_coder *coder);

/* back-seekability */

extern spio_t_error_code spio_char_coder_translate_offset(spio_t_coder *coder, spio_t_offset from_unicode_byte_offset, spio_t_offset *pto_byte_offset);
extern spio_t_error_code spio_char_coder_seek(spio_t_coder *coder, spio_t_offset unicode_byte_offset, spio_t_bits options);



#define SPIO_CHAR_CODER_INTERNAL_GRANULARITY 4 /* sizeof spio_t_wchar (our UNICODE representation) */

#define SPIO_CHAR_ASCII_LF 10
#define SPIO_CHAR_ASCII_CR 13
#define SPIO_CHAR_ASCII_SUB 0x1A
#define SPIO_CHAR_NEWLINE 10
#define SPIO_CHAR_ASCII_REPLACEMENT_CHARACTER SPIO_CHAR_ASCII_SUB /* SUB. This is the default used by UNICODE. See UTS #22 <http://www.unicode.org/reports/tr22/> */

#define SPIO_CHAR_UNICODE_REPLACEMENT_CHARACTER ((spio_t_utf16)0xFFFD)
#define SPIO_CHAR_UNICODE_BOM_CHARACTER ((spio_t_utf16)0xFEFF)

/* Note that the surrogates occupies the completa range
   SPIO_UNI_SUR_HIGH_START .. SPIO_UNI_SUR_LOW_END,
   a.k.a. SPIO_UNI_SUR_START .. SPIO_UNI_SUR_END */
#define SPIO_UNI_SUR_HIGH_START  ((spio_t_utf16)0xD800)
#define SPIO_UNI_SUR_HIGH_END    ((spio_t_utf16)0xDBFF)
#define SPIO_UNI_SUR_LOW_START   ((spio_t_utf16)0xDC00) /* i.e., SPIO_UNI_SUR_HIGH_END+1 */
#define SPIO_UNI_SUR_LOW_END     ((spio_t_utf16)0xDFFF)

/* UTF-16 surrogate pair ranges (disallowed in UTF-32) */
#define SPIO_UNI_SUR_START SPIO_UNI_SUR_HIGH_START
#define SPIO_UNI_SUR_END   SPIO_UNI_SUR_LOW_END


extern spio_t_error_code spio_init_char_coder(spio_t_bits options);

#define SPIO_DETECT_FILE_ENCODING_OPTION_IGNORE_UNKOWN_CHARSET SPIO_BIT(0) /* do not report error if (e.g.) -*- coding:<charset> -*- finds an unknown charset */
#define SPIO_DETECT_FILE_ENCODING_OPTION_SEEKABLE SPIO_NEXT_BIT(SPIO_DETECT_FILE_ENCODING_OPTION_IGNORE_UNKOWN_CHARSET) /* char coder must support seek */
extern spio_t_error_code spio_detect_file_encoding(SPIO *s, spio_t_coder_funcs **pcoder_funcs, spio_t_bits options);

#endif  /* SPIO_CHAR_CODER_H_INCLUDED */
