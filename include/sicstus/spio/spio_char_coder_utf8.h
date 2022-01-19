#ifndef SPIO_CHAR_CODER_UTF8_H_INCLUDED
#define SPIO_CHAR_CODER_UTF8_H_INCLUDED

#define SPIO_CHAR_CODER_UTF8_OPEN_OPTION_TCL SPIO_PRIVATE_OPTION_1
#define SPIO_CHAR_CODER_UTF8_OPEN_OPTION_JAVA SPIO_PRIVATE_OPTION_2
/* FIXME: Add JAVA (old (<= 1.4) and new (>= 1.5)), perhaps also _NON_SHORTEST_FORM_NUL */


extern spio_t_error_code spio_init_char_coder_utf8(spio_t_bits options);

/* Used by SICStus */
#define SPIO_DECODE_UTF8_TO_UTF32_OPTION_ALLOW_NON_SHORTEST_FORM_NUL SPIO_BIT(0)
#define SPIO_DECODE_UTF8_TO_UTF32_OPTION_ALLOW_OUT_OF_RANGE_UNICODE SPIO_NEXT_BIT(SPIO_DECODE_UTF8_TO_UTF32_OPTION_ALLOW_NON_SHORTEST_FORM_NUL)
extern spio_t_error_code spio_decode_utf8_to_utf32(spio_t_utf8 const *from_utf8, size_t from_utf8_count, spio_t_utf32 *to_utf32, size_t *pto_utf32_count, spio_t_bits options);



#define SPIO_IS_LEGAL_UTF8_STRING_OPTION_ALLOW_NON_SHORTEST_FORM_NUL SPIO_BIT(0)
#define SPIO_IS_LEGAL_UTF8_STRING_OPTION_ALLOW_OUT_OF_RANGE_UNICODE SPIO_NEXT_BIT(SPIO_IS_LEGAL_UTF8_STRING_OPTION_ALLOW_NON_SHORTEST_FORM_NUL)
extern spio_t_error_code spio_is_legal_utf8_string(spio_t_utf8 const *source, size_t source_length, spio_t_bits options);
/* 4.3 Convert bytes (i.e. ISO_8859-1) to UTF-8 */
extern spio_t_error_code spio_utf8_from_latin1(char const *latin1_string, size_t latin1_string_size, char **putf8_string, size_t *putf8_string_size);

#endif  /* SPIO_CHAR_CODER_UTF8_H_INCLUDED */
