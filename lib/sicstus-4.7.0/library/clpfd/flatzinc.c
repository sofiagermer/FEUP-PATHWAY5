#include "fd.h"
/* A super-simple FlatZinc tokenizer. */

enum token {
  TOKEN_WORD=1,
  TOKEN_INT,
  TOKEN_FLOAT,
  TOKEN_SEMICOLON,
  TOKEN_COMMA,
  TOKEN_EQUAL,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_LCURLY,
  TOKEN_RCURLY,
  TOKEN_LBRACKET,
  TOKEN_RBRACKET,
  TOKEN_ANNOTATION,
  TOKEN_TYPE_DECL,
  TOKEN_RANGE,
  TOKEN_ERROR,
  TOKEN_EOF,
  TOKEN_STRING,
};

struct tokenizer {
  SP_stream *stream;
  int current_char;
  char *string;
  size_t string_fill;
  size_t string_size;
};

#define GET(tk) (tk)->current_char = SP_get_code((tk)->stream)
#define UNGET(tk,c) {SP_unget_code((tk)->stream, (tk)->current_char); (tk)->current_char = (c);}

/* xref Emulator/widechar.c */
#define isspace(i) ((i) >= 0 && (i) <= 32)
#define isalphanum(i)  ( isdigit(i) || ((i) >= 'a' && (i) <= 'z') || ((i) >= 'A' && (i) <= 'Z') || ((i) == '_') )
#define isdigit(i) ((i) >= '0' && (i) <= '9')

static void pushchar(Wam wam,
		     struct tokenizer *tk)
{
  if (tk->string_size==0) {
    size_t size = 64;
    tk->string_size = size;
    tk->string = (char *)SP_malloc(size);
  } else if (tk->string_fill == tk->string_size) {
    size_t size = tk->string_size * 2;
    tk->string_size = size;
    tk->string = (char *)SP_realloc(tk->string, size);
  }
  tk->string[tk->string_fill++] = (char)tk->current_char;
}

static enum token fzn_read_token(Wam wam,
				 void *v_stream,
				 SP_integer look_ahead0,
				 SP_term_ref value,
				 SP_integer *look_ahead)
{
  struct tokenizer tkstruct;
  struct tokenizer *tk = &tkstruct;

  tk->stream = v_stream;
  tk->string_size = 0;
  tk->string_fill = 0;
  *look_ahead = -2;
  if (look_ahead0 != -2) {
    tk->current_char = (int)look_ahead0;
  } else {
    GET(tk);
  }
  
  while (TRUE) {
    if (isspace(tk->current_char)) {
      GET(tk);
    } else if (tk->current_char == '%') {
      while (tk->current_char != '\n')
	GET(tk);
    } else {
      break;
    }
  }
  if (isdigit(tk->current_char) || tk->current_char=='+' || tk->current_char=='-') {
    int factor = 1;
    int f = 0;
    SP_integer accum = 0; 

    if (tk->current_char == '-') {
      GET(tk);
      factor = -1;
    } else if (tk->current_char == '+') {
      GET(tk);
    }

    while (isdigit(tk->current_char)) {
      accum = 10*accum + tk->current_char - '0';
      GET(tk);
    }
    if (tk->current_char == '.') {
      GET(tk);
      if (!isdigit(tk->current_char)) {
	UNGET(tk, '.');
      } else {
	f=1;
	while (isdigit(tk->current_char)) {
	  GET(tk);
	}
      }
    }
    if (tk->current_char == 'e' || tk->current_char == 'E') {
      f=1;
      GET(tk);
      while (isdigit(tk->current_char) || tk->current_char=='+' || tk->current_char=='-') {
	GET(tk);
      }
    }
    *look_ahead = tk->current_char;
    if (f==1)
      return TOKEN_FLOAT;
    RefTerm(value) = MakeSmall(factor*accum);
    return TOKEN_INT;
  }
  if (isalphanum(tk->current_char)) {
    while (isalphanum(tk->current_char)) {
      pushchar(SPAPI_ARG tk); GET(tk);
    }
    *look_ahead = tk->current_char;
    tk->current_char = 0;
    pushchar(SPAPI_ARG tk);
    SP_put_string(value, tk->string);
    SP_free(tk->string);
    return TOKEN_WORD;
  }
  if (tk->current_char == '\'') {
    GET(tk);
    while (tk->current_char != '\'') {
      pushchar(SPAPI_ARG tk); GET(tk);
    }
    tk->current_char = 0;
    pushchar(SPAPI_ARG tk);
    tk->current_char = '\'';
    SP_put_string(value, tk->string);
    SP_free(tk->string);
    return TOKEN_WORD;
  }
  if (tk->current_char == '"') {
    GET(tk);
    while (tk->current_char != '"') {
      pushchar(SPAPI_ARG tk); GET(tk);
    }
    tk->current_char = 0;
    pushchar(SPAPI_ARG tk);
    tk->current_char = '"';
    SP_put_string(value, tk->string);
    SP_free(tk->string);
    return TOKEN_STRING;
  }
  if (tk->current_char==';')
    return TOKEN_SEMICOLON;
  if (tk->current_char==',')
    return TOKEN_COMMA;
  if (tk->current_char=='=')
    return TOKEN_EQUAL;
  if (tk->current_char=='(')
    return TOKEN_LPAREN;
  if (tk->current_char==')')
    return TOKEN_RPAREN;
  if (tk->current_char=='{')
    return TOKEN_LCURLY;
  if (tk->current_char=='}')
    return TOKEN_RCURLY;
  if (tk->current_char=='[')
    return TOKEN_LBRACKET;
  if (tk->current_char==']')
    return TOKEN_RBRACKET;
  if (tk->current_char==':') {
    GET(tk);
    if (tk->current_char==':')
      return TOKEN_ANNOTATION;
    *look_ahead = tk->current_char;
    return TOKEN_TYPE_DECL;
  }
  if (tk->current_char=='.') {
    GET(tk);
    if (tk->current_char=='.')
      return TOKEN_RANGE;
    *look_ahead = tk->current_char;
    return TOKEN_ERROR;
  }
  if (tk->current_char==-1)
    return TOKEN_EOF;
  return TOKEN_ERROR;
}

/*** experimental flatzinc parser ***/

struct token_buffer {
  enum token token;
  SP_term_ref ref;
  void *v_stream;
  TAGGED term;
  SP_integer look_ahead;
};

static TAGGED  fzn_read_item(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_curly_set_literal(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_constraint_elem(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_solve_kind(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_annotations(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_annotation(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_annotation_array(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_flat_expr_array(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_ann_expr(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_flat_expr(Wam wam, struct token_buffer *current, int live);
static TAGGED  fzn_flat_expr_after_id(Wam wam, struct token_buffer *current, TAGGED ident, int live);
static TAGGED  fzn_flat_expr_not_after_id(Wam wam, struct token_buffer *current, int live);
static void    fzn_get_token(Wam wam, struct token_buffer *current);
static SP_BOOL fzn_consume(Wam wam, struct token_buffer *current, enum token token);
static SP_BOOL fzn_consume_word(Wam wam, struct token_buffer *current, SP_atom atom);
static TAGGED  sp_cons_list(Wam wam, int a1, int a2, int live);
static TAGGED  sp_cons_structure1(Wam wam, SP_atom f, int a1, int live);
static TAGGED  sp_cons_structure2(Wam wam, SP_atom f, int a1, int a2, int live);
static TAGGED  sp_cons_structure5(Wam wam, SP_atom f, int a1, int a2, int a3, int a4, int a5, int live);
static TAGGED  sp_cons_univ(Wam wam, SP_atom f, int ar, int list, int live);

static TAGGED fzn_read_item(Wam wam, struct token_buffer *current, int live)
{
  SP_BOOL seen_set = FALSE;
  if (fzn_consume_word(wam, current, atom_constraint)) {
    X(live+0) = fzn_constraint_elem(wam, current, live+0);
    if (X(live+0) == ERRORTAG)
      return ERRORTAG;
    X(live+1) = fzn_annotations(wam, current, live+1);
    return sp_cons_structure2(wam, atom_constraint, live+0, live+1, live+2);
  } else if (fzn_consume_word(wam, current, atom_solve)) {
    X(live+0) = fzn_annotations(wam, current, live+0);
    X(live+1) = fzn_solve_kind(wam, current, live+1);
    if (X(live+1) == ERRORTAG)
      return ERRORTAG;
    return sp_cons_structure2(wam, atom_solve, live+0, live+1, live+2);
  } else {
    X(live+0) = atom_par;
    if (fzn_consume_word(wam, current, atom_array)) {
      int a = atom_pararray;
      
      if (!fzn_consume(wam, current, TOKEN_LBRACKET))
	return ERRORTAG;
      X(live+0) = RefTerm(current->ref);
      if (!fzn_consume(wam, current, TOKEN_INT) ||
	  !fzn_consume(wam, current, TOKEN_RANGE))
	return ERRORTAG;
      X(live+1) = RefTerm(current->ref);
      if (!fzn_consume(wam, current, TOKEN_INT) ||
	  !fzn_consume(wam, current, TOKEN_RBRACKET) ||
	  !fzn_consume_word(wam, current, atom_of))
	return ERRORTAG;
      if (fzn_consume_word(wam, current, atom_var))
	a = atom_vararray;
      X(live+0) = sp_cons_structure2(wam, a, live+0, live+1, live+2);
      if (X(live+0) == ERRORTAG)
	return ERRORTAG;
    } else if (fzn_consume_word(wam, current, atom_var)) {
      X(live+0) = atom_var;
    }
    if (fzn_consume_word(wam, current, atom_set)) {
      seen_set = TRUE;
      if (!fzn_consume_word(wam, current, atom_of))
	return ERRORTAG;
    }
    if (current->token == TOKEN_WORD) {
      X(live+1) = RefTerm(current->ref);
      fzn_consume(wam, current, TOKEN_WORD);
    } else if (current->token == TOKEN_INT) {
      X(live+1) = RefTerm(current->ref);
      fzn_consume(wam, current, TOKEN_INT);
      if (!fzn_consume(wam, current, TOKEN_RANGE))
	return ERRORTAG;
      X(live+2) = RefTerm(current->ref);
      if (!fzn_consume(wam, current, TOKEN_INT))
	return ERRORTAG;
      X(live+1) = sp_cons_structure2(wam, atom_range, live+1, live+2, live+3);
      if (X(live+1) == ERRORTAG)
	return ERRORTAG;
    } else if (fzn_consume(wam, current, TOKEN_LCURLY)) {
      X(live+1) = fzn_curly_set_literal(wam, current, live+1);
      if (X(live+1) == ERRORTAG)
	return ERRORTAG;
    } else {
      return ERRORTAG;
    }
    if (seen_set) {
      X(live+1) = sp_cons_structure1(wam, atom_set, live+1, live+2);
    }
    if (!fzn_consume(wam, current, TOKEN_TYPE_DECL))
      return ERRORTAG;
    X(live+2) = RefTerm(current->ref);
    if (!fzn_consume(wam, current, TOKEN_WORD))
      return ERRORTAG;
    X(live+3) = fzn_annotations(wam, current, live+3);
    X(live+4) = atom_nil;
    if (fzn_consume(wam, current, TOKEN_EQUAL)) {
      X(live+4) = fzn_flat_expr(wam, current, live+4);
      X(live+5) = atom_nil;
      X(live+4) = sp_cons_list(wam, live+4, live+5, live+6);
    }
    return sp_cons_structure5(wam, atom_decl, live+0, live+1, live+2, live+3, live+4, live+5);
  }
}

static TAGGED fzn_curly_set_literal(Wam wam, struct token_buffer *current, int live)
{
  X(live+0) = RefTerm(current->ref);
  X(live+1) = atom_nil;
  if (!fzn_consume(wam, current, TOKEN_INT))
    return ERRORTAG;
  X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
  while (fzn_consume(wam, current, TOKEN_COMMA)) {
    X(live+2) = RefTerm(current->ref);
    X(live+3) = atom_nil;
    if (!fzn_consume(wam, current, TOKEN_INT))
      return ERRORTAG;
    X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
    X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
  }
  if (!fzn_consume(wam, current, TOKEN_RCURLY))
    return ERRORTAG;
  return sp_cons_structure1(wam, atom_curly, live+0, live+1);
}

static TAGGED fzn_constraint_elem(Wam wam, struct token_buffer *current, int live)
{
  SP_atom a = RefTerm(current->ref);
  
  if (!fzn_consume(wam, current, TOKEN_WORD))
    return ERRORTAG;
  if (fzn_consume(wam, current, TOKEN_LPAREN)) {
    int ar = 1;
    X(live+0) = fzn_flat_expr(wam, current, live);
    X(live+1) = atom_nil;
    X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
    while (fzn_consume(wam, current, TOKEN_COMMA)) {
      ar++;
      X(live+2) = fzn_flat_expr(wam, current, live+2);
      X(live+3) = atom_nil;
      X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
      X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
    }
    if (!fzn_consume(wam, current, TOKEN_RPAREN))
      return ERRORTAG;
    X(live+0) = sp_cons_univ(wam, a, ar, live+0, live+1);
  } else if (fzn_consume(wam, current, TOKEN_LBRACKET)) {
    X(live+0) = a;
    X(live+1) = RefTerm(current->ref);
    if (!fzn_consume(wam, current, TOKEN_INT) && !fzn_consume(wam, current, TOKEN_WORD))
      return ERRORTAG;
    X(live+0) = sp_cons_structure2(wam, atom_subscript, live+0, live+1, live+2);
    if (!fzn_consume(wam, current, TOKEN_RBRACKET))
      return ERRORTAG;
  } else {
    X(live+0) = a;
  }
  return X(live+0);
}

static TAGGED fzn_solve_kind(Wam wam, struct token_buffer *current, int live)
{
  SP_atom a = RefTerm(current->ref);
  
  if (!fzn_consume(wam, current, TOKEN_WORD))
    return ERRORTAG;
  if (current->token != TOKEN_SEMICOLON) {
    X(live+0) = fzn_constraint_elem(wam, current, live);
    if (X(live+0) == ERRORTAG)
      return ERRORTAG;
    return sp_cons_structure1(wam, a, live+0, live+1);
  } else {
    return a;
  }
}
    
static TAGGED fzn_annotations(Wam wam, struct token_buffer *current, int live)
{
  if (!fzn_consume(wam, current, TOKEN_ANNOTATION))
    return atom_nil;
  fzn_consume(wam, current, TOKEN_ANNOTATION);
  X(live+0) = fzn_annotation(wam, current, live);
  if (X(live+0) == ERRORTAG)
    return ERRORTAG;
  X(live+1) = atom_nil;
  X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
  while (fzn_consume(wam, current, TOKEN_ANNOTATION)) {
    X(live+2) = fzn_annotation(wam, current, live+2);
    if (X(live+2) == ERRORTAG)
      return ERRORTAG;
    X(live+3) = atom_nil;
    X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
    X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
  }
  return X(live+0);
}

static TAGGED fzn_annotation(Wam wam, struct token_buffer *current, int live)
{
  if (fzn_consume_word(wam, current, atom_seq_search)) {
    if (!fzn_consume(wam, current, TOKEN_LPAREN))
      return ERRORTAG;
    if (!fzn_consume(wam, current, TOKEN_LBRACKET))
      return ERRORTAG;
    X(live+0) = fzn_annotation_array(wam, current, live);
    if (X(live+0) == ERRORTAG)
      return ERRORTAG;
    if (!fzn_consume(wam, current, TOKEN_RPAREN))
      return ERRORTAG;
    return sp_cons_structure1(wam, atom_seq_search, live+0, live+1);
  } else {
    TAGGED ident = RefTerm(current->ref);
    int ar = 1;
    if (!fzn_consume(wam, current, TOKEN_WORD))
      return ERRORTAG;
    if (!fzn_consume(wam, current, TOKEN_LPAREN))
      return ident;
    X(live+0) = fzn_ann_expr(wam, current, live);
    if (X(live+0) == ERRORTAG)
      return ERRORTAG;
    X(live+1) = atom_nil;
    X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
    while (fzn_consume(wam, current, TOKEN_COMMA)) {
      ar++;
      X(live+2) = fzn_ann_expr(wam, current, live+2);
      if (X(live+2) == ERRORTAG)
	return ERRORTAG;
      X(live+3) = atom_nil;
      X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
      X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
    }
    if (!fzn_consume(wam, current, TOKEN_RPAREN))
      return ERRORTAG;
    return sp_cons_univ(wam, ident, ar, live+0, live+1);
  }
}

static TAGGED fzn_annotation_array(Wam wam, struct token_buffer *current, int live)
{
  if (fzn_consume(wam, current, TOKEN_RBRACKET))
    return atom_nil;
  X(live+0) = fzn_annotation(wam, current, live);
  if (X(live+0) == ERRORTAG)
    return ERRORTAG;
  X(live+1) = atom_nil;
  X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
  while (fzn_consume(wam, current, TOKEN_COMMA)) {
    X(live+2) = fzn_annotation(wam, current, live+2);
    if (X(live+2) == ERRORTAG)
      return ERRORTAG;
    X(live+3) = atom_nil;
    X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
    X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
  }
  if (!fzn_consume(wam, current, TOKEN_RBRACKET))
    return ERRORTAG;
  return X(live+0);
}

static TAGGED fzn_flat_expr_array(Wam wam, struct token_buffer *current, int live)
{
  if (fzn_consume(wam, current, TOKEN_RBRACKET))
    return atom_nil;
  X(live+0) = fzn_flat_expr(wam, current, live);
  X(live+1) = atom_nil;
  X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
  while (fzn_consume(wam, current, TOKEN_COMMA)) {
    X(live+2) = fzn_flat_expr(wam, current, live+2);
    X(live+3) = atom_nil;
    X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
    X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
  }
  if (!fzn_consume(wam, current, TOKEN_RBRACKET))
    return ERRORTAG;
  return X(live+0);
}

static TAGGED fzn_ann_expr(Wam wam, struct token_buffer *current, int live)
{
  TAGGED ident = RefTerm(current->ref);
  if (fzn_consume(wam, current, TOKEN_WORD)) {
    if (fzn_consume(wam, current, TOKEN_LPAREN)) {
      int ar = 1;
      X(live+0) = fzn_ann_expr(wam, current, live);
      if (X(live+0) == ERRORTAG)
	return ERRORTAG;
      X(live+1) = atom_nil;
      X(live+1) = X(live+0) = sp_cons_list(wam, live+0, live+1, live+2);
      while (fzn_consume(wam, current, TOKEN_COMMA)) {
	ar++;
	X(live+2) = fzn_ann_expr(wam, current, live+2);
	if (X(live+2) == ERRORTAG)
	  return ERRORTAG;
	X(live+3) = atom_nil;
	X(live+2) = sp_cons_list(wam, live+2, live+3, live+4);
	X(live+1) = CTagToCdr(X(live+1)) = X(live+2);
      }
      if (!fzn_consume(wam, current, TOKEN_RPAREN))
	return ERRORTAG;
      return sp_cons_univ(wam, ident, ar, live+0, live+1);
    } else {
      return fzn_flat_expr_after_id(wam, current, ident, live);
    }
  } else {
    return fzn_flat_expr_not_after_id(wam, current, live);
  }
}

static TAGGED fzn_flat_expr(Wam wam, struct token_buffer *current, int live)
{
  TAGGED ident = RefTerm(current->ref);
  if (fzn_consume(wam, current, TOKEN_WORD)) {
    return fzn_flat_expr_after_id(wam, current, ident, live);
  } else {
    return fzn_flat_expr_not_after_id(wam, current, live);
  }  
}

static TAGGED fzn_flat_expr_after_id(Wam wam, struct token_buffer *current, TAGGED ident, int live)
{
  if (fzn_consume(wam, current, TOKEN_LBRACKET)) {
    X(live+0) = ident;
    X(live+1) = RefTerm(current->ref);
    if ((fzn_consume(wam, current, TOKEN_WORD) || fzn_consume(wam, current, TOKEN_INT)) &&
	fzn_consume(wam, current, TOKEN_RBRACKET)) {
      return sp_cons_structure2(wam, atom_subscript, live+0, live+1, live+2);
    } else {
      return ERRORTAG;
    }
  } else {
    return ident;
  }
}

static TAGGED fzn_flat_expr_not_after_id(Wam wam, struct token_buffer *current, int live)
{
  X(live+0) = RefTerm(current->ref);
  if (fzn_consume(wam, current, TOKEN_LBRACKET)) {
    X(live+0) = fzn_flat_expr_array(wam, current, live);
    if (X(live+0) == ERRORTAG)
      return ERRORTAG;
    X(live+1) = atom_nil;
    return sp_cons_list(wam, live+0, live+1, live+2); /* Prolog code expects an extra [] wrapper. */
  } else if (fzn_consume(wam, current, TOKEN_LCURLY)) {
    return fzn_curly_set_literal(wam, current, live);
  } else if (fzn_consume(wam, current, TOKEN_FLOAT)) {
    return TaggedZero;
  } else if (fzn_consume(wam, current, TOKEN_STRING)) {
    return X(live+0);
  } else if (fzn_consume(wam, current, TOKEN_INT)) {
    if (fzn_consume(wam, current, TOKEN_RANGE)) {
      X(live+1) = RefTerm(current->ref);
      if (!fzn_consume(wam, current, TOKEN_INT))
	return ERRORTAG;
      return sp_cons_structure2(wam, atom_range, live+0, live+1, live+2);
    } else {
      return X(live+0);
    }
  } else {
    return ERRORTAG;
  }
}

static void fzn_get_token(Wam wam, struct token_buffer *current)
{
  current->token =
    fzn_read_token(wam, current->v_stream, current->look_ahead, current->ref, &current->look_ahead);
}

static SP_BOOL fzn_consume(Wam wam, struct token_buffer *current, enum token token)
{
  if (current->token == token) {
    fzn_get_token(wam, current);
    return TRUE;
  } else {
    return FALSE;
  }
}

static SP_BOOL fzn_consume_word(Wam wam, struct token_buffer *current, SP_atom atom)
{
  if (current->token == TOKEN_WORD && RefTerm(current->ref) == atom) {
    fzn_get_token(wam, current);
    return TRUE;
  } else {
    return FALSE;
  }
}

static TAGGED sp_cons_list(Wam wam, int a1, int a2, int live)
{
  TAGGED *h;

  FdMemRequireHeap(2, live);
  h = w->global_top;
  *h++ = X(a1);
  *h++ = X(a2);
  w->global_top = h;
  return MakeList(h-2);
}

static TAGGED sp_cons_structure1(Wam wam, SP_atom f, int a1, int live)
{
  TAGGED *h;

  FdMemRequireHeap(2, live);
  h = w->global_top;
  *h++ = SetArity(f,1);
  *h++ = X(a1);
  w->global_top = h;
  return MakeStructure(h-2);
}

static TAGGED sp_cons_structure2(Wam wam, SP_atom f, int a1, int a2, int live)
{
  TAGGED fun = SetArity(f,2);
  TAGGED *h;

  if (fun == functor_Dmutable || fun == functor_Darray1d)
    return ERRORTAG;
  FdMemRequireHeap(3, live);
  h = w->global_top;
  *h++ = fun;
  *h++ = X(a1);
  *h++ = X(a2);
  w->global_top = h;
  return MakeStructure(h-3);
}

static TAGGED sp_cons_structure5(Wam wam, SP_atom f, int a1, int a2, int a3, int a4, int a5, int live)
{
  TAGGED *h;

  FdMemRequireHeap(6, live);
  h = w->global_top;
  *h++ = SetArity(f,5);
  *h++ = X(a1);
  *h++ = X(a2);
  *h++ = X(a3);
  *h++ = X(a4);
  *h++ = X(a5);
  w->global_top = h;
  return MakeStructure(h-6);
}

static TAGGED sp_cons_univ(Wam wam, SP_atom f, int ar, int list, int live)
{
  TAGGED fun = SetArity(f,ar);
  TAGGED *h;

  if (fun == functor_Dmutable || fun == functor_Darray1d)
    return ERRORTAG;
  FdMemRequireHeap(ar+1, live);
  h = w->global_top;
  *h++ = fun;
  while (TagIsLST(X(list))) {
    *h++ = CTagToCar(X(list));
    X(list) = CTagToCdr(X(list));
  }
  w->global_top = h;
  return MakeStructure(h-(ar+1));
}

// fzn_read_token(+StreamCode, +LookAhead0, [-Token], -Value, -LookAhead)
// -2 means no lookahead
SP_integer SPCDECL
prolog_fzn_read_token(Wam wam, 
		      void *v_stream,
		      SP_integer look_ahead0,
		      SP_term_ref value,
		      SP_integer *look_ahead) {
  return (SP_integer) fzn_read_token(wam, v_stream, look_ahead0, value, look_ahead);
}

// fzn_read_item(+StreamCode, -Item, [-OK])
SP_integer SPCDECL
prolog_fzn_read_item(Wam wam,
		     void *v_stream,
		     SP_term_ref item)
{
  struct token_buffer current_struct;
  struct token_buffer *current = &current_struct;

  current->v_stream = v_stream;
  current->ref = SP_new_term_ref();
  current->look_ahead = -2;
  
  fzn_get_token(wam, current);
 start:
  if (fzn_consume_word(wam, current, atom_predicate)) {
    while (!fzn_consume(wam, current, TOKEN_SEMICOLON))
      fzn_get_token(wam, current);
    goto start;
  } else if (current->token == TOKEN_EOF) {
    RefTerm(item) = MakeSmall(-1);
    return 1;
  } else {
    TAGGED t = fzn_read_item(wam, current, 0);
    if (t == ERRORTAG)
      return 0;
    RefTerm(item) = t;
    return current->token == TOKEN_SEMICOLON;
  }
}
