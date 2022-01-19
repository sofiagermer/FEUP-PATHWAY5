/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

struct fd_findall {
  int arity;
  int pop;
  int size;
  TAGGED *buf;
};

void SPCDECL
prolog_fd_findall_begin(Wam wam,
			SP_term_ref RootRef,
			SP_term_ref TemplateRef) {
  struct fd_findall *root = SP_malloc(sizeof(struct fd_findall));
  TAGGED template = RefTerm(TemplateRef);

  DEREF(template,template);
  root->arity = fd_list_length(template);
  root->pop = 0;
  root->size = 1;
  root->buf = SP_malloc(root->arity * sizeof(TAGGED));
  RefTerm(RootRef) = PointerToTerm(root);
}

void SPCDECL
prolog_fd_findall_end(Wam wam,
		      SP_term_ref RootRef) {
  TAGGED tptr;
  struct fd_findall *root;
  
  DEREF(tptr, RefTerm(RootRef));
  root = (struct fd_findall *)TermToPointer(tptr);

  SP_free(root->buf);
  SP_free(root);
}

SP_integer SPCDECL
prolog_fd_findall_inserta(Wam wam,
			  SP_term_ref RootRef,
			  SP_term_ref TemplateRef) {
  TAGGED tptr;
  struct fd_findall *root;
  TAGGED *p;
  int i;
  int arity;
  TAGGED template;

  
  DEREF(tptr, RefTerm(RootRef));
  DEREF(template, RefTerm(TemplateRef));
  root = (struct fd_findall *)TermToPointer(tptr);
  arity = root->arity;
  if (root->pop == root->size) {
    root->size *= 2;
    root->buf = SP_realloc(root->buf, root->size * arity * sizeof(TAGGED));
  }
  p = root->buf + arity * root->pop;
  for (i=0; i<arity; i++) {
    TAGGED elt;
    DerefCar(elt,template);
    DerefCdr(template,template);
    if (!TagIsSmall(elt))
      return 1;
    *p++ = elt;
  }
  root->pop++;
  return 0;
}

void SPCDECL
prolog_fd_findall_list(Wam wam,
		       SP_term_ref RootRef,
		       SP_term_ref SolnsRef) {
  TAGGED tptr;
  struct fd_findall *root;
  TAGGED *h, *h2;
  int i;
  int arity;
  int pop;
  
  DEREF(tptr, RefTerm(RootRef));
  root = (struct fd_findall *)TermToPointer(tptr);
  arity = root->arity;
  pop = root->pop;

  if (!pop)
    return;
  FdMemRequireHeap(2*pop*arity + 2*pop, 0);
  h = w->global_top;

  for (i=0; i<pop*arity; i++) {
    h[2*i] = root->buf[i];
    if ((i+1) % arity)
      h[2*i + 1] = MakeList(&h[2*i + 2]);
    else
      h[2*i + 1] = EmptySet;
  }
  h2 = h + 2*pop*arity;	/* base for list of rows */
  RefTerm(SolnsRef) = MakeList(h2);
  for (i=0; i<pop; i++) {
    h2[2*i] = arity>0 ? MakeList(&h[2*i*arity]) : EmptySet;
    if ((i+1) < pop)
      h2[2*i + 1] = MakeList(&h2[2*i + 2]);
    else
      h2[2*i + 1] = EmptySet;
  }
  h = h2 + 2*pop;
  w->global_top = h;
}
