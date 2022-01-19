/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct all_equal_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  SP_integer stamp;		/* increases up to backtracking */
  int nvars;			/* _original_ #terms */
  int nrefs;			/* _original_ #terms */
  struct bvar reif;
  int *target;
  TAGGED *val;			/* volatile */
  Dvar dvar;
};

#define RefAttr(T) (pdata->refbase + 2*(T) + 2)
#define RefVar(T) (pdata->refbase + 2*(T) + 3)
#define RefAttrReif (pdata->refbase)
#define RefVarReif (pdata->refbase + 1)
#define VAL(T) (pdata->val[T])
#define DVAR(T) (pdata->dvar+(T))

static void SPCDECL all_equal_destructor(void *pdata_v)
{
  struct all_equal_data *pdata = (struct all_equal_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,2*pdata->nvars + 2);
  SP_free(pdata);
}

static int all_equal_tell_pos(Wam wam, struct all_equal_data *pdata, int ntargets);
static int all_equal_tell_neg(Wam wam, struct all_equal_data *pdata, int ntargets);
static int all_equal_ask(Wam wam, struct all_equal_data *pdata, int ntargets);

/*
  '$fd_all_equal_reif'(+State0, +State, -Actions).
  State = state(Vec,Reif,NGround,Handle,Stamp)
*/
void SPCDECL
prolog_fd_all_equal_reif(Wam wam,
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_integer state_stamp, total_size;
  int i, ntargets, nvars=0;
  struct all_equal_data *pdata;
  SP_BOOL committed;
  int elt;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct all_equal_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec);	/* count terms, moving ground terms to RHS */
    total_size =
      nvars*sizeof(int) +
      nvars*sizeof(struct dvar);
    pdata = Palloc(struct all_equal_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (all_equal_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(2*nvars + 2);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);

    DerefArg(tvec,X(0),1);		/* get Vec */
    for (elt=0; elt<nvars; elt++) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(elt));
      dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
      pdata->target[elt] = elt;
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    DerefArg(telt,X(0),2);	/* get Reif */
    fd_get_var_and_attr(telt,RefAttrReif);
    bvar_init(&pdata->reif, RefAttrReif, RefVarReif);
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  pdata->val = Malloc(nvars,TAGGED);
  DerefArg(telt,X(0),3);	/* get NGround */
  ntargets = nvars-GetSmall_int(telt);
  pdata->stamp = state_stamp+1;
  bvar_refresh(&pdata->reif);
  for (i=0; i<ntargets; i++) {
    elt = pdata->target[i];
    dvar_refresh(DVAR(elt));
  }
  if (pdata->reif.min==TaggedOne) { /* TELL all_equal */
    ent = all_equal_tell_pos(wam, pdata, ntargets);
  } else if (pdata->reif.max==TaggedZero) { /* TELL not all_equal */
    ent = all_equal_tell_neg(wam, pdata, ntargets);
  } else { 			/* ASK not all_equal */
    ent = all_equal_ask(wam, pdata, ntargets);
  }
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}

static int all_equal_tell_pos(Wam wam, struct all_equal_data *pdata, int ntargets)
{
  int nvars = pdata->nvars;
  int ent = 1;
  int i;
  
  /* partition the targets */
  for (i=ntargets-1; i>=0; --i) {
    int ti = pdata->target[i];
    if (dvar_is_integer(DVAR(ti))) {
      pdata->target[i] = pdata->target[ntargets-1];
      pdata->target[ntargets-1] = ti;
      ntargets--;
    }
  } 

  if (ntargets < nvars) {	/* fix value for all */
    TAGGED value = dvar_min_t(DVAR(pdata->target[ntargets]));
    for (i=ntargets+1; i<nvars; i++) {
      if (dvar_min_t(DVAR(pdata->target[i])) != value)
	return -1;
    }
    for (i=0; i<ntargets; i++)
      if (dvar_fix_value_t(DVAR(pdata->target[i]), value) < 0)
	return -1;
  } else {			/* pairwise equal in two passes */
    for (i=1; i<nvars; i++) {
      int x = pdata->target[i-1];
      int y = pdata->target[i];
      if (dvar_fix_set(DVAR(y), dvar_set(DVAR(x))) < 0)
	return -1;
    }
    for (i=nvars-1; i>=1; --i) {
      int x = pdata->target[i];
      int y = pdata->target[i-1];
      if (dvar_fix_set(DVAR(y), dvar_set(DVAR(x))) < 0)
	return -1;
    }
    for (i=0; i<nvars && ent==1; i++) {
      int y = pdata->target[i];
      if (!dvar_is_integer(DVAR(y)))
	ent = 0;
    }
  }
  for (i=0; i<ntargets; i++) {
    Dvar dv = DVAR(pdata->target[i]);
    dvar_pruning_done(dv);
  }
  for (i=0; i<ntargets; i++) {
    Dvar dv = DVAR(pdata->target[i]);
    dvar_export(dv);
  }
  CTagToArg(X(0),3) = MakeSmall(nvars-ntargets); /* update NGround */
  return ent;
}

static int all_equal_tell_neg(Wam wam, struct all_equal_data *pdata, int ntargets)
{
  int nvars = pdata->nvars;
  int ent = 1;
  int i;
  
  /* partition the targets */
  for (i=ntargets-1; i>=0; --i) {
    int ti = pdata->target[i];
    if (dvar_is_integer(DVAR(ti))) {
      pdata->target[i] = pdata->target[ntargets-1];
      pdata->target[ntargets-1] = ti;
      ntargets--;
    }
  } 

  if (ntargets < nvars) {	/* compare fixed values */
    TAGGED value = dvar_min_t(DVAR(pdata->target[ntargets]));
    for (i=ntargets+1; i<nvars; i++) {
      if (dvar_min_t(DVAR(pdata->target[i])) != value)
	return 1;
    }
    if (ntargets==0) {
      return -1;
    } else if (ntargets==1) {
      if (dvar_prune_value_t(DVAR(pdata->target[0]), value) < 0)
	return -1;
    } else {
      ent = 0;
    }
  } else {
    if (ntargets==0) {
      return -1;
    } else if (ntargets==1) {
      return 1;
    } else {
      ent = 0;
    }
  }
  for (i=0; i<ntargets; i++) {
    Dvar dv = DVAR(pdata->target[i]);
    dvar_pruning_done(dv);
  }
  for (i=0; i<ntargets; i++) {
    Dvar dv = DVAR(pdata->target[i]);
    dvar_export(dv);
  }
  CTagToArg(X(0),3) = MakeSmall(nvars-ntargets); /* update NGround */
  return ent;
}

static int all_equal_ask(Wam wam, struct all_equal_data *pdata, int ntargets)
{
  int nvars = pdata->nvars;
  int i;
  
  /* partition the targets */
  for (i=ntargets-1; i>=0; --i) {
    int ti = pdata->target[i];
    if (dvar_is_integer(DVAR(ti))) {
      pdata->target[i] = pdata->target[ntargets-1];
      pdata->target[ntargets-1] = ti;
      ntargets--;
    }
  } 

  if (ntargets < nvars) {	/* compare fixed values */
    TAGGED value = dvar_min_t(DVAR(pdata->target[ntargets]));
    for (i=ntargets+1; i<nvars; i++) {
      if (dvar_min_t(DVAR(pdata->target[i])) != value)
	goto reif0;
    }
    if (ntargets==0)
      goto reif1;
    for (i=0; i<ntargets; i++)
      if (!dvar_contains_value_t(DVAR(pdata->target[i]), value))
	goto reif0;
    /* value occurs in all domains now */
  } else if (ntargets==0 || ntargets==1) {
      goto reif1;
  } else {
    TAGGED inters = dvar_set(DVAR(pdata->target[0]));
    for (i=1; i<ntargets && inters != EmptySet; i++)
      inters = fd_intersection(wam, inters, dvar_set(DVAR(pdata->target[i])));
    if (inters == EmptySet)
      goto reif0;
    /* nonempty domain intersection */
  }
  CTagToArg(X(0),3) = MakeSmall(nvars-ntargets); /* update NGround */
  return 0;
 reif0:
  bvar_export_value(wam, &pdata->reif, 0);
  return 1;
 reif1:
  bvar_export_value(wam, &pdata->reif, 1);
  return 1;
}

