/* Copyright(C) 1999, Swedish Institute of Computer Science */
/*** 
Graeme Gange, Peter J. Stuckey,
Sequential Precede Chain for Value Symmetry Elimination. CP 2018: 144-159
Generalized to ValuePrecede Chain by Mats Carlsson
***/

#include "fd.h"
#include "dvars.h"

#define SV(T) (pdata->target[T])
#define VAR(V) (pdata->dvar+(V))

enum trail {
  FIRST=1,
  LIMIT
};

struct value_precede_chain_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  SP_integer stamp;
  int nvals;
  int nvars;
  int max_val;			/* largest feasible value */
  Dvar dvar;
  TAGGED *values;	    /* [nvals], values */
  TAGGED *succ;		    /* [(nvals+1)*nvals], for each value k: sorted list of its successors */
  int *target;		    /* [nvars], "active" var */
  int *first;		    /* [k] = first possible occurrence of k */
  int *limit;               /* [k] = last possible occurrence of k, or nvars, if it can not occur */
  int *first_val;           /* [ii] = the k for which this is a first occurrence */
  int *limit_val;           /* [ii] = the k for which this is a limit occurrence */
  int *trail;
  int *ttop;
  int *tend;
  unsigned char *buffer;
};

static void SPCDECL value_precede_chain_destructor(void *pdata_v)
{
  struct value_precede_chain_data *pdata = (struct value_precede_chain_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->refbase)
    SP_free_globrefs(pdata->refbase,2*pdata->nvars);
  if (pdata->trail)
    SP_free(pdata->trail);
  SP_free(pdata);
}

static void
trail_expand(Wam wam,
	     struct value_precede_chain_data *pdata)
{
  SP_integer inuse   = pdata->ttop - pdata->trail;
  SP_integer oldsize = pdata->tend - pdata->trail;
  
  pdata->trail = SP_realloc(pdata->trail, 2*oldsize*sizeof(int));
  pdata->ttop = pdata->trail + inuse;
  pdata->tend = pdata->trail + 2*oldsize;
}

static void
trail(Wam wam,
      struct value_precede_chain_data *pdata,
      enum trail arr, int k, int ii) {
  int offset1, offset2;
  if (pdata->ttop+4 > pdata->tend)
    trail_expand(wam,pdata);
  if (arr==FIRST) {
    offset1 = k;
    offset2 = (int)(pdata->first_val - pdata->first) + ii;
  } else {
    offset1 = (int)(pdata->limit - pdata->first) + k;
    offset2 = (int)(pdata->limit_val - pdata->first) + ii;
  }
  pdata->ttop[0] = offset1;
  pdata->ttop[1] = pdata->first[offset1];
  pdata->ttop[2] = offset2;
  pdata->ttop[3] = pdata->first[offset2];
  pdata->ttop += 4;
  pdata->first[offset1] = ii;
  pdata->first[offset2] = k;
}

static void
trail_max_val(Wam wam,
	      struct value_precede_chain_data *pdata,
	      int k) {
  if (pdata->ttop+2 > pdata->tend)
    trail_expand(wam,pdata);
  pdata->ttop[0] = (int)(&pdata->max_val - pdata->first);
  pdata->ttop[1] = pdata->max_val;
  pdata->ttop += 2;
  pdata->max_val = k;
}

static int
mapped_dvar_fix_max_l(Wam wam, struct value_precede_chain_data *pdata, Dvar dv, int k) {
  FDCONS cons;
  TAGGED succ;
  int i;

  if (k > pdata->nvals)
    return TRUE;
  fdcons_init(&cons);
  for (i = k * (pdata->nvals+1);; i++) {
    succ = pdata->succ[i];
    if (succ == atom_nil)
      break;
    fdcons_add(wam, &cons, succ);
  }
  return dvar_prune_set(dv, fdcons_set(&cons));
}

static int
mapped_dvar_fix_value_l(struct value_precede_chain_data *pdata, Dvar dv, int k) {
  return dvar_fix_value_t(dv, pdata->values[k-1]);
}

static int
mapped_dvar_contains_value_l(struct value_precede_chain_data *pdata, Dvar dv, int k) {
  return (k <= pdata->nvals && dvar_contains_value_t(dv, pdata->values[k-1]));
}

static int
mapped_dvar_min_l(Wam wam, struct value_precede_chain_data *pdata, Dvar dv) {
  int i=0, k;
  DVITER it;
  TAGGED x;
  (void)wam;

  if (FDlt(dvar_min_t(dv), pdata->succ[0]) || FDgt(dvar_max_t(dv), pdata->succ[pdata->nvals-1]))
    return 0;			/* dv contains joker */
  
  dviter_init(&it, dv);
  x = dviter_next_value_t(&it);
  while (x) {
    TAGGED v = i < pdata->nvals ? pdata->succ[i] : x+1;
    if (Tlt(x,v)) {
      return 0;
    } else if (Tgt(x,v)) {
      i++;
    } else {
      i++;
      x = dviter_empty(&it) ? 0 : dviter_next_value_t(&it);
    }
  }
    
  /* no joker value -- find first non-joker */

  for (k=1; k<=pdata->nvals; k++)
    if (dvar_contains_value_t(dv, pdata->values[k-1]))
      break;
  return k;
}

static int
mapped_dvar_max_l(struct value_precede_chain_data *pdata, Dvar dv) {
  int k;

  for (k=pdata->nvals; k>=1; --k)
    if (dvar_contains_value_t(dv, pdata->values[k-1]))
      break;
  return k;
}

static SP_BOOL
repair_upper(Wam wam, struct value_precede_chain_data *pdata, int k) {
  unsigned int ii = pdata->first[k];
  unsigned int lim = pdata->limit[k+1];
  int nvars = pdata->nvars;

  if (k >= pdata->max_val)
    return TRUE;
  for(; ii < lim; ii++) {
    if (mapped_dvar_fix_max_l(wam, pdata, VAR(ii), k)<0)
      return FALSE;		/* happens */
    if (mapped_dvar_contains_value_l(pdata, VAR(ii), k)) {
      trail(wam, pdata, FIRST, k, ii);
      if(ii == pdata->limit[k]) /* First and last occurrences coincide */
	if(mapped_dvar_fix_value_l(pdata, VAR(ii), k)<0)
	  return FALSE;		/* doesn't happen? */
      k++;
      if(k == pdata->max_val || ii < pdata->first[k])
      	return TRUE;
      lim = pdata->limit[k+1];
    }
  }
  if (ii < nvars)		/* first occ of k comes too late */
    return FALSE;
  SP_ASSERT(k < pdata->max_val);
  trail_max_val(wam, pdata, k);	/* k is new largest feasible */
  return TRUE;
}

static SP_BOOL
repair_limit(Wam wam, struct value_precede_chain_data *pdata, int k) {
  int ii = pdata->limit[k];
  
  if (ii >= pdata->nvars)	/* can happen if buffer is dirty */
    return TRUE;
  for(; ii >= 0; --ii) {
    if(k < 1 || pdata->limit[k] < ii)
      return TRUE;
    if (mapped_dvar_contains_value_l(pdata, VAR(ii), k)) {
      trail(wam, pdata, LIMIT, k, ii);
      if(ii == pdata->first[k]) /* First and last occurrences coincide */
	if(mapped_dvar_fix_value_l(pdata, VAR(ii), k)<0)
	  return FALSE;		/* doesn't happen? */
      --k;
    }
  }
  return TRUE;
}

static SP_BOOL
warm_propagation(Wam wam, struct value_precede_chain_data *pdata) {
  int k;
  int maxk = pdata->max_val;
  
  for(k = 1; k <= maxk; k++) {
    if((pdata->buffer[k] & 0x1) && !repair_upper(wam, pdata, k))
      return FALSE;
  }
  for(k = maxk; k >= 1; --k) {
    if((pdata->buffer[k] & 0x2) && !repair_limit(wam, pdata, k))
      return FALSE;
  }
  for(k = 1; k <= maxk; k++)
    pdata->buffer[k] = 0;
  
  return TRUE;
}

static SP_BOOL
cold_propagation(Wam wam, struct value_precede_chain_data *pdata) {
  int ii, k = 0;
  int M = 1;
  int nvars = pdata->nvars;
  (void)wam;
  
  for (ii=0; ii<nvars+1; ii++) {
    pdata->first[ii] = 0;
    pdata->limit[ii] = nvars;
    pdata->first_val[ii] = 0;
    pdata->limit_val[ii] = nvars;
  }
  
  for (ii = 0; ii < nvars; ii++) {
    if (mapped_dvar_fix_max_l(wam, pdata, VAR(ii), M)<0)
      return FALSE;
    if (mapped_dvar_contains_value_l(pdata, VAR(ii), M)) {
      pdata->first[M] = ii;
      pdata->first_val[ii] = M++;
    }
  }
  pdata->max_val = M-1;

  for (ii = nvars-1; ii >= 0; --ii) {
    int mx = mapped_dvar_min_l(wam, pdata, VAR(ii));
    if(mx > k) {
      k = mx;
    }
    if(k>0 && mapped_dvar_contains_value_l(pdata, VAR(ii), k)) {
      pdata->limit[k] = ii;
      pdata->limit_val[ii] = k;
      if(pdata->first[k] == ii) /* First and last occurrences coincide */
	if(mapped_dvar_fix_value_l(pdata, VAR(ii), k)<0)
	  return FALSE;		/* doesn't happen? */
      --k;
    }
  }
  return TRUE;
}

#define ARG_NG   3
#define ARG_TTOP 4

static DAEMON_RC SPCDECL 
value_precede_chain_daemon(Wam wam, void *vdata, SP_globref attr_ref, TAGGED *global)
{
  struct value_precede_chain_data *pdata = (struct value_precede_chain_data *)vdata;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  int *btr = pdata->trail + GetSmall(CTagToArg(tstate,ARG_TTOP));
  SP_BOOL buried;
  SP_integer state_stamp;
  DAEMON_RC rc = DAEMON_FIX;
  int ii = (int)(attr_ref - pdata->refbase) >> 1;
  Dvar dv = VAR(ii);
  int ar = Arity(TagToHeadfunctor(tstate));
  int k, m;
  
  dvar_refresh(dv);
  m = mapped_dvar_min_l(wam, pdata, dv);
  while (pdata->ttop != btr) {
    int offset;
    pdata->ttop -= 2;
    offset = pdata->ttop[0];
    pdata->first[offset] = pdata->ttop[1];
  }
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    for(k = 1; k <= pdata->max_val; k++)
      pdata->buffer[k] = 0;
    pdata->stamp = state_stamp;
  }
  if (pdata->first[pdata->first_val[ii]] == ii && mapped_dvar_max_l(pdata, dv) < pdata->first_val[ii]) {
    pdata->buffer[pdata->first_val[ii]] |= 0x1;
    rc = DAEMON_NOFIX;
  }
  if(pdata->limit[pdata->limit_val[ii]] == ii && !mapped_dvar_contains_value_l(pdata, dv, pdata->limit_val[ii])) {
    pdata->buffer[pdata->limit_val[ii]] |= 0x2;
    rc = DAEMON_NOFIX;
  }
  if(m > 0 && ii < pdata->limit[m]) {
    trail(wam, pdata, LIMIT, m, ii);
    pdata->buffer[m] |= 0x2;
    rc = DAEMON_NOFIX;
  }
  (void)fd_daemon_copy_state(wam, global,&buried);
  if (!buried)
    CTagToArg(tstate,ar) -= IStep(1);
  else
    pdata->stamp++;	/* increase iff old and new states are separated by a choicepoint */
  return rc;
}

#if 0
static void dump_state(struct value_precede_chain_data *pdata) {
  int i;

  fprintf(stderr, "first =     ");
  for (i=0; i<pdata->nvars+1; i++)
    fprintf(stderr, "%d ", pdata->first[i]);
  fprintf(stderr, "\n");
  fprintf(stderr, "first_val = ");
  for (i=0; i<pdata->nvars+1; i++)
    fprintf(stderr, "%d ", pdata->first_val[i]);
  fprintf(stderr, "\n");
  fprintf(stderr, "limit =     ");
  for (i=0; i<pdata->nvars+1; i++)
    fprintf(stderr, "%d ", pdata->limit[i]);
  fprintf(stderr, "\n");
  fprintf(stderr, "limit_val = ");
  for (i=0; i<pdata->nvars+1; i++)
    fprintf(stderr, "%d ", pdata->limit_val[i]);
  fprintf(stderr, "\n");
  fprintf(stderr, "\n");
}
#endif


/*
  '$fd_value_precede_chain'(+State0, +State, -Actions).
  State = state(Values,VATuple,NGround,TrailTop,Handle,Stamp)
*/
void SPCDECL
prolog_fd_value_precede_chain(Wam wam,
			      SP_term_ref State0,
			      SP_term_ref State,
			      SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  int nvars, nnonground;
  TAGGED handle, cur;
  struct value_precede_chain_data *pdata;
  SP_BOOL committed, posted, allbutone;
  char *ptr;
  int i, k;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct value_precede_chain_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    int nvals;
    SP_integer extra_size;

    posted = TRUE;
    DerefArg(cur,X(0),1);
    nvals = fd_list_length(cur);
    DerefArg(cur,X(0),2);
    nvars = fd_list_length(cur);
    extra_size = nvars*sizeof(struct dvar) +
      nvals*sizeof(TAGGED) +
      (nvals+1)*(nvals+1)*sizeof(TAGGED) +
      (5*nvars+4)*sizeof(int) +
      nvars+1;
    pdata = Palloc(struct value_precede_chain_data, extra_size, handle); /* GC, clobbers cur */
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->values = (TAGGED *)ptr;
    ptr += nvals*sizeof(TAGGED);
    pdata->succ = (TAGGED *)ptr;
    ptr += (nvals+1)*(nvals+1)*sizeof(TAGGED);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->first = (int *)ptr;
    ptr += (nvars+1)*sizeof(int);
    pdata->limit = (int *)ptr;
    ptr += (nvars+1)*sizeof(int);
    pdata->first_val = (int *)ptr;
    ptr += (nvars+1)*sizeof(int);
    pdata->limit_val = (int *)ptr;
    ptr += (nvars+1)*sizeof(int);
    pdata->buffer = (unsigned char *)ptr;
    ptr += nvars+1;
    SP_ASSERT(ptr==(char *)(pdata+1)+extra_size);
    pdata->destructor = value_precede_chain_destructor;
    pdata->daemon = value_precede_chain_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->stamp = 0;
    pdata->nvals = nvals;
    pdata->nvars = nvars;
    pdata->refbase = SP_alloc_globrefs(2*nvars);
    pdata->trail = SP_malloc(16*sizeof(int));
    pdata->ttop = pdata->trail;
    pdata->tend = pdata->trail+16;
    DerefArg(cur,X(0),1);
    for (i=0; i<nvals; i++) {
      TAGGED x;
      DerefCar(x,cur);
      DerefCdr(cur,cur);
      pdata->values[i] = x;
    }
    for (i=0; i<=nvals; i++) {
      int base = i * (nvals+1);
      int pos = base;
      for (k=i; k<nvals; k++)
	pdata->succ[pos++] = pdata->values[k];
      fd_qsort_asc_tagged(wam, &pdata->succ[base], nvals-i);
      pdata->succ[pos] = atom_nil;
    }
    DerefArg(cur,X(0),2);
    for (i=0, k=0; i<nvars; i++, k+=2) {
      TAGGED x;
      DerefCar(x,cur);
      DerefCdr(cur,cur);
      fd_get_var_and_attr(x,pdata->refbase + k);
      SV(i) = i;
    }
    for (i=0, k=0; i<nvars; i++, k+=2) {
      SP_globref refoffset = pdata->refbase + k;
      Dvar dv = VAR(i);
      dvar_init(dv, refoffset,   refoffset+1);
      dvar_attach_daemon(wam, dv, pdata, X(1), functor_dom1);
    }
  }
  
  /* RESUME HERE */
  dvar_export_start(wam);
  DerefArg(cur,X(0),ARG_NG);
  nnonground = nvars - GetSmall_int(cur);
  for (i=0; i<nnonground; i++)
    dvar_refresh(VAR(SV(i)));
  if (posted) {
    if (!cold_propagation(wam, pdata))
      goto ret;
  } else {
    if (!warm_propagation(wam, pdata))
      goto ret;
  }
  for (i=0; i<nnonground; i++)
    dvar_pruning_done(VAR(SV(i)));
  for (i=0; i<nnonground; i++)
    dvar_export(VAR(SV(i)));
  
  allbutone = TRUE;
  for (k=1; allbutone && k<pdata->max_val; k++)
    allbutone = (pdata->first[k] == pdata->limit[k]);

  if (!allbutone) {
    int inf = 0;
    int sup = nnonground-1;
    int held = SV(sup); /* sup is the hole */
    int current = SV(0);
    while (inf<=sup) {
      Dvar dv = VAR(current);
      if (!dvar_is_integer(dv)) {
	SV(inf) = current;
	inf++;
	current = (inf>=sup ? held : SV(inf));
      } else {
	SV(sup) = current;
	sup--;
	current = (inf>=sup ? held : SV(sup));
      }
    }
    CTagToArg(X(0),ARG_NG) = MakeSmall(nvars-inf);
    CTagToArg(X(0),ARG_TTOP) = MakeSmall(pdata->ttop - pdata->trail);
    ent = 0;
  } else {
    ent = 1;
    Pfree;
  }
 ret:
  dvar_export_done(wam,Actions, ent);
}

