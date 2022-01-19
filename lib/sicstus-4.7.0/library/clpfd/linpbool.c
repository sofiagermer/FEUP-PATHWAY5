/* Copyright(C) 1999, Swedish Institute of Computer Science */
/* Linear arithmetic over 0-1 variables, aka. pseudo-boolean constraints */

#include "fd.h"
#include "dvars.h"

struct dlist {
  int term;
  struct dlist *forward;
  struct dlist *backward;
};

struct linear_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_globref refbase;
  enum rel op;
  enum rel action;
  int nonground;		/* maintained incrementally */
  int nvars;			/* #terms */
  SP_integer bige;
  SP_integer bigf;
  int *target;
  int *tloc;
  struct bvar reif;
  Bvar bvar;
  struct {
    SP_integer *cmin;		/* min(ai*xi) */
    SP_integer *cmax;		/* max(ai*xi) */
    SP_integer *interval;	/* cmax-cmin */
    SP_integer *coeff;		/* ai */
  } term;
  struct dlist root;
  struct dlist *dlist_item;	/* [nvars] */
};

  /* Maintain:
     bigf = rhs - sum{min(a_i x_i)}
     bige = sum{max(a_i x_i)} - rhs

     I_i = max(a_i x_i) - min(a_i x_i)
  */

#define COEFF(t) (pdata->term.coeff[t])
#define BVAR(t) (pdata->bvar+(t))
#define RefAttr(T) (pdata->refbase + 2*(T) + 2)
#define RefVar(T) (pdata->refbase + 2*(T) + 3)
#define RefAttrReif (pdata->refbase)
#define RefVarReif (pdata->refbase + 1)
#define CMIN(t) (pdata->term.cmin[t])
#define CMAX(t) (pdata->term.cmax[t])
#define INTERVAL(t) (pdata->term.interval[t])

static void SPCDECL linear_destructor(void *pdata_v)
{
  struct linear_data *pdata = (struct linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,(pdata->nvars<<1) + 2);
  SP_free(pdata);
}

static int
op_action(struct linear_data *pdata)
{
  bvar_refresh(&pdata->reif);
  if (pdata->reif.min==TaggedOne)
    return pdata->op;
  else if (pdata->reif.max==TaggedZero)
    return ASK_LT - pdata->op;
  else
    return pdata->op + ASK_LT - 1;
}  

static SP_BOOL
not_fixpoint(struct linear_data *pdata)
{
  if (pdata->nonground==0) {
    return TRUE;
  } else {
    int elt = pdata->root.forward->term;
    switch (pdata->action) {
    case TELL_LT:
      return (pdata->bigf <= INTERVAL(elt) || pdata->bige < 0);
    case TELL_LE:
      return (pdata->bigf < INTERVAL(elt) || pdata->bige <= 0);
    case TELL_GT:
      return (pdata->bige <= INTERVAL(elt) || pdata->bigf < 0);
    case TELL_GE:
      return (pdata->bige < INTERVAL(elt) || pdata->bigf <= 0);
    case TELL_EQ:
      return (pdata->bigf < INTERVAL(elt) ||
	      pdata->bige < INTERVAL(elt));
    case TELL_NE:
      return (pdata->nonground <= 1);
    case ASK_LT:
    case ASK_GE:
      return (pdata->bigf <= 0 || pdata->bige < 0);
    case ASK_LE:
    case ASK_GT:
      return (pdata->bigf < 0 || pdata->bige <= 0);
    case ASK_EQ:
    case ASK_NE:
    default:
      return (pdata->bigf < 0 || pdata->bige < 0 || (pdata->bigf == 0 && pdata->bige == 0));
    }
  }
}

static DAEMON_RC SPCDECL 
linear_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct linear_data *pdata = (struct linear_data *)vdata;
  int elt = (int)((attr_ref - pdata->refbase)>>1) - 1;
  SP_BOOL buried;
  SP_integer cminj, cmaxj;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried);
  int nonground = pdata->nvars - GetSmall_int(CTagToArg(tstate,5));
  DAEMON_RC rc = DAEMON_FIX;

  while (pdata->nonground < nonground) {	/* restore state wrt. trail */
    int ielt = pdata->target[pdata->nonground++];
    struct dlist *item = pdata->dlist_item+ielt;
    SP_integer c = COEFF(ielt);
    
    if (c>0) {
      cminj = 0;
      cmaxj = c;
    } else {
      cmaxj = 0;
      cminj = c;
    }
    pdata->bigf += CMIN(ielt)-cminj;
    pdata->bige -= CMAX(ielt)-cmaxj;
    CMIN(ielt) = cminj;
    CMAX(ielt) = cmaxj;
    INTERVAL(ielt) = cmaxj - cminj;
				/* dlist_insert(item); */
    item->backward->forward = item;
    item->forward->backward = item;
  }
  if (elt>=0) {				/* update state wrt. elt */
    struct dlist *item = pdata->dlist_item+elt;
    Bvar bv = BVAR(elt);
    SP_integer c = COEFF(elt);
    SP_integer pi = INTERVAL(elt); /* can be 0, if self-invocation */
    
    bvar_refresh(bv);
    cminj = (Tgtz(bv->min) ? c : 0);
    pdata->bigf += CMIN(elt)-cminj;
    pdata->bige -= CMAX(elt)-cminj;
    CMIN(elt) = cminj;
    CMAX(elt) = cminj;
    INTERVAL(elt) = 0;
    if (pi>0) {
      int loc = pdata->tloc[elt];
      int swap = pdata->target[--pdata->nonground];
      pdata->target[loc] = swap;
      pdata->tloc[swap] = loc;
      pdata->target[pdata->nonground] = elt;
      pdata->tloc[elt] = pdata->nonground;
				/* dlist_delete(item); */
      item->backward->forward = item->forward;
      item->forward->backward = item->backward;
    }
  }
  CTagToArg(tstate,5) = MakeSmall(pdata->nvars-pdata->nonground); /* update NGround */
  pdata->action = op_action(pdata);
  if (not_fixpoint(pdata))
    rc = DAEMON_NOFIX;
  return rc;
}

static SP_BOOL
scalar_product_le(Wam wam, struct linear_data *pdata, int elt, SP_BOOL strict)
{
  SP_integer c = COEFF(elt);
  struct dlist *item = pdata->dlist_item+elt;

  /*
    Phase 2:

    For <=
    ******
    bigf>=0 is a necessary condition.
    bige<=0 is a sufficient condition.

    enforce:
    bigf >= I_i for all i

    rules:
    x_i <=  floor(F / a_i) + min(x_i)  if a_i>0
    x_i >= -floor(F /-a_i) + max(x_i)  if a_i<0

  */

  if (pdata->bigf-strict<0)
    return FALSE;
  if (c>0) {
    if (!bvar_export_value(wam, BVAR(elt), 0))
      return FALSE;
    CMAX(elt) = 0;
    pdata->bige -= c;
  } else {
    if (!bvar_export_value(wam, BVAR(elt), 1))
      return FALSE;
    CMAX(elt) = c;
    pdata->bige += c;
  }
  INTERVAL(elt) = 0;
  {
    int loc = pdata->tloc[elt];
    int swap = pdata->target[--pdata->nonground];
    pdata->target[loc] = swap;
    pdata->tloc[swap] = loc;
    pdata->target[pdata->nonground] = elt;
    pdata->tloc[elt] = pdata->nonground;
  }
				/* dlist_delete(elt); */
  item->backward->forward = item->forward;
  item->forward->backward = item->backward;
  return TRUE;
}

static SP_BOOL
scalar_product_ge(Wam wam, struct linear_data *pdata, int elt, SP_BOOL strict)
{
  SP_integer c = COEFF(elt);
  struct dlist *item = pdata->dlist_item+elt;

  /*
    Phase 2:

    For >=
    ******
    bige>=0 is a necessary condition.
    bigf<=0 is a sufficient condition.

    enforce:
    bige >= I_i for all i

    rules:
    x_i >= -floor(E / a_i) + max(x_i)  if a_i>0
    x_i <=  floor(E /-a_i) + min(x_i)  if a_i<0
  */

  if (pdata->bige-strict<0)
    return FALSE;
  if (c>0) {
    if (!bvar_export_value(wam, BVAR(elt), 1))
      return FALSE;
    CMIN(elt) = c;
    pdata->bigf -= c;
  } else {
    if (!bvar_export_value(wam, BVAR(elt), 0))
      return FALSE;
    CMIN(elt) = 0;
    pdata->bigf += c;
  }
  INTERVAL(elt) = 0;
  {
    int loc = pdata->tloc[elt];
    int swap = pdata->target[--pdata->nonground];
    pdata->target[loc] = swap;
    pdata->tloc[swap] = loc;
    pdata->target[pdata->nonground] = elt;
    pdata->tloc[elt] = pdata->nonground;
  }
				/* dlist_delete(elt); */
  item->backward->forward = item->forward;
  item->forward->backward = item->backward;
  return TRUE;
}

static SP_BOOL
scalar_product_setmin(Wam wam, struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    int elt = pdata->target[i];
    if (!bvar_export_value(wam, BVAR(elt), COEFF(elt)<0))
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_setmax(Wam wam, struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    int elt = pdata->target[i];
    if (!bvar_export_value(wam, BVAR(elt), COEFF(elt)>0))
      return FALSE;
  }
  return TRUE;
}

/* sort by descending interval */
static int 
cmp_dec_interval(Wam wam, int *v1, int *v2)
{
  struct linear_data *pdata = fd.gdata; 
  SP_integer d1 = INTERVAL(*v1);
  SP_integer d2 = INTERVAL(*v2);

  return (d1>d2) ? -1 : (d1<d2) ? 1 : 0;
}

#if SP_ASSERTIONS
/* check that NONGROUND items precede GROUND items */
static SP_BOOL verify_nonground(struct linear_data *pdata)
{
  int i;

  for (i=0; i<pdata->nonground; i++)
    if (!INTERVAL(pdata->target[i]))
      return FALSE;
  for (; i<pdata->nvars; i++)
    if (INTERVAL(pdata->target[i]))
      return FALSE;

  return TRUE;
}
#endif

#define QType int
#define QCmp  cmp_dec_interval
#define QSort qsort_dec_interval
#include "qsort.ic"

SP_BOOL
fd_linear_filter_pbool(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int nvars, i, j, k, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct linear_data *pdata;

/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_static_output_state(wam, &handle, &committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    struct dlist *curitem;
    
    DerefArg(tvec,X(0),1);	  /* get CX0 */
    nvars = fd_list_length(tvec); /* count terms */
    total_size = 
      nvars*sizeof(struct bvar) +
      nvars*sizeof(struct dlist) +
      4*nvars*sizeof(SP_integer) +
      nvars*sizeof(int) +
      nvars*sizeof(int);
  
    pdata = Palloc(struct linear_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->bvar = (Bvar)ptr;
    ptr += nvars*sizeof(struct bvar);
    pdata->dlist_item = (struct dlist *)ptr;
    ptr += nvars*sizeof(struct dlist);
    pdata->term.cmin = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.cmax = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.interval = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs((nvars<<1) + 2);
    DerefArg(telt,X(0),2);
    pdata->op = GetSmall(telt) & 0x7;
    pdata->destructor = linear_destructor;
    pdata->daemon = linear_daemon;
    pdata->nvars = nvars;
    DerefArg(telt,X(0),4);	/* get Reif */
    fd_get_var_and_attr(telt,RefAttrReif);
    DerefArg(telt,X(0),3);	/* get RHS */
    pdata->bigf = GetSmall(telt);
    pdata->bige = -GetSmall(telt);
    DerefArg(tvec,X(0),1);	/* get CX0 */
    for (i=0; i<nvars; i++) {
      int elt = i;
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = GetSmall(t1);
      fd_get_var_and_attr(telt+WD(1),RefAttr(elt));
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    bvar_init(&pdata->reif, RefAttrReif, RefVarReif);
    bvar_attach_daemon(wam, &pdata->reif, pdata, X(1), fd.functor_val);
    pdata->action = op_action(pdata);
    for (i=0; i<nvars; i++) {
      int elt = i;
      Bvar bv = BVAR(elt);
      TAGGED functor;
      bvar_init(bv, RefAttr(elt), RefVar(elt));
      functor = fd.functor_val;
      bvar_attach_daemon(wam, bv, pdata, X(1), functor);
    }
    for (i=0, j=0, k=nvars; i<nvars; i++) {
      int elt = i;
      SP_WORD c = COEFF(elt);
      Bvar bv = BVAR(elt);

      if (c>0) {
	CMIN(elt) = c*bvar_min_l(bv);
	CMAX(elt) = c*bvar_max_l(bv);
      } else {
	CMAX(elt) = c*bvar_min_l(bv);
	CMIN(elt) = c*bvar_max_l(bv);
      }
      INTERVAL(elt) = CMAX(elt) - CMIN(elt);
      pdata->bigf -= CMIN(elt);
      pdata->bige += CMAX(elt);
      if (bvar_is_integer(bv)) {
	pdata->tloc[elt] = --k;
	pdata->target[k] = elt;
      } else {
	pdata->tloc[elt] = j;
	pdata->target[j++] = elt;
      }
    }
    pdata->nonground = j;
    /* build the dlist by decreasing interval */
    fd.gdata = pdata;
    qsort_dec_interval(wam, pdata->target,pdata->nonground);
    curitem = &pdata->root;
    for (i=0; i<pdata->nonground; i++) {
      int elt = pdata->target[i];
      struct dlist *item = pdata->dlist_item+elt;

      pdata->tloc[elt] = i;
      item->term = elt;
      curitem->forward = item;
      item->backward = curitem;
      curitem = item;
    }
    curitem->forward = &pdata->root;
    pdata->root.backward = curitem;
  }
  
				/* RESUME HERE */
  dvar_export_start(wam);
  SP_ASSERT(verify_nonground(pdata));
  if (pdata->action >= ASK_LT) {
    int entailed = 2;
    switch (pdata->action) {
    case ASK_LT:
      entailed = pdata->bige < 0 ? 1 : pdata->bigf <= 0 ? 0 : 2;
      break;
    case ASK_LE:
      entailed = pdata->bige <= 0 ? 1 : pdata->bigf < 0 ? 0 : 2;
      break;
    case ASK_GT:
      entailed = pdata->bigf < 0 ? 1 : pdata->bige <= 0 ? 0 : 2;
      break;
    case ASK_GE:
      entailed = pdata->bigf <= 0 ? 1 : pdata->bige < 0 ? 0 : 2;
      break;
    case ASK_EQ:
      entailed = (pdata->bige == 0 && pdata->bigf == 0) ? 1 : (pdata->bige < 0 || pdata->bigf < 0) ? 0 : 2;
      break;
    case ASK_NE:
    default:
      entailed = (pdata->bige < 0 || pdata->bigf < 0) ? 1 : (pdata->bige == 0 && pdata->bigf == 0) ? 0 : 2;
      break;
    }
    ent = (entailed<2);
    if (ent)
      bvar_export_value(wam, &pdata->reif, entailed);
    goto ret;
  }
  /* fast special cases */
  switch (pdata->action) {
  case TELL_LT:
    if (pdata->bigf <= 0) {
      goto ret;
    } else if (pdata->bigf==1) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto ret;
    }
    break;
  case TELL_LE:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto ret;
    }
    break;
  case TELL_GT:
    if (pdata->bige <= 0) {
      goto ret;
    } else if (pdata->bige==1) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto ret;
    }
    break;
  case TELL_GE:
    if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto ret;
    }
    break;
  case TELL_EQ:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto ret;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto ret;
    }
    break;
  case TELL_NE:
  default:
    if (pdata->bigf==0 && pdata->bige==0)
      goto ret;
    break;
  }

 loop:
  switch (pdata->action) {
  case TELL_LT:
    if (pdata->nonground>0) {
      int elt = pdata->root.forward->term;
      if (pdata->bigf <= INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata, elt, 1)) {
	  goto ret;
	} else {
	  goto loop;
	}
      }
    }
    break;
  case TELL_LE:
    if (pdata->nonground>0) {
      int elt = pdata->root.forward->term;
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata, elt, 0)) {
	  goto ret;
	} else {
	  goto loop;
	}
      }
    }
    break;
  case TELL_GT:
    if (pdata->nonground>0) {
      int elt = pdata->root.forward->term;
      if (pdata->bige <= INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata, elt, 1)) {
	  goto ret;
	} else {
	  goto loop;
	}
      }
    }
    break;
  case TELL_GE:
    if (pdata->nonground>0) {
      int elt = pdata->root.forward->term;
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata, elt, 0)) {
	  goto ret;
	} else {
	  goto loop;
	}
      }
    }
    break;
  case TELL_EQ:
    if (pdata->nonground>0) {
      int elt = pdata->root.forward->term; 
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata, elt, 0))
	  goto ret;
	goto loop;
      }
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata, elt, 0))
	  goto ret;
	goto loop;
      }
    }
    break;
  case TELL_NE:
  default:
    if (pdata->nonground==0 && pdata->bigf==0) {
      goto ret;
    } else if (pdata->nonground==1) {
      int elt = pdata->root.forward->term;
      
      if (pdata->bigf % COEFF(elt)==0) { /* RHS a multiple of coefficient */
	SP_integer nono = (pdata->bigf+CMIN(elt))/COEFF(elt);
	if (nono >= 0 && nono <= 1) {
	  if (!bvar_export_value(wam, BVAR(elt), 1-(int)nono))
	    return -1;
	}
      }
    }
    break;
  }
  switch (pdata->action) {
  case TELL_LT:
    ent = (pdata->bigf <= 0 ? -1 : pdata->bige < 0);
    break;
  case TELL_LE:
    ent = (pdata->bigf < 0 ? -1 : pdata->bige <= 0);
    break;
  case TELL_GT:
    ent = (pdata->bige <= 0 ? -1 : pdata->bigf < 0);
    break;
  case TELL_GE:
    ent = (pdata->bige < 0 ? -1 : pdata->bigf <= 0);
    break;
  case TELL_EQ:
    ent = (pdata->bigf < 0 ? -1 :
	   pdata->bige < 0 ? -1 : 
	   pdata->bigf > 0 ? 0 :
	   pdata->bige > 0 ? 0 : 1);
    break;
  case TELL_NE:
  default:
    ent = (pdata->nonground<=1);
    break;
  }

 ret:
  CTagToArg(X(0),5) = MakeSmall(pdata->nvars-pdata->nonground); /* update NGround */
  return ent;
}
