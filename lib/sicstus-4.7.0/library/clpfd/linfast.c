/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct linear_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_integer stamp;		/* increases up to backtracking */
  SP_globref refbase;
  enum rel op;
  enum rel action;
  int nonground;		/* maintained incrementally */
  int nvars;			/* #terms */
  SP_BOOL gcd_due;		/* whether gcd check is due */
  SP_integer bige;
  SP_integer bigf;
  int *heap;
  int *vheap;
  struct bvar reif;
  Dvar dvar;
  Dvar* refreshed;
  Dvar* refreshed_top;
  int *trail;
  int *ttop;
  int *tend;
  struct {
    SP_integer *cmin;		/* min(ai*xi) */
    SP_integer *cmax;		/* max(ai*xi) */
    SP_integer *interval;	/* cmax-cmin */
    SP_integer *coeff;		/* ai */
    SP_integer *abscoeff;	/* |ai| */
  } term;
};

  /* Maintain:
     bigf = rhs - sum{min(a_i x_i)}
     bige = sum{max(a_i x_i)} - rhs

     I_i = max(a_i x_i) - min(a_i x_i)
  */

#define IS_HEAP_OP(op) ((op) < ASK_LT && (op) != TELL_NE)

/* INVARIANTS: 
   (1) heap is partitioned into [ nonground_items | ground_items ]
   (2) if IS_HEAP_OP(pdata->action) then
          nonground_items is a heap of items i sorted on INTERVAL(i)
*/

#define COEFF(t) (pdata->term.coeff[t])
#define ABSCOEFF(t) (pdata->term.abscoeff[t])
#define DVAR(t) (pdata->dvar+(t))
#define RefAttr(T) (pdata->refbase + 2*(T) + 2)
#define RefVar(T) (pdata->refbase + 2*(T) + 3)
#define RefAttrReif (pdata->refbase)
#define RefVarReif (pdata->refbase + 1)
#define CMIN(t) (pdata->term.cmin[t])
#define CMAX(t) (pdata->term.cmax[t])
#define INTERVAL(t) (pdata->term.interval[t])

#define SWAP(I,J)				\
{						\
  int vi = heap[I];				\
  int vj = heap[J];				\
  heap[I] = vj;					\
  heap[J] = vi;					\
  vheap[vi] = (J);				\
  vheap[vj] = (I);				\
}

static void SPCDECL linear_destructor(void *pdata_v)
{
  struct linear_data *pdata = (struct linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,(pdata->nvars<<1) + 2);
  SP_free(pdata->trail);
  SP_free(pdata);
}

static void
trail_expand(Wam wam, struct linear_data *pdata)
{
  SP_integer inuse   = pdata->ttop - pdata->trail;
  SP_integer oldsize = pdata->tend - pdata->trail;
  
  pdata->trail = SP_realloc(pdata->trail, 2*oldsize*sizeof(int));
  pdata->ttop = pdata->trail + inuse;
  pdata->tend = pdata->trail + 2*oldsize;
}

static void trail_var(Wam wam, struct linear_data *pdata, int term) {
  if (pdata->ttop >= pdata->tend)
    trail_expand(wam,pdata);
  *pdata->ttop++ = term;
}

#if 0
static SP_BOOL
verify_heap(struct linear_data *pdata)
{
  int *heap = pdata->heap;
  int i;
  for (i=1; i<pdata->nonground/2; i++) {
    int l = (i<<1)+1;
    int r = l+1;
    if (l<pdata->nonground && INTERVAL(heap[l]) > INTERVAL(heap[i])) {
      fprintf(stderr, "heap[%d]: %d with label %ld\n", i, heap[i], INTERVAL(heap[i]));
      fprintf(stderr, "heap[%d]: %d with label %ld\n", l, heap[l], INTERVAL(heap[l]));
      return FALSE;
    }
    if (r<pdata->nonground && INTERVAL(heap[r]) > INTERVAL(heap[i])) {
      fprintf(stderr, "heap[%d]: %d with label %ld\n", i, heap[i], INTERVAL(heap[i]));
      fprintf(stderr, "heap[%d]: %d with label %ld\n", r, heap[r], INTERVAL(heap[r]));
      return FALSE;
    }
  }
  return TRUE;
}
#endif

static void 
heap_demote(struct linear_data *pdata, int i)
{
  int *heap = pdata->heap;
  int *vheap = pdata->vheap;
  int elt = heap[i];
  SP_integer key = INTERVAL(elt);
  
  for (;;) {
    int l = (i<<1)+1;
    int r = l+1;
    int topmost = i;
    int topelt;
    SP_integer lkey = l<pdata->nonground ? INTERVAL(heap[l]) : 0;
    SP_integer rkey = r<pdata->nonground ? INTERVAL(heap[r]) : 0;
    if (lkey > key && lkey >= rkey)
      topmost = l;
    else if (rkey > key && rkey > lkey)
      topmost = r;
    else
      break;
    topelt = heap[topmost];
    heap[i] = topelt;
    vheap[topelt] = i;
    i = topmost;
  }
  heap[i] = elt;
  vheap[elt] = i;
}

static void
heap_promote(struct linear_data *pdata, int i)
{
  int *heap = pdata->heap;
  int *vheap = pdata->vheap;
  int dest = i;
  int elt = heap[i];
  SP_integer key = INTERVAL(elt);

  while (dest>0 && INTERVAL(heap[(dest-1)>>1]) < key)
    dest = (dest-1)>>1;
  while (i>dest) {
    int parent = (i-1)>>1;
    int vj = heap[parent];
    heap[i] = vj;
    vheap[vj] = i;
    i = parent;
  }
  heap[i] = elt;
  vheap[elt] = i;
}

static void
sync_action(struct linear_data *pdata)
{
  enum rel newrel;
  
  bvar_refresh(&pdata->reif);
  if (pdata->reif.min==TaggedOne)
    newrel = pdata->op;
  else if (pdata->reif.max==TaggedZero)
    newrel = ASK_LT - pdata->op;
  else
    newrel = pdata->op + ASK_LT - 1;
  if (!IS_HEAP_OP(pdata->action) && IS_HEAP_OP(newrel)) {
    int i;
    for (i=(pdata->nonground-2)>>1; i>=0; i--)
      heap_demote(pdata,i);
    pdata->gcd_due = TRUE;
  }
  pdata->action = newrel;
}  

static SP_BOOL
not_fixpoint(struct linear_data *pdata)
{
  int elt;
  if (pdata->nonground==0) {
    return TRUE;
  } else {
    switch (pdata->action) {
    case TELL_LT:
      elt = pdata->heap[0];
      return (pdata->bigf <= INTERVAL(elt) || pdata->bige < 0);
    case TELL_LE:
      elt = pdata->heap[0];
      return (pdata->bigf < INTERVAL(elt) || pdata->bige <= 0);
    case TELL_GT:
      elt = pdata->heap[0];
      return (pdata->bige <= INTERVAL(elt) || pdata->bigf < 0);
    case TELL_GE:
      elt = pdata->heap[0];
      return (pdata->bige < INTERVAL(elt) || pdata->bigf <= 0);
    case TELL_EQ:
      elt = pdata->heap[0];
      return (pdata->gcd_due ||
	      pdata->bigf < INTERVAL(elt) ||
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

/* assertion: IS_HEAP_OP(pdata->action) && oldkey != newkey */
static void
repair_heap(struct linear_data *pdata, int ielt, SP_integer oldkey, SP_integer newkey)
{
  int *heap = pdata->heap;
  int *vheap = pdata->vheap;
  int index = vheap[ielt];
  
  if (newkey==0) {
    pdata->gcd_due = TRUE;
    --pdata->nonground;
    if (index != pdata->nonground) {
      SWAP(index, pdata->nonground);
      newkey = INTERVAL(heap[index]);
      if (oldkey>newkey) {
	heap_demote(pdata, index);
      } else {
	heap_promote(pdata, index);
      }
    }
  } else if (oldkey==0) {
    if (index != pdata->nonground) {
      SWAP(index, pdata->nonground);
      index = pdata->nonground;
    }
    pdata->nonground++;
    heap_promote(pdata, index);
  } else if (oldkey>newkey) {
    heap_demote(pdata, index);
  } else {
    heap_promote(pdata, index);
  }
#if 0
  if (!verify_heap(pdata))
    fprintf(stderr, "! CORRUPT HEAP\n");
#endif
}

/* assertion: oldkey != newkey */
static void
repair_partition(struct linear_data *pdata, int ielt, SP_integer oldkey, SP_integer newkey)
{
  int *heap = pdata->heap;
  int *vheap = pdata->vheap;
  int index = vheap[ielt];
  
  if (newkey==0) {
    --pdata->nonground;
    if (index != pdata->nonground) {
      SWAP(index, pdata->nonground);
    }
  } else if (oldkey==0) {
    pdata->nonground++;
    if (index != pdata->nonground-1) {
      SWAP(index, pdata->nonground-1);
    }
  }
}

static void sync_var(Wam wam, struct linear_data *pdata, int ielt)
{
  SP_globref ref = RefAttr(ielt);
  SP_integer c = COEFF(ielt);
  TAGGED tmin, tmax;
  SP_integer key_mem = INTERVAL(ielt);
  SP_integer cminj, cmaxj, key_new;
  (void)wam;

  REF_GET_BOUNDS(ref, tmin, tmax);
  if (c>0) {
    cminj = c*GetSmall(tmin);
    cmaxj = c*GetSmall(tmax);
  } else {
    cmaxj = c*GetSmall(tmin);
    cminj = c*GetSmall(tmax);
  }
  if (cminj!=CMIN(ielt) || cmaxj!=CMAX(ielt)) {
    pdata->bigf += CMIN(ielt) - cminj;
    pdata->bige -= CMAX(ielt) - cmaxj;
    CMIN(ielt) = cminj;
    CMAX(ielt) = cmaxj;
    INTERVAL(ielt) = key_new = cmaxj - cminj;
    if (IS_HEAP_OP(pdata->action) && key_mem != key_new)
      repair_heap(pdata, ielt, key_mem, key_new);
    else if (key_mem != key_new)
      repair_partition(pdata, ielt, key_mem, key_new);
  }
}

#define ARG_TTOP 5

static DAEMON_RC SPCDECL 
linear_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct linear_data *pdata = (struct linear_data *)vdata;
  int elt = (int)((attr_ref - pdata->refbase)>>1) - 1;
  SP_BOOL buried;
  SP_BOOL synced = FALSE;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  int *btr = pdata->trail + GetSmall(CTagToArg(tstate,ARG_TTOP));
  DAEMON_RC rc = DAEMON_FIX;
  int ar;

  ar = Arity(TagToHeadfunctor(tstate));
  while (pdata->ttop != btr) {
    int item = *--pdata->ttop;
    sync_var(wam, pdata, item);
    if (item==elt)
      synced = TRUE;
  }
  if (elt>=0) {
    if (!synced) 
      sync_var(wam, pdata, elt);
    trail_var(wam, pdata, elt);
  }
  tstate = fd_daemon_copy_state(wam, global,&buried);
  if (!buried)
    CTagToArg(tstate,ar) -= IStep(1);
  else
    pdata->stamp++;	/* increase iff old and new states are separated by a choicepoint */
  CTagToArg(tstate,ARG_TTOP) = MakeSmall(pdata->ttop - pdata->trail);
  sync_action(pdata);
  if (not_fixpoint(pdata))
    rc = DAEMON_NOFIX;
  return rc;
}

static void
dvar_refresh_lazy(Wam wam, struct linear_data *pdata, Dvar dv)
{
  (void)wam;
  if (dv->flags & 0x80) {
    dvar_refresh(dv);
    *pdata->refreshed_top++ = dv;
  }
}

static SP_BOOL
scalar_product_le(Wam wam, struct linear_data *pdata, int elt, SP_BOOL strict)
{
  Dvar dv = DVAR(elt);
  SP_integer c = COEFF(elt);
  SP_integer cmax0 = CMAX(elt);
  SP_integer key_mem = INTERVAL(elt);
  SP_integer decr;
  SP_integer cmax;
  SP_integer bigf1 = pdata->bigf-strict;
  (void)wam;

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

  if (bigf1<0)
    return FALSE;
  dvar_refresh_lazy(wam, pdata, dv);
  if (c>0) {
    SP_integer ub = bigf1/c + dvar_min_l(dv);
    if (dvar_fix_max_l(dv,ub)<0)
      return FALSE;
    cmax = c*dvar_max_l(dv);
  } else {
    SP_integer lb = -(bigf1/(-c)) + dvar_max_l(dv);
    if (dvar_fix_min_l(dv,lb)<0)
      return FALSE;
    cmax = c*dvar_min_l(dv);
  }
  CMAX(elt) = cmax;
  decr = cmax0-cmax;
  INTERVAL(elt) -= decr;
  pdata->bige -= decr;
  trail_var(wam, pdata, elt);
  repair_heap(pdata, elt, key_mem, INTERVAL(elt));
  return TRUE;
}

static SP_BOOL
scalar_product_ge(Wam wam, struct linear_data *pdata, int elt, SP_BOOL strict)
{
  Dvar dv = DVAR(elt);
  SP_integer c = COEFF(elt);
  SP_integer cmin0 = CMIN(elt);
  SP_integer key_mem = INTERVAL(elt);
  SP_integer decr;
  SP_integer cmin;
  SP_integer bige1 = pdata->bige-strict;
  (void)wam;

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

  if (bige1<0)
    return FALSE;
  dvar_refresh_lazy(wam, pdata, dv);
  if (c>0) {
    SP_integer lb = -(bige1/c) + dvar_max_l(dv);
    if (dvar_fix_min_l(dv,lb)<0)
      return FALSE;
    cmin = c*dvar_min_l(dv);
  } else {
    SP_integer ub = bige1/(-c) + dvar_min_l(dv);
    if (dvar_fix_max_l(dv,ub)<0)
      return FALSE;
    cmin = c*dvar_max_l(dv);
  }
  CMIN(elt) = cmin;
  decr = cmin-cmin0;
  INTERVAL(elt) -= decr;
  pdata->bigf -= decr;
  trail_var(wam, pdata, elt);
  repair_heap(pdata, elt, key_mem, INTERVAL(elt));
  return TRUE;
}

static SP_BOOL
scalar_product_setmin(Wam wam, struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    int elt = pdata->heap[i];
    Dvar dv = DVAR(elt);

    trail_var(wam, pdata, elt);
    dvar_refresh_lazy(wam, pdata, dv);
    if (dvar_fix_value_l(dv, CMIN(elt)/COEFF(elt))<0)
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_setmax(Wam wam, struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    int elt = pdata->heap[i];
    Dvar dv = DVAR(elt);

    trail_var(wam, pdata, elt);
    dvar_refresh_lazy(wam, pdata, dv);
    if (dvar_fix_value_l(dv, CMAX(elt)/COEFF(elt))<0)
      return FALSE;
  }
  return TRUE;
}

/* assert: c1>1 && c2>1 */
static INLINE SP_integer gcd(SP_integer c1,SP_integer c2)
{
  while (TRUE) {
    if (c1==c2) {
      return c1;
    } else if (c1<c2) {
      if (c1==0)
	return c2;
      else
	c2 %= c1;
    } else {
      if (c2==0)
	return c1;
      else
	c1 %= c2;
    }
  }
}

static SP_BOOL
gcd_check(struct linear_data *pdata)
{
  SP_integer g = ABSCOEFF(pdata->heap[0]);
  int i;
  
  for (i=1; i<pdata->nonground && g!=1; i++) {
    g = gcd(g,ABSCOEFF(pdata->heap[i]));
  }
  pdata->gcd_due = FALSE;
  return (g==1 || pdata->bigf % g == 0);
}

#if SP_ASSERTIONS
/* check that NONGROUND items precede GROUND items */
static SP_BOOL verify_nonground(struct linear_data *pdata)
{
  int i;

  for (i=0; i<pdata->nonground; i++)
    if (!INTERVAL(pdata->heap[i]))
      return FALSE;
  for (; i<pdata->nvars; i++)
    if (INTERVAL(pdata->heap[i]))
      return FALSE;

  return TRUE;
}
#endif

SP_BOOL
fd_linear_filter_fast(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int nvars, niter, i, j, k, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct linear_data *pdata;

/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_static_output_state(wam, &handle, &committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get CX0 */
    nvars = fd_list_length(tvec);	/* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      nvars*sizeof(Dvar) +
      5*nvars*sizeof(SP_integer) +
      2*nvars*sizeof(int);
  
    pdata = Palloc(struct linear_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->refreshed = (Dvar *)ptr;
    ptr += nvars*sizeof(Dvar);
    pdata->term.cmin = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.cmax = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.interval = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.abscoeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->heap = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->vheap = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs((nvars<<1) + 2);
    pdata->trail = SP_malloc(2*nvars*sizeof(int));
    pdata->ttop = pdata->trail;
    pdata->tend = pdata->trail+2*nvars;
    DerefArg(telt,X(0),2);
    pdata->op = GetSmall(telt) & 0x7;
    pdata->destructor = linear_destructor;
    pdata->daemon = linear_daemon;
    pdata->nvars = nvars;
    pdata->stamp = 0;
    DerefArg(telt,X(0),4);	/* get Reif */
    fd_get_var_and_attr(telt,RefAttrReif);
    DerefArg(telt,X(0),3);	/* get RHS */
    pdata->bigf = GetSmall(telt);
    pdata->bige = -GetSmall(telt);
    DerefArg(tvec,X(0),1);	/* get CX0 */
    for (i=0; i<nvars; i++) {
      int elt = i;
      SP_integer c;
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = c = GetSmall(t1);
      ABSCOEFF(i) = (c>=0 ? c : -c);
      fd_get_var_and_attr(telt+WD(1),RefAttr(elt));
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    bvar_init(&pdata->reif, RefAttrReif, RefVarReif);
    bvar_attach_daemon(wam, &pdata->reif, pdata, X(1), fd.functor_val);
    pdata->action = ASK_LT;	/* any ASK is good; sync it later */
    pdata->gcd_due = TRUE;
    pdata->refreshed_top = pdata->refreshed;
    for (i=0; i<nvars; i++) {
      int elt = i;
      Dvar dv = DVAR(elt);
      TAGGED functor;
      dvar_init(dv, RefAttr(elt), RefVar(elt));
      functor = fd.functor_minmax;
      dvar_attach_daemon(wam, dv, pdata, X(1), functor);
      dv->flags |= 0x80;
    }
    for (i=0, j=0, k=nvars; i<nvars; i++) {
      int elt = i;
      SP_WORD c = COEFF(elt);
      Dvar dv = DVAR(elt);

      if (c>0) {
	CMIN(elt) = c*dvar_min_l(dv);
	CMAX(elt) = c*dvar_max_l(dv);
      } else {
	CMAX(elt) = c*dvar_min_l(dv);
	CMIN(elt) = c*dvar_max_l(dv);
      }
      INTERVAL(elt) = CMAX(elt) - CMIN(elt);
      pdata->bigf -= CMIN(elt);
      pdata->bige += CMAX(elt);
      if (dvar_is_integer(dv)) {
	pdata->vheap[elt] = --k;
	pdata->heap[k] = elt;
      } else {
	pdata->vheap[elt] = j;
	pdata->heap[j++] = elt;
      }
    }
    pdata->nonground = j;
    sync_action(pdata);
  }
  
				/* RESUME HERE */
  dvar_export_start(wam);
  while (pdata->refreshed_top > pdata->refreshed) {
    Dvar dv = *--pdata->refreshed_top;
    dv->flags |= 0x80;
  }
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
      goto export;
    }
    break;
  case TELL_LE:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  case TELL_GT:
    if (pdata->bige <= 0) {
      goto ret;
    } else if (pdata->bige==1) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  case TELL_GE:
    if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  case TELL_EQ:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto export;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  case TELL_NE:
  default:
    if (pdata->bigf==0 && pdata->bige==0)
      goto ret;
    break;
  }
  niter = pdata->nonground;	/* don't overstay our welcome */
 loop:
  switch (pdata->action) {
  case TELL_LT:
    if (pdata->nonground>0) {
      int elt = pdata->heap[0];
      if (pdata->bigf <= INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata, elt, 1))
	  goto ret;
	else if (--niter >= 0)
	  goto loop;
      }
    }
    break;
  case TELL_LE:
    if (pdata->nonground>0) {
      int elt = pdata->heap[0];
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata, elt, 0))
	  goto ret;
	else if (--niter >= 0)
	  goto loop;
      }
    }
    break;
  case TELL_GT:
    if (pdata->nonground>0) {
      int elt = pdata->heap[0];
      if (pdata->bige <= INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata, elt, 1))
	  goto ret;
	else if (--niter >= 0)
	  goto loop;
      }
    }
    break;
  case TELL_GE:
    if (pdata->nonground>0) {
      int elt = pdata->heap[0];
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata, elt, 0))
	  goto ret;
	else if (--niter >= 0)
	  goto loop;
      }
    }
    break;
  case TELL_EQ:
    if (pdata->nonground>0) {
      int elt = pdata->heap[0];
      if (pdata->gcd_due && !gcd_check(pdata))
	goto ret;
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata, elt, 0))
	  goto ret;
      }
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata, elt, 0))
	  goto ret;
	else if (--niter >= 0)
	  goto loop;
      }
    }
    break;
  case TELL_NE:
  default:
    if (pdata->nonground==0 && pdata->bigf==0) {
      goto ret;
    } else if (pdata->nonground==1) {
      int elt = pdata->heap[0];
      Dvar dv = DVAR(elt);
	
      if (pdata->bigf % COEFF(elt)==0) { /* RHS a multiple of coefficient */
	dvar_refresh_lazy(wam, pdata, dv);
	dvar_prune_value_l(dv,(pdata->bigf+CMIN(elt))/COEFF(elt));
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

  if (!ent && not_fixpoint(pdata))
    fd_not_fixpoint(wam);
 export:
  while (pdata->refreshed_top > pdata->refreshed) {
    Dvar dv = *--pdata->refreshed_top;
    dvar_export(dv);
    dv->flags |= 0x80;
  }
 ret:
  CTagToArg(X(0),ARG_TTOP) = MakeSmall(pdata->ttop - pdata->trail);
  return ent;
}

struct gcd_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_integer stamp;		/* increases up to backtracking */
  SP_globref refbase;
  SP_integer gcdall;		/* valid during GCD rules */
  SP_integer rhs;
  int nedges;			/* maintained incrementally */
  int nvars;			/* #terms */
  int ntargets;			/* #terms that may be targets */
  int *target;
  int *tloc;
  Dvar dvar;
  struct {
    SP_integer *coeff;		/* ai */
    SP_integer *abscoeff;	/* |ai| */
    SP_integer *gcd;		/* valid during GCD rules */
    int *mate;
  } term;
};

#define GCD(t) (pdata->term.gcd[t])
#define MATE(t) (pdata->term.mate[t])

static void SPCDECL gcd_destructor(void *pdata_v)
{
  struct gcd_data *pdata = (struct gcd_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

/* TRUE iff gcdall>1 or GCD(j)>1 (* and ABSCOEFF(j)>1 *) for some j, */
/* i.e. pruning is possible */
/* DONALD puzzle can prune even if ABSCOEFF(j)==1 */
/* FALSE means that pdata->gcdall and pdata->GCD(_) are undefined */
static SP_BOOL
refresh_gcd_aux(struct gcd_data *pdata)
{
  int ntargets = pdata->ntargets;

  if (ntargets==0 || pdata->nedges>1) {
    return FALSE;
  } else {	/* for each i, compute GCD of all coefficients except i */
    SP_integer g;
    int i;
    int first1;			/* GCD(pdata->target[first1...]) are all 1 */
    int last1;			/* GCD(pdata->target[...last1]) are all 1 */
    SP_BOOL rc = FALSE;
    
    g = ABSCOEFF(pdata->target[0]);
    for (i=1; i<ntargets && g!=1; i++) {
      int elt = pdata->target[i];
      
      GCD(elt) = g;
      g = gcd(g,ABSCOEFF(elt));
    }
    first1 = i;
    pdata->gcdall = g;
				/* now, GCD(pdata->target[i]) = gcd(a[0] ... a[i-1]) */
    rc |= (first1==ntargets);
    if (!rc)
      GCD(pdata->target[ntargets-1]) = 1;
    g = ABSCOEFF(pdata->target[ntargets-1]);
    for (i=ntargets-2; i>0 && g!=1; i--) {
      int elt = pdata->target[i];
      
      GCD(elt) = i>=first1 ? 1 : gcd(g,GCD(elt));
      rc |= (GCD(elt)>1 /* && ABSCOEFF(elt)>1 */);
      g = gcd(g,ABSCOEFF(elt));
    }
    last1 = i;
    GCD(pdata->target[0]) = g;
    rc |= (g>1 /* && ABSCOEFF(pdata->target[0])>1 */);
    if (rc) {
      for (i=1; i<=last1; i++)
	GCD(pdata->target[i]) = 1;
    }
				/* now, GCD(pdata->target[i]) = gcd({a[j] | i!=j}) */
    return rc;
  }
}

static void
gcd_handle_ground(struct gcd_data *pdata,int varno,SP_integer cvalue)
{
  int loc = pdata->tloc[varno];
  int swap = pdata->target[--pdata->ntargets];
  int mate = MATE(varno);
  
  pdata->target[loc] = swap;
  pdata->tloc[swap] = loc;
  pdata->target[pdata->ntargets] = varno;
  pdata->tloc[varno] = pdata->ntargets;
  pdata->rhs -= cvalue;
  if (mate==varno ||		/* self-loop */
      (mate > -1 && pdata->tloc[mate] < pdata->ntargets)) { /* proper edge, mate among targets */
    --pdata->nedges;
  }
}

static DAEMON_RC SPCDECL 
gcd_daemon(Wam wam,
	   void *vdata,
	   SP_globref attr_ref,
	   TAGGED *global)
{
  struct gcd_data *pdata = (struct gcd_data *)vdata;
  SP_BOOL buried = FALSE;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried); 
  int varno = (int)((attr_ref - pdata->refbase)>>1);
  int enable;
  TAGGED tmin, tmax;
  DAEMON_RC rc = DAEMON_FIX;
  
  pdata->rhs = GetSmall(CTagToArg(tstate,2)); /* Sum */
  pdata->ntargets = pdata->nvars - GetSmall_int(CTagToArg(tstate,3)); /* NGround */
  pdata->nedges = GetSmall_int(CTagToArg(tstate,4)); /* NEdges */
  enable = (pdata->nedges<=1 && pdata->gcdall>1);
  REF_GET_BOUNDS(attr_ref, tmin, tmax);
  if (tmin==tmax && pdata->tloc[varno]<pdata->ntargets) { /* if varno not among targets then self-invocation */
    gcd_handle_ground(pdata,varno,COEFF(varno)*GetSmall(tmin));
    CTagToArg(tstate,2) = MakeSmall(pdata->rhs); /* update Sum */
    CTagToArg(tstate,3) = MakeSmall(pdata->nvars-pdata->ntargets); /* update NGround */
    CTagToArg(tstate,4) = MakeSmall(pdata->nedges); /* update NEdges */
  }
  if (pdata->nedges<=1)
    enable = refresh_gcd_aux(pdata);
  if (pdata->ntargets==0 || enable)
    rc = DAEMON_NOFIX;
  return rc;
}

/* Preconditions:
   0<coeff<mod, 0=<rhs<mod, 0<mod

   Solve min X such that coeff*X = rhs (modulo mod)
*/
static SP_integer solve_gcd(SP_integer coeff, SP_integer rhs, SP_integer mod)
{
  if (rhs==0)
    return 0;
  else {
    SP_integer rhs1 = rhs%coeff;
    if (rhs1==0)
      return rhs/coeff;
    else
      return (rhs + mod*solve_gcd(mod%coeff, coeff-rhs1, coeff)) / coeff;
  }
}

/* Preconditions: 
   0<mod, gcdall = gcd(coeff,mod)

   Adjusts minx up and maxx down s.t.
   Returns smallest s.t. coeff*minx = coeff*maxx = rhs (modulo mod)
*/
static void 
adjust_bounds_gcd(SP_integer coeff, SP_integer rhs,
		  SP_integer mod, SP_integer gcdall,
		  SP_integer *minx, SP_integer *maxx)
{
  SP_integer minx0 = *minx, maxx0 = *maxx;
  SP_integer q = mod/gcdall;
  SP_integer r, x, s;
  SP_integer rhslocal;

  if (coeff>0) {
    rhslocal = rhs;
  } else {
    rhslocal = -rhs;
    coeff = -coeff;
  }
  coeff %= mod;
  rhslocal %= mod;
  if (rhslocal<0)		/* ensure mod, not rem */
    rhslocal += mod;
  s = solve_gcd(coeff, rhslocal, mod);
  r = minx0 % q;
  if (r<0)
    r += q;
  x = minx0 - r + s;
  if (x<minx0)
    x += q;
  *minx = x;
  r = maxx0 % q;
  if (r<0)
    r += q;
  x = maxx0 - r + s;
  if (x>maxx0)
    x -= q;
  *maxx = x;
}

/*
  '$fd_gcd_aux'(+State0, -State, -Actions).
  State = state(Vec,Sum,NGround,NEdges,Handle,Stamp) where CX are all non-ground
*/
void SPCDECL
prolog_fd_gcd_aux(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL posted, committed;
  SP_integer state_stamp;
  int nvars, i, j, k, ent = -1, ntargets0;
  SP_integer total_size;
  char *ptr;
  struct gcd_data *pdata;
  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct gcd_data,handle);
  } else {			/* build persistent state */
    posted = TRUE;
    DerefArg(tvec,X(0),1);	  /* get Vec */
    nvars = fd_list_length(tvec); /* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      3*nvars*sizeof(SP_integer) +
      2*nvars*sizeof(int) +
      nvars*sizeof(int);
  
    pdata = Palloc(struct gcd_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.abscoeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.gcd = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.mate = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->destructor = gcd_destructor;
    pdata->daemon = gcd_daemon;
    pdata->nvars = nvars;
    DerefArg(tvec,X(0),1);	/* get Vec */
    DerefArg(telt,X(0),2);	/* get Sum */
    pdata->rhs = GetSmall(telt);
    pdata->nedges = 0;
    for (i=0, j=0, k=nvars; i<nvars; i++) {
      SP_integer c;
      Dvar dv = DVAR(i);
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(i) = c = GetSmall(t1);
      ABSCOEFF(i) = (c>=0 ? c : -c);
      MATE(i) = -1;
      fd_get_var_and_attr(telt+WD(1),RefAttr(i));
      dvar_init(dv, RefAttr(i), RefVar(i));
      if (dvar_is_integer(dv)) {
	pdata->rhs -= c*dvar_min_l(dv);
	pdata->target[--k] = i;
	pdata->tloc[i] = k;
      } else {
	pdata->tloc[i] = j;
	pdata->target[j++] = i;
      }
    }
    pdata->ntargets = k;
    for (i=0; i<k; i++) {
      Dvar dv = DVAR(pdata->target[i]);
      SP_integer aci = ABSCOEFF(i);
      
      dvar_attach_daemon(wam, dv, pdata, X(1), fd.functor_minmax);
      if (aci==1) {
	MATE(i) = i;
	pdata->nedges++;
      } else if (MATE(i) > -1) {
      } else {
	for (j=i+1; j<k; j++) {
	  SP_integer acj = ABSCOEFF(j);
	  if (acj>1 && gcd(aci,acj)==1 && MATE(j)==-1) {
	    MATE(i) = j;
	    MATE(j) = i;
	    pdata->nedges++;
	    break;
	  }
	}
      }	
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free Vec for GC */
    if (!(pdata->nedges<=1 && refresh_gcd_aux(pdata)))
      goto nofilter;
  }
  (void)posted; /* [PM] 4.3 avoid warning about unused assignment */
  
				/* RESUME HERE with pdata->* refreshed by daemon */
  dvar_export_start(wam);
  pdata->stamp = state_stamp+1;
  if (pdata->ntargets==0)
    goto nofilter;
  ntargets0 = pdata->ntargets;	/* the DVARs that we touch in the propagator */
  for (i=0; i<ntargets0; i++) {
    int j = pdata->target[i];
    Dvar dv = DVAR(j);
    dvar_refresh(dv);
  }
  if (pdata->rhs % pdata->gcdall != 0)
    goto ret;
  for (i=0; i<pdata->ntargets; i++) {
    int elt = pdata->target[i];
    Dvar dv = DVAR(elt);
    SP_integer imin = dvar_min_l(dv);
    SP_integer imax = dvar_max_l(dv);
      
    if (imin<imax && GCD(elt)>1) {
      /* Ensure that:

	 Ai * min(Xi) = Ai * max(Xi) = RHS (modulo G)

	 where G is the gcd of all coefficients excepts Ai.
      */
      SP_integer c = COEFF(elt);
      int rc;
      adjust_bounds_gcd(c,
			pdata->rhs,
			GCD(elt),
			pdata->gcdall,
			&imin, &imax);
      rc = dvar_fix_interval_l(dv, imin, imax);
      if (rc<0) {
	goto ret;
      } else if (rc>0) {
	if (dvar_is_integer(dv)) {
	  gcd_handle_ground(pdata,elt,c*dvar_min_l(dv));
	  if (refresh_gcd_aux(pdata)) {
	    fd_not_fixpoint(wam);
	    break;
	  }
	} else if (dvar_min_l(dv) > imin || dvar_max_l(dv) < imax) { /* hit a hole */
	  fd_not_fixpoint(wam);
	  break;
	}
      }
    }
  }

  for (i=0; i<ntargets0; i++)
    dvar_pruning_done( DVAR(pdata->target[i]));
  for (i=0; i<ntargets0; i++)
    dvar_export(DVAR(pdata->target[i]));
 nofilter:
  CTagToArg(X(0),2) = MakeSmall(pdata->rhs); /* update Sum */
  CTagToArg(X(0),3) = MakeSmall(pdata->nvars-pdata->ntargets); /* update NGround */
  CTagToArg(X(0),4) = MakeSmall(pdata->nedges); /* update NEdges */
  ent = (pdata->ntargets==0);
  if (ent==1)
    Pfree;
 ret:
  dvar_export_done(wam, Actions, ent);
}
