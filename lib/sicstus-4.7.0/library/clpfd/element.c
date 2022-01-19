/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct int_element_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  int flags;			/* 2 for DC */
  struct dvar dv_index;
  struct dvar dv_value;
  TAGGED *v;
};

static void SPCDECL int_element_destructor(void *pdata_v)
{
  struct int_element_data *pdata = (struct int_element_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,4);
  SP_free(pdata);
}

struct var_bool_element_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  int flags;
  int alpha, beta;
  struct dvar dv_index;
  struct bvar bv_value;
  struct bvar *bvar;
};

static void SPCDECL var_bool_element_destructor(void *pdata_v)
{
  struct var_bool_element_data *pdata = (struct var_bool_element_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,2*pdata->n+4);
  SP_free(pdata);
}

struct var_int_element_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  int flags;
  int alpha, beta;
  struct dvar dv_index;
  struct dvar dv_value;
  struct dvar *dvar;
};

static void SPCDECL var_int_element_destructor(void *pdata_v)
{
  struct var_int_element_data *pdata = (struct var_int_element_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,2*pdata->n+4);
  SP_free(pdata);
}

static int
var_bool_element_eq(Wam wam, Bvar x, Bvar y)
{
  unsigned int xmask = (unsigned int)((x->min+5) | (x->max+5)) >> 3;
  unsigned int ymask = (unsigned int)((y->min+5) | (y->max+5)) >> 3;

  switch ((xmask<<2) + ymask) {
  case 0x5:
  case 0xa:
    return 1;
  case 0x7:
  case 0xb:
    bvar_export_value(wam, y, GetSmall_int(x->min));
    return 1;
  case 0xd:
  case 0xe:
    bvar_export_value(wam, x, GetSmall_int(y->min));
    return 1;
  case 0xf:
    return 0;
  }
  return -1;
}

static int
var_int_element_eq(Wam wam, Dvar x, Dvar y)
{
  int rc1 = dvar_fix_set(x, dvar_set(y));
  int rc2 = dvar_fix_set(y, dvar_set(x));
  
  return (rc1>=0 && rc2>=0);
}

static int
fd_int_element(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct int_element_data *pdata;
  SP_globref refbase;

  RefTerm(State) = fd_static_output_state(wam, &handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct int_element_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),3);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      (n+1)*sizeof(TAGGED);
  
    fd.gdata = pdata = Palloc(struct int_element_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->v = (TAGGED *)ptr;
    ptr += (n+1)*sizeof(TAGGED);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    pdata->refbase = refbase = SP_alloc_globrefs(4);
    pdata->destructor = int_element_destructor;
    DerefArg(telt,X(0),4);	/* get Flags */
    pdata->flags = GetSmall_int(telt);
    DerefArg(telt,X(0),1);	/* get Index */
    fd_get_var_and_attr(telt,refbase);
    dvar_init(&pdata->dv_index, refbase, refbase+1);    
    DerefArg(telt,X(0),2);	/* get Value */
    fd_get_var_and_attr(telt,refbase+2);
    dvar_init(&pdata->dv_value, refbase+2, refbase+3);    
    DerefArg(tvec,X(0),3);	/* get Array */
    for (i=1; i<=n; i++) {
      DerefCar(pdata->v[i],tvec);
      DerefCdr(tvec,tvec);
    }
  }

  /* RESUME */
  dvar_export_start(wam);
  dvar_refresh(&pdata->dv_index);
  dvar_refresh(&pdata->dv_value);
  
  if (dvar_is_integer(&pdata->dv_index)) {
    if (dvar_fix_value_t(&pdata->dv_value, pdata->v[dvar_min_l(&pdata->dv_index)])<0)
      return -1;
    ent = 1;
  } else if (dvar_is_integer(&pdata->dv_value)) {
    TAGGED key = dvar_min_t(&pdata->dv_value);
    SP_integer imin, imax;
    FDCONS icons;
    DVITER it;
    
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++) {
	if (pdata->v[i] == key)
	  fdcons_add(wam, &icons, MakeSmall(i));
      }
    }
    if (dvar_fix_set(&pdata->dv_index, fdcons_set(&icons))<0)
      return -1;
    ent = 1;
  } else if (pdata->flags & IStep(2)) {			/* general DC case */
    DVITER it;
    FDCONS icons, vcons;
    TAGGED *tarray;
    SP_integer imin, imax;
    SP_BOOL sorted = TRUE;
    int nt = 0;
    int nv = (int)dvar_value_count(wam,&pdata->dv_index);
    
    NumstackAlloc(nv,tarray);
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++) {
	TAGGED v = pdata->v[i];
	if (!dvar_contains_value_t(&pdata->dv_value,v)) {
	  fdcons_add(wam, &icons, MakeSmall(i));
	} else if (nt==0) {
	  tarray[nt++] = v;
	} else if (v<tarray[nt-1]) {
	  sorted = FALSE;
	  tarray[nt++] = v;
	} else if (v>tarray[nt-1]) {
	  tarray[nt++] = v;
	}
      }
    }
    
    if (!sorted)
      fd_qsort_asc_tagged(wam,tarray,nt);
    fdcons_init(&vcons);
    for (i=0; i<nt; i++)
      if (i==0 || tarray[i-1] != tarray[i])
	fdcons_add(wam, &vcons, tarray[i]);
    if (nt==0 ||
	dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0 ||
	dvar_fix_set(&pdata->dv_value, fdcons_set(&vcons))<0)
      return -1;
    else
      ent = (dvar_is_integer(&pdata->dv_value) ? 1 : 0);
  } else {			/* general BC case */
    DVITER it;
    FDCONS icons;
    TAGGED vmin = dvar_max_t(&pdata->dv_value);
    TAGGED vmax = dvar_min_t(&pdata->dv_value);
    SP_integer imin, imax;
    
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++) {
	TAGGED v = pdata->v[i];
	if (!dvar_contains_value_t(&pdata->dv_value,v)) {
	  fdcons_add(wam, &icons, MakeSmall(i));
	} else {
	  vmin = Tlt(vmin,v) ? vmin : v;
	  vmax = Tgt(vmax,v) ? vmax : v;
	}
      }
    }
    
    if (Tgt(vmin,vmax) ||
	dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0 ||
	dvar_fix_interval_t(&pdata->dv_value, vmin, vmax)<0)
      return -1;
    else
      ent = (dvar_is_integer(&pdata->dv_value) ? 1 : 0);
  }
  
  dvar_pruning_done(&pdata->dv_index);
  dvar_pruning_done(&pdata->dv_value);
  dvar_export(&pdata->dv_index);
  dvar_export(&pdata->dv_value);
    
  return ent;
}

static DAEMON_RC SPCDECL 
var_bool_element_repair(struct var_bool_element_data *pdata,
			int alpha,
			int beta)
{
  DVITER it;
  FDCONS icons;

  fdcons_init(&icons);
  dviter_init(&it, &pdata->dv_index);
  while (!dviter_empty(&it) && (alpha==0 || beta==0)) {
    SP_integer ival = dviter_next_value_l(&it);
    Bvar bv = pdata->bvar+ival;
      
    bvar_refresh(bv);
    if (bv->min == TaggedZero) alpha = (int)ival;
    if (bv->max == TaggedOne) beta = (int)ival;
  }
  pdata->alpha = alpha;
  pdata->beta = beta;
  return alpha==0 ? DAEMON_NOFIX : beta==0 ? DAEMON_NOFIX : DAEMON_FIX;
}

static DAEMON_RC SPCDECL 
var_bool_element_daemon(Wam wam,
			void *vdata,
			SP_globref attr_ref,
			TAGGED *global)
{
  struct var_bool_element_data *pdata = (struct var_bool_element_data *)vdata;
  Bvar pivot;
  int varno = (int)((attr_ref - pdata->refbase)>>1) - 1;
  (void)wam;
  (void)global;

  dvar_refresh(&pdata->dv_index);
  bvar_refresh(&pdata->bv_value);
  pivot = pdata->bvar + varno;
  bvar_refresh(pivot);
  
  if (dvar_is_integer(&pdata->dv_index))
    return DAEMON_NOFIX;
  else if (bvar_is_integer(&pdata->bv_value) && pdata->bv_value.min != pivot->min)
    return DAEMON_NOFIX;
  else if (!bvar_is_integer(&pdata->bv_value) && varno==pdata->beta && pivot->min==TaggedZero)
    return var_bool_element_repair(pdata, pdata->alpha, 0);
  else if (!bvar_is_integer(&pdata->bv_value) && varno==pdata->alpha && pivot->min==TaggedOne)
    return var_bool_element_repair(pdata, 0, pdata->beta);
  else
    return DAEMON_FIX;
}

static int
fd_var_bool_element(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct var_bool_element_data *pdata;
  SP_globref refbase, r;
  Bvar pivot = 0;

  RefTerm(State) = fd_static_output_state(wam, &handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct var_bool_element_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),3);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      (n+1)*sizeof(struct bvar);
  
    fd.gdata = pdata = Palloc(struct var_bool_element_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->bvar = (struct bvar *)ptr;
    ptr += (n+1)*sizeof(struct bvar);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    DerefArg(telt,X(0),4);	/* get Flags */
    pdata->flags = GetSmall_int(telt);
    pdata->refbase = refbase = SP_alloc_globrefs(2*n+4);
    pdata->destructor = var_bool_element_destructor;
    pdata->daemon = var_bool_element_daemon;
    DerefArg(telt,X(0),1);	/* get Index */
    fd_get_var_and_attr(telt,refbase);
    dvar_init(&pdata->dv_index, refbase, refbase+1);    
    DerefArg(telt,X(0),2);	/* get Value */
    fd_get_var_and_attr(telt,refbase+2);
    bvar_init(&pdata->bv_value, refbase+2, refbase+3);
    r = refbase+4;
    DerefArg(tvec,X(0),3);	/* get Array */
    for (i=1; i<=n; i++, r+=2) {
      struct bvar *bvar = pdata->bvar + i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,r);
      bvar_init(bvar, r, r+1);
    }
    for (i=1; i<=n; i++) {
      bvar_attach_daemon(wam, pdata->bvar+i, pdata, X(1), functor_dom1);
    }
  }

  /* RESUME */
  dvar_export_start(wam);
  dvar_refresh(&pdata->dv_index);
  bvar_refresh(&pdata->bv_value);

  if (dvar_is_integer(&pdata->dv_index)) {
    pivot = pdata->bvar + dvar_min_l(&pdata->dv_index);
    bvar_refresh(pivot);
    ent = var_bool_element_eq(wam, &pdata->bv_value, pivot);
  } else {			/* general case */
    DVITER it;
    FDCONS icons;
    unsigned int vmask = (unsigned int)((pdata->bv_value.min+5) | (pdata->bv_value.max+5)) >> 3;
    unsigned int support = 0;
    int alpha=0, beta=0;

    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      SP_integer ival = dviter_next_value_l(&it);
      Bvar bv = pdata->bvar+ival;
      unsigned int xmask;
      
      bvar_refresh(bv);
      xmask = (unsigned int)((bv->min+5) | (bv->max+5)) >> 3;
      if (!(vmask & xmask)) {
	fdcons_add(wam, &icons, MakeSmall(ival));
      } else {
	support |= xmask;
	if (xmask & 0x1) alpha = (int)ival;
	if (xmask & 0x2) beta  = (int)ival;
      }
    }
    if (dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0) {
      return -1;
    } else if (dvar_is_integer(&pdata->dv_index)) {
      pivot = pdata->bvar + dvar_min_l(&pdata->dv_index);
      ent = var_bool_element_eq(wam, pivot, &pdata->bv_value);
    } else if (support == 0x2 && !bvar_export_value(wam, &pdata->bv_value, 1)) {
      return -1;
    } else if (support == 0x1 && !bvar_export_value(wam, &pdata->bv_value, 0)) {
      return -1;
    } else {
      ent = (support != 0x3);
    }
    pdata->alpha = alpha;
    pdata->beta = beta;
  }
  dvar_pruning_done(&pdata->dv_index);
  dvar_export(&pdata->dv_index);
    
  return ent;
}

static DAEMON_RC SPCDECL 
var_int_element_repair(struct var_int_element_data *pdata,
			int alpha,
			int beta)
{
  DVITER it;
  FDCONS icons;
  
  fdcons_init(&icons);
  dviter_init(&it, &pdata->dv_index);
  while (!dviter_empty(&it) && (alpha==0 || beta==0)) {
    SP_integer ival = dviter_next_value_l(&it);
    Dvar dv = pdata->dvar+ival;
      
    dvar_refresh(dv);
    if (alpha==0 && dvar_contains_value_t(dv, pdata->dv_value.min))
      alpha = (int)ival;
    if (beta==0 && dvar_contains_value_t(dv, pdata->dv_value.max))
      beta = (int)ival;
  }
  pdata->alpha = alpha;
  pdata->beta = beta;
  
  return alpha==0 ? DAEMON_NOFIX : beta==0 ? DAEMON_NOFIX : DAEMON_FIX;
}

static DAEMON_RC SPCDECL 
var_int_element_daemon(Wam wam,
		       void *vdata,
		       SP_globref attr_ref,
		       TAGGED *global)
{
  struct var_int_element_data *pdata = (struct var_int_element_data *)vdata;
  Dvar pivot;
  int varno = (int)((attr_ref - pdata->refbase)>>1) - 1;
  int ok = 1;
  (void)wam;
  (void)global;

  dvar_refresh(&pdata->dv_index);
  dvar_refresh(&pdata->dv_value);
  pivot = pdata->dvar + varno;
  dvar_refresh(pivot);
  
  if (dvar_is_integer(&pdata->dv_index) ||
      pdata->alpha==0 ||
      pdata->beta==0 ||
      (dvar_contains_value_l(&pdata->dv_index, varno) &&
       dvar_compare_set(pivot, pdata->dv_value.set)==FDI_DISJOINT))
    return DAEMON_NOFIX;
  if (varno==pdata->alpha && !dvar_contains_value_t(pivot, pdata->dv_value.min))
    pdata->alpha = ok = 0;
  if (varno==pdata->beta && !dvar_contains_value_t(pivot, pdata->dv_value.max))
    pdata->beta = ok = 0;
  if (ok)
    return DAEMON_FIX;
  else
    return var_int_element_repair(pdata, pdata->alpha, pdata->beta);
}

static SP_BOOL
dvars_intersection_min_max(Dvar x, Dvar y, TAGGED *tmin, TAGGED *tmax)
{
  if (dvar_is_integer(x)) {
    if (!dvar_contains_value_t(y, x->min)) {
      return FALSE;
    } else {
      *tmin = *tmax = x->min;
      return TRUE;
    }
  } else if (dvar_is_integer(y)) {
    if (!dvar_contains_value_t(x, y->min)) {
      return FALSE;
    } else {
      *tmin = *tmax = y->min;
      return TRUE;
    }
  } else {
    TAGGED lmin = fd_intersection_min(x->set, y->set);

    if (lmin==ERRORTAG) {
      return FALSE;
    } else {
      *tmin = lmin;
      *tmax = fd_intersection_max(x->set, y->set);
      return TRUE;
    }
  }
}

static int
fd_var_int_element(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct var_int_element_data *pdata;
  SP_globref refbase, r;
  Dvar pivot = 0;

  RefTerm(State) = fd_static_output_state(wam, &handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct var_int_element_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),3);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      (n+1)*sizeof(struct dvar);
  
    fd.gdata = pdata = Palloc(struct var_int_element_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->dvar = (struct dvar *)ptr;
    ptr += (n+1)*sizeof(struct dvar);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    DerefArg(telt,X(0),4);	/* get Flags */
    pdata->flags = GetSmall_int(telt);
    pdata->refbase = refbase = SP_alloc_globrefs(2*n+4);
    pdata->destructor = var_int_element_destructor;
    pdata->daemon = var_int_element_daemon;
    DerefArg(telt,X(0),1);	/* get Index */
    fd_get_var_and_attr(telt,refbase);
    dvar_init(&pdata->dv_index, refbase, refbase+1);    
    DerefArg(telt,X(0),2);	/* get Value */
    fd_get_var_and_attr(telt,refbase+2);
    dvar_init(&pdata->dv_value, refbase+2, refbase+3);
    r = refbase+4;
    DerefArg(tvec,X(0),3);	/* get Array */
    for (i=1; i<=n; i++, r+=2) {
      struct dvar *dvar = pdata->dvar + i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,r);
      dvar_init(dvar, r, r+1);
    }
    for (i=1; i<=n; i++) {
      dvar_attach_daemon(wam, pdata->dvar+i, pdata, X(1), functor_dom1);
    }
  }

  /* RESUME */
  dvar_export_start(wam);
  dvar_refresh(&pdata->dv_index);
  dvar_refresh(&pdata->dv_value);

  if (dvar_is_integer(&pdata->dv_index)) {
    pivot = pdata->dvar + dvar_min_l(&pdata->dv_index);
    dvar_refresh(pivot);
    if (!var_int_element_eq(wam, pivot, &pdata->dv_value))
      return -1;
    ent = dvar_is_integer(pivot) ? 1 : 0;
  } else {			/* general case */
    DVITER it;
    FDCONS icons;
    TAGGED vmin = Sup, vmax = Inf;
    int alpha=0, beta=0;

    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      SP_integer ival = dviter_next_value_l(&it);
      Dvar dv = pdata->dvar+ival;
      TAGGED tmin, tmax;

      dvar_refresh(dv);
      if (!dvars_intersection_min_max(dv, &pdata->dv_value, &tmin, &tmax)) {
	fdcons_add(wam, &icons, MakeSmall(ival));
      } else {
	if (dvar_is_integer(dv)) { /* alpha/beta: prefer ground vars */
	  if (FDle(tmin,vmin)) {
	    vmin = tmin;
	    alpha = (int)ival;
	  }
	  if (FDge(tmax,vmax)) {
	    vmax = tmax;
	    beta = (int)ival;
	  }
	} else {
	  if (FDlt(tmin,vmin)) {
	    vmin = tmin;
	    alpha = (int)ival;
	  }
	  if (FDgt(tmax,vmax)) {
	    vmax = tmax;
	    beta = (int)ival;
	  }
	}
      }
    }
    if (FDgt(vmin,vmax) ||
	dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0 ||
	dvar_fix_interval_t(&pdata->dv_value, vmin, vmax)<0) {
      return -1;
    } else if (dvar_is_integer(&pdata->dv_index)) {
      pivot = pdata->dvar + dvar_min_l(&pdata->dv_index);
      if (!var_int_element_eq(wam, pivot, &pdata->dv_value))
	return -1;
      ent = dvar_is_integer(pivot) ? 1 : 0;
    } else {
      ent = 0;
    }
    pdata->alpha = alpha;
    pdata->beta = beta;
  }

  dvar_pruning_done(&pdata->dv_index);
  dvar_pruning_done(&pdata->dv_value);
  if (pivot)
    dvar_pruning_done(pivot);
  dvar_export(&pdata->dv_index);
  dvar_export(&pdata->dv_value);
  if (pivot)
    dvar_export(pivot);
    
  return ent;
}

/*
  '$fd_element'(+State0, +State, -Actions).
  State = state(Value,Array,Flags,Handle,Stamp)
  !(Flags&1) -> Array is a list of Var-Mutable
  (Flags&1) -> Array is a list of integers
  !(Flags&2) -> don't maintain DC
  (Flags&2) -> maintain DC
  !(Flags&4) -> Value and Array are any
  (Flags&4) -> Value and Array are Boolean
*/
void SPCDECL
prolog_fd_element(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  int ent;
  TAGGED telt;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  DerefArg(telt,X(0),4);

/*    X(0) = RefTerm(State0); */
  if (telt & IStep(1))
    ent = fd_int_element(wam, State);
  else if (telt & IStep(4))
    ent = fd_var_bool_element(wam, State);
  else
    ent = fd_var_int_element(wam, State);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

#define BVAR_SIGNATURE(x) ((unsigned int)(((x)->min+5) | ((x)->max+5)) >> 3)

struct var_bool_member_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  int flags;
  int watch0;
  int watch1;
  struct bvar bv_value;
  struct bvar *bvar;
};

static void SPCDECL var_bool_member_destructor(void *pdata_v)
{
  struct var_bool_member_data *pdata = (struct var_bool_member_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,2*pdata->n+2);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
var_bool_member_daemon(Wam wam,
		      void *vdata,
		      SP_globref attr_ref,
		      TAGGED *global)
{
  struct var_bool_member_data *pdata = (struct var_bool_member_data *)vdata;
  int varno = (int)((attr_ref - pdata->refbase)>>1) - 1;
  int i, watch01 = -1;
  unsigned int vsign, xsign;

  (void)wam;
  (void)global;
  bvar_refresh(&pdata->bv_value);
  bvar_refresh(pdata->bvar+varno);
  vsign = BVAR_SIGNATURE(&pdata->bv_value);
  xsign = BVAR_SIGNATURE(pdata->bvar+varno);
  switch ((vsign<<4) + xsign) {
  case 0x11:
  case 0x22:
    return DAEMON_ENTAIL;
  case 0x12:
  case 0x21:
    for (i=0; i<pdata->n; i++) {
      if (i != varno) {
	Bvar pv = pdata->bvar + i;
	bvar_refresh(pv);
	if (!bvar_is_integer(pv)) {
	  if (watch01>=0)
	    return DAEMON_FIX;
	  else
	    watch01 = i;
	}
      }
    }
    return DAEMON_NOFIX;
  case 0x31:
    if (pdata->watch1>=0) {
      Bvar pv = pdata->bvar + pdata->watch1;
      bvar_refresh(pv);
      if (pv->min==TaggedOne)
	return DAEMON_ENTAIL;
    }
    pdata->watch1 = -1;
    for (i=0; i<pdata->n; i++) {
      if (i != varno) {
	Bvar pv = pdata->bvar + i;
	bvar_refresh(pv);
	if (pv->min==TaggedOne)
	  return DAEMON_ENTAIL;
	else if (pv->max==TaggedOne)
	  watch01 = i;
      }
    }
    return watch01>=0 ? DAEMON_FIX : DAEMON_NOFIX;
  case 0x32:
    if (pdata->watch0>=0) {
      Bvar pv = pdata->bvar + pdata->watch0;
      bvar_refresh(pv);
      if (pv->max==TaggedZero)
	return DAEMON_ENTAIL;
    }
    pdata->watch0 = -1;
    for (i=0; i<pdata->n; i++) {
      if (i != varno) {
	Bvar pv = pdata->bvar + i;
	bvar_refresh(pv);
	if (pv->max==TaggedZero)
	  return DAEMON_ENTAIL;
	else if (pv->min==TaggedZero)
	  watch01 = i;
      }
    }
    return watch01>=0 ? DAEMON_FIX : DAEMON_NOFIX;
  default:
    return DAEMON_FIX;
  }
}

static int
fd_var_bool_member(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, watch0, watch1, nb01, varno=-1;
  unsigned int vsign, xsign;
  SP_integer total_size;
  struct var_bool_member_data *pdata;
  SP_globref refbase, r;
  char *ptr;
  
  RefTerm(State) = fd_static_output_state(wam, &handle,&committed);
  
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct var_bool_member_data,handle);
    n = pdata->n;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),2);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      n*sizeof(struct bvar);
  
    fd.gdata = pdata = Palloc(struct var_bool_member_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->bvar = (struct bvar *)ptr;
    ptr += n*sizeof(struct bvar);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    DerefArg(telt,X(0),3);	/* get Flags */
    pdata->flags = GetSmall_int(telt);
    pdata->refbase = refbase = SP_alloc_globrefs(2*n+2);
    pdata->destructor = var_bool_member_destructor;
    pdata->daemon = var_bool_member_daemon;
    DerefArg(telt,X(0),1);	/* get Value */
    fd_get_var_and_attr(telt,refbase);
    bvar_init(&pdata->bv_value, refbase, refbase+1);
    r = refbase+2;
    DerefArg(tvec,X(0),2);	/* get Array */
    for (i=0; i<n; i++, r+=2) {
      struct bvar *bvar = pdata->bvar + i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,r);
      bvar_init(bvar, r, r+1);
    }
    for (i=0; i<n; i++) {
      bvar_attach_daemon(wam, pdata->bvar+i, pdata, X(1), functor_dom1);
    }
  }

  /* RESUME */
  dvar_export_start(wam);
  bvar_refresh(&pdata->bv_value);
  vsign = BVAR_SIGNATURE(&pdata->bv_value);

  /* propagator core */

  watch0 = watch1 = -1;
  nb01 = 0;
  for (i=0; i<n; i++) {
    bvar_refresh(pdata->bvar + i);
    xsign = BVAR_SIGNATURE(pdata->bvar + i);
    switch ((vsign<<4) + xsign) {
    case 0x11:
      return 1;			/* ENTAIL */
    case 0x12:
      watch1 = i;
      break;
    case 0x21:
      watch0 = i;
      break;
    case 0x22:
      return 1;			/* ENTAIL */
    case 0x31:
      if (watch1>=0) return 1;	/* ENTAIL? */
      watch0 = i;
      break;
    case 0x32:
      if (watch0>=0) return 1;	/* ENTAIL? */
      watch1 = i;
      break;
    default:
      nb01++;
      varno = i;
      break;
    }
  }

  
  switch (vsign) {
  case 0x1:
    if (nb01==0) {
      return -1;	/* FAIL */
    } else if (nb01==1) {
      return bvar_export_value(wam, pdata->bvar+varno, 0) ? 1 : -1;
    }
    break;
  case 0x2:
    if (nb01==0) {
      return -1;	/* FAIL */
    } else if (nb01==1) {
      return bvar_export_value(wam, pdata->bvar+varno, 1) ? 1 : -1;
    }
    break;
  case 0x3:
    if (nb01==0) {
      return bvar_export_value(wam, &pdata->bv_value, watch0<0) ? 1 : -1;
    }
    break;
  }
  pdata->watch0 = watch0;
  pdata->watch1 = watch1;
  return 0;
}

struct var_int_member_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  int ntarget;
  int flags;
  struct dvar dv_value;
  struct dvar *dvar;
  int *target;
  int *buffer;
  int *index;
};

static void SPCDECL var_int_member_destructor(void *pdata_v)
{
  struct var_int_member_data *pdata = (struct var_int_member_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,2*pdata->n+2);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
var_int_member_daemon(Wam wam,
		      void *vdata,
		      SP_globref attr_ref,
		      TAGGED *global)
{
  struct var_int_member_data *pdata = (struct var_int_member_data *)vdata;
  int varno = (int)((attr_ref - pdata->refbase)>>1) - 1;
  (void)wam;
  (void)global;

  return (pdata->index[varno] < pdata->ntarget) ? DAEMON_NOFIX : DAEMON_FIX;
}

static int
fd_var_int_member(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle, r1, r2;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, p, q, ent = -1;
  SP_integer total_size, state_stamp;
  struct var_int_member_data *pdata;
  SP_globref refbase, r;
  char *ptr;

  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct var_int_member_data,handle);
    n = pdata->n;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),2);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      n*sizeof(struct dvar) +
      3*n*sizeof(int);
  
    fd.gdata = pdata = Palloc(struct var_int_member_data, total_size, handle); /* GC, clobbers tvec */
    ptr = (char *)(pdata+1);
    pdata->dvar = (struct dvar *)ptr;
    ptr += n*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += n*sizeof(int);
    pdata->buffer = (int *)ptr;
    ptr += n*sizeof(int);
    pdata->index = (int *)ptr;
    ptr += n*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    pdata->ntarget = n;
    DerefArg(telt,X(0),3);	/* get Flags */
    pdata->flags = GetSmall_int(telt);
    pdata->refbase = refbase = SP_alloc_globrefs(2*n+2);
    pdata->destructor = var_int_member_destructor;
    pdata->daemon = var_int_member_daemon;
    DerefArg(telt,X(0),1);	/* get Value */
    fd_get_var_and_attr(telt,refbase);
    dvar_init(&pdata->dv_value, refbase, refbase+1);
    r = refbase+2;
    DerefArg(tvec,X(0),2);	/* get Array */
    for (i=0; i<n; i++, r+=2) {
      struct dvar *dvar = pdata->dvar + i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,r);
      dvar_init(dvar, r, r+1);
      pdata->target[i] = i;
    }
    for (i=0; i<n; i++) {
      dvar_attach_daemon(wam, pdata->dvar+i, pdata, X(1), functor_dom1);
    }
  }

  /* RESUME */
  DerefArg(telt,X(0),4);
  pdata->ntarget = GetSmall_int(telt);
  dvar_export_start(wam);
  dvar_refresh(&pdata->dv_value);

  /* propagator core */
  
  p = 0;
  q = pdata->ntarget;
  r1 = r2 = pdata->dv_value.set;
  for (i=0; i<pdata->ntarget; i++) {
    int varno = pdata->target[i];
    Dvar dv = pdata->dvar+varno;

    dvar_refresh(dv);
    if (dvar_is_integer(dv)) {
      if (dvar_contains_value_t(&pdata->dv_value, dv->min)) {
	pdata->index[varno] = p;
	pdata->buffer[p++] = varno;
	r1 = fd_delete(wam, r1, dv->min);
	r2 = fd_delete(wam, r2, dv->min);
	if (r2==EmptySet)
	  return 1;
      } else {
	pdata->buffer[--q] = varno;
	pdata->index[varno] = q;
      }
    } else {
      if (dvar_compare_set(dv, pdata->dv_value.set)!=FDI_DISJOINT) {
	pdata->index[varno] = p;
	pdata->buffer[p++] = varno;
	r1 = fd_subtract(wam, r1, dv->set);
      } else {
	pdata->buffer[--q] = varno;
	pdata->index[varno] = q;
      }
    }
  }

  if (p==0)
    return -1;

  memcpy(pdata->target, pdata->buffer, pdata->ntarget*sizeof(int));
    
  if (p==1) {
    int varno = pdata->target[0];
    if (!var_int_element_eq(wam, pdata->dvar+varno, &pdata->dv_value))
      return -1;
    else if (dvar_is_integer(&pdata->dv_value))
      ent = 1;
    else
      ent = 0;
  } else if (r1!=EmptySet && dvar_prune_set(&pdata->dv_value, r1)<0) {
    return -1;
  } else {
    ent = (r1==r2 || fd_compare(r1,r2)==FDI_EQUAL);
  }

  if (p==1)
    dvar_pruning_done(pdata->dvar+pdata->target[0]);
  dvar_pruning_done(&pdata->dv_value);
  if (p==1)
    dvar_export(pdata->dvar+pdata->target[0]);
  dvar_export(&pdata->dv_value);
    
  pdata->ntarget = p;
  CTagToArg(X(0),4) = MakeSmall(p);
  return ent;
}

/*
  '$fd_member'(+State0, +State, -Actions).
  State = state(Value,Array,Flags,Nin,Handle,Stamp)
  !(Flags&1) -> Value and Array are any
  (Flags&1) -> Value and Array are Boolean
*/
void SPCDECL
prolog_fd_member(Wam wam,
		 SP_term_ref State0,
		 SP_term_ref State,
		 SP_term_ref Actions)
{
  int ent;
  TAGGED telt;
  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  DerefArg(telt,X(0),3);

/*    X(0) = RefTerm(State0); */
  if (telt & IStep(1))
    ent = fd_var_bool_member(wam, State);
  else
    ent = fd_var_int_member(wam, State);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

