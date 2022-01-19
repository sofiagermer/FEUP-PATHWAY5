/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct alldiff_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  SP_integer stamp;			/* increases up to backtracking */
  int nvars;			/* _original_ #terms */
  int *target;
  TAGGED *val;			/* volatile */
  Dvar dvar;
  /* for circuit */
  int *head;
  int *tail;
  int *ref;
  int *trail;
  int *buffer;
  int *ttop;
  int *btop;
  SP_BOOL subcircuit;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define SV(T) (pdata->target[T])
#define VAL(T) (pdata->val[T])
#define DVAR(T) (pdata->dvar+(T))

static void SPCDECL alldiff_destructor(void *pdata_v)
{
  struct alldiff_data *pdata = (struct alldiff_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

/*
  '$fd_all_different'(+State0, +State, -Actions).
  State = state(Vec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_all_different(Wam wam,
			SP_term_ref State0,
			SP_term_ref State,
			SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec, min=0;
  TAGGED infeasible=0;
  SP_integer state_stamp, total_size;
  int i, ntargets, nint, nvars=0;
  struct alldiff_data *pdata;
  SP_BOOL committed;
  int elt;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec);	/* count terms, moving ground terms to RHS */
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    total_size = nvars*(sizeof(int)+sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (alldiff_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
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
      SV(elt) = elt;
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  pdata->val = Malloc(nvars,TAGGED);
  DerefArg(telt,X(0),2);
  ntargets = nvars-GetSmall_int(telt);
  pdata->stamp = state_stamp+1;
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    dvar_refresh(DVAR(elt));
  }
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = SV(sup);		/* sup is the hole */
    Dvar dv;
    
    elt = SV(inf);
    while (inf<=sup) {
      dv = DVAR(elt);
      if (!dvar_is_integer(dv))
	goto nonground;
      VAL(sup) = dvar_min_t(dv);
      SV(sup) = elt;
      sup--;
      elt = (inf>=sup ? held : SV(sup));
      continue;
    nonground:
      SV(inf) = elt;
      inf++;
      elt = (inf>=sup ? held : SV(inf));
    }
    nint = ntargets-inf;
    if (nint==1) {
      min = VAL(inf);
    } else if (nint>1) {
      FDCONS cons;
      
      fd_qsort_asc_tagged(wam, &VAL(inf),nint);
      fdcons_init(&cons);
      min = VAL(inf);
      fdcons_add(wam, &cons,min);
      for (i=inf+1; i<ntargets; i++) {
	if (min==VAL(i))
	  goto ret;
	min = VAL(i);
	fdcons_add(wam, &cons,min);
      }
      infeasible = fdcons_set(&cons);
    }
    if (nint>0) {
      CTagToArg(X(0),2) = MakeSmall(nvars-inf);
      for (i=0; i<inf; i++) {
	Dvar dv = DVAR(SV(i));
	if ((nint==1 ? dvar_prune_interval_t(dv, min,min) : dvar_prune_set(dv, infeasible)) < 0)
	  goto ret;
	dvar_pruning_done(dv);
      }
      for (i=0; i<inf; i++) {
	Dvar dv = DVAR(SV(i));
	dvar_export(dv);
      }
    }
    ent = !inf;
  }
 ret:
  SP_free(pdata->val);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam,Actions, ent);
}

/*
  '$fd_pairing'(+State0, +State, -Actions).
  State = state(XVec,YVec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_pairing(Wam wam,
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec, infeasible, min;
  SP_integer state_stamp, total_size;
  int i, k, ntargets, nxs, nvars=0; /* NB. nvars is _total_ #vars */
  struct alldiff_data *pdata;
  SP_BOOL committed;
  int elt, ok;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec)<<1; /* count terms, moving ground terms to RHS */
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    total_size = nvars*(sizeof(int)+sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (alldiff_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    elt = 0;
    for (i=0; i<2; i++) {
      DerefArg(tvec,X(0),i+1);		/* get XVec */
      while (TagIsLST(tvec)) {
	DerefCar(telt,tvec);
	DerefCdr(tvec,tvec);
	fd_get_var_and_attr(telt,RefAttr(elt));
	dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
	SV(elt) = elt;
	elt++;
      }
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  nxs = nvars>>1;
  DerefArg(telt,X(0),3);
  ntargets = nvars-GetSmall_int(telt);
  pdata->val = Malloc(nvars,TAGGED);
  pdata->stamp = state_stamp+1;
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    dvar_refresh(DVAR(elt));
  }
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = SV(sup);		/* sup is the hole */
    Dvar dv;
    TAGGED val;
    int top[2];

    top[0] = 0;
    top[1] = nxs;
    ok = TRUE;
    elt = SV(inf);
    while (inf<=sup) {
      dv = DVAR(elt);
      if (!ok || !dvar_is_integer_first(dv))
	goto nonground;
      val = dvar_min_t(dv);
      if (elt<nxs) {
	VAL(top[0]++) = val;
	if (dvar_fix_value_l(DVAR(GetSmall(val)+nxs-1),elt+1) <0)
	  ok = FALSE;
      } else {
	VAL(top[1]++) = val;
	if (dvar_fix_value_l(DVAR(GetSmall(val)-1),elt-nxs+1) <0)
	  ok = FALSE;
      }
      SV(sup) = elt;
      sup--;
      elt = (inf>=sup ? held : SV(sup));
      continue;
    nonground:
      SV(inf) = elt;
      inf++;
      elt = (inf>=sup ? held : SV(inf));
    }
    if (!ok)
      goto ret;
    for (k=0; k<2; k++) {
      int bos = k*nxs;
      int nvals = top[k]-bos;
      if (nvals>0) {	/* typically, nvals is a very small integer */
	if (nvals==1) {
	  min = VAL(bos);
	  infeasible = fd_interval(wam,min,min);
	} else {
	  FDCONS cons;
	
	  fd_qsort_asc_tagged(wam, &VAL(bos),nvals);
	  fdcons_init(&cons);
	  min = VAL(bos);
	  fdcons_add(wam, &cons,min);
	  for (i=1; i<nvals; i++) {
	    if (min==VAL(bos+i))
	      goto ret;
	    min = VAL(bos+i);
	    fdcons_add(wam, &cons,min);
	  }
	  infeasible = fdcons_set(&cons);
	}
	for (i=0; i<inf; i++) {
	  elt = SV(i);
	  if ((elt<nxs)^(k==1))
	    if (dvar_prune_set(DVAR(elt),infeasible)<0)
	      goto ret;
	}
      }
    }
    CTagToArg(X(0),3) = MakeSmall(nvars-inf);
    for (i=0; i<inf; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_pruning_done(dv);
    }
    for (i=0; i<inf; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_export(dv);
    }
    ent = (inf==0);
  }
 ret:
  SP_free(pdata->val);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam,Actions, ent);
}

#define ARG_TTOP 2

static DAEMON_RC
circuit_do_binding(Wam wam, struct alldiff_data *pdata, int var)
{
  Dvar dv = DVAR(var);
  DAEMON_RC rc = DAEMON_NOFIX;
  int tovar, hvar, tvar;
  (void)wam;
  
  dvar_refresh(dv);
  tovar = dvar_min_int(dv)-1;
  if (pdata->ref[tovar])
    return DAEMON_FAIL;		/* not all different */
  hvar = pdata->head[var];
  tvar = pdata->tail[tovar];
  if (hvar==pdata->head[tvar]) { /* [sub]circuit */
    if (hvar==tvar)	       /* self-loop - just trail, no action */
      rc = DAEMON_FIX;
  }
  pdata->ref[tovar] = 1;
  pdata->tail[hvar] = tvar;
  pdata->head[tvar] = hvar;
  *pdata->ttop++ = var;
  *pdata->ttop++ = tovar;
  if (rc != DAEMON_FIX)
    *pdata->btop++ = hvar;
  return DAEMON_NOFIX;
}

static DAEMON_RC SPCDECL 
circuit_daemon(Wam wam,
	       void *vdata,
	       SP_globref attr_ref,
	       TAGGED *global)
{
  struct alldiff_data *pdata = (struct alldiff_data *)vdata;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  int var = (int)(attr_ref - pdata->refbase) >> 1;
  SP_integer state_stamp;
  SP_BOOL buried;
  int ar;
  
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    int *btr = pdata->trail + GetSmall(CTagToArg(tstate,ARG_TTOP));
    
    pdata->stamp = state_stamp;
    pdata->btop = pdata->buffer;
    while (pdata->ttop != btr) { /* undo fromvar->tovar binding */
      int fromvar = pdata->ttop[-2];
      int tovar = pdata->ttop[-1];
      int headvar = pdata->head[fromvar];
      int tailvar = pdata->tail[tovar];

      pdata->ref[tovar] = 0;
      pdata->tail[headvar] = fromvar;
      pdata->head[tailvar] = tovar;
      pdata->ttop -= 2;
    }
  }
  (void)fd_daemon_copy_state(wam, global,&buried);
  if (!buried)
    CTagToArg(tstate,ar) -= IStep(1);
  else
    pdata->stamp++;	/* increase iff old and new states are separated by a choicepoint */
  return circuit_do_binding(wam, pdata, var);
}

/* returns ent = -1/0/1 */
static int
circuit_common(Wam wam,
	       SP_term_ref State0,
	       SP_term_ref State,
	       SP_BOOL subcircuit)
{
  TAGGED handle, telt, tvec;
  SP_integer state_stamp;
  int i, nvars, jvar;
  size_t total_size;
  struct alldiff_data *pdata;
  SP_BOOL committed;
  char *ptr;
  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec);	/* count terms, moving ground terms to RHS */
    if (nvars==0) {
      return 1;
    }
    total_size = 
      nvars*(6*sizeof(int)+
	     sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data,total_size,handle); /* GC, clobbers tvec */
    pdata->destructor = (alldiff_destructor);
    pdata->daemon = circuit_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->head = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tail = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->ref = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->trail = (int *)ptr;
    ptr += 2*nvars*sizeof(int);
    pdata->buffer = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->ttop = pdata->trail;
    pdata->btop = pdata->buffer;
    pdata->subcircuit = subcircuit;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    i = 0;
    DerefArg(tvec,X(0),1);		/* get XVec */
    while (TagIsLST(tvec)) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      i++;
    }
    for (i=0; i<nvars; i++) {
      pdata->head[i] = i;
      pdata->tail[i] = i;
      pdata->ref[i] = 0;
      dvar_attach_daemon(wam, DVAR(i), pdata, X(1), fd.functor_val);
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    /* initial processing of bound vars */
    for (i=0; i<nvars; i++) {
      Dvar dv = DVAR(i);
      
      dvar_refresh(dv);
      if (dvar_is_integer(dv))
	circuit_do_binding(wam, pdata, i);
    }
  }

#define pathhead(I) pdata->head[I]==(I) && pdata->head[pdata->tail[I]]==(I)
#define pathtail(I) pdata->tail[I]==(I) && pdata->tail[pdata->head[I]]==(I)


  /* RESUME HERE */
  dvar_export_start(wam);
  for (i=0; i<nvars; i++)
    if (pathtail(i))
      dvar_refresh(DVAR(i));
  while (pdata->btop > pdata->buffer) { /* prevent tvar->hvar */
    int ivar = *--pdata->btop;		/* get potential head */

    if (pathhead(ivar)) { /* real head */
      if (pdata->ref[ivar]) {			 /* closed [sub]circuit */
	for (jvar=0; jvar<nvars; jvar++) {
	  if (ivar != jvar && pathhead(jvar)) { /* another head */
	    if (!subcircuit) {
	      return -1;
	    } else if (pdata->tail[jvar] != jvar) {
	      return -1;
	    } else if (dvar_fix_value_l(DVAR(jvar), jvar+1)<0) { /* it must become a self-loop */
	      return -1;
	    }
	  }
	}
      } else {			/* open [sub]circuit */
	int tvar = pdata->tail[ivar];
	if (!subcircuit) {
	  SP_BOOL more = FALSE;
	  for (jvar=0; jvar<nvars; jvar++)
	    if (jvar != tvar && pathtail(jvar))
	      more = TRUE;
	  if (more && dvar_prune_value_l(DVAR(tvar), ivar+1)<0) { /* unless single tail, prevent tail->head */
	    return -1;
	  }
	} else {
	  SP_BOOL more = FALSE;
	  for (jvar=0; jvar<nvars; jvar++) {
	    if (jvar != tvar && pathtail(jvar) && pdata->head[jvar] != jvar) {
	      more = TRUE;
	      if (dvar_prune_value_l(DVAR(jvar), pdata->head[jvar]+1)<0) /* prevent tail->head of another path */
		return -1;
	    }
	  }
	  if (more && dvar_prune_value_l(DVAR(tvar), ivar+1)<0) /* unless single non-singleton tail, prevent tail->head */
	    return -1;
	}
      }
    }
  }
	      
  CTagToArg(X(0),ARG_TTOP) = MakeSmall(pdata->ttop - pdata->trail);
  for (i=0; i<nvars; i++)
    if (pathtail(i))
      dvar_pruning_done(DVAR(i));
  for (i=0; i<nvars; i++)
    if (pathtail(i))
      dvar_export(DVAR(i));
  if (pdata->ttop - pdata->trail >= 2*(nvars-1)) /* at most one var left */
    return 1;
  else
    return 0;
}

/*
  '$fd_circuit'(+State0, +State, -Actions).
  State = state(XVec,TrailTop,Handle,Stamp)
*/
void SPCDECL
prolog_fd_circuit(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  int ent;

  ent = circuit_common(wam, State0, State, FALSE);
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}

/*
  '$fd_subcircuit'(+State0, +State, -Actions).
  State = state(XVec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_subcircuit(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  int ent;

  ent = circuit_common(wam, State0, State, TRUE);
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}

