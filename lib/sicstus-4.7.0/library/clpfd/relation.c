/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct minmax_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  int nrefs;
  int *target;
  Dvar dvar;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define TARGET(T) (pdata->target[T])
#define DVAR(T) (pdata->dvar+(T))
#define BVAR(T) (pdata->bvar+(T))

static void SPCDECL minmax_destructor(void *pdata_v)
{
  struct minmax_data *pdata = (struct minmax_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* Daemon for minimum/2: effectively disable Xi that is for sure > Y */
/* Daemon for maximum/2: effectively disable Xi that is for sure < Y */
static DAEMON_RC SPCDECL 
minmax_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct minmax_data *pdata = (struct minmax_data *)vdata;
  TAGGED tstate;
  int varno, i, ntargets;

  (void)wam;
  varno = (int)((attr_ref - pdata->refbase)>>1);
  tstate = RefMutable(CTagToArg(*global,1));
  DerefArg(tstate,tstate,3);	/* get NTargets */
  ntargets = GetSmall_int(tstate);
  for (i=0; i<ntargets; i++)
    if (TARGET(i)==varno)
      return DAEMON_NOFIX;
  return DAEMON_FIX;
}

/*
  '$fd_minmax'(+State0, +State, -Actions).
  State = state(Y,Xs,NTargets,IsMax,Handle,Stamp)
  IsMax is 0 -> minimum/2
  IsMax is 1 -> maximum/2
*/
void SPCDECL
prolog_fd_minmax(Wam wam,
			       SP_term_ref State0,
			       SP_term_ref State,
			       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, tismax;
  TAGGED ymin, ymax;
  SP_integer state_stamp, total_size;
  int i, ntargets, count, pivot=0;
  struct minmax_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  DerefArg(tvec,X(0),3);	/* get NTargets */
  DerefArg(tismax,X(0),4);	/* get IsMax */
  ntargets = GetSmall_int(tvec);
  if (ntargets==0) {
    goto ret;
  } else if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_data,handle);
  } else {			/* build persistent state */
    total_size = (ntargets+1)*(sizeof(int)+sizeof(struct dvar));
    pdata = Palloc(struct minmax_data, total_size, handle); /* GC */
    pdata->destructor = minmax_destructor;
    pdata->daemon = minmax_daemon;
    pdata->nrefs = (ntargets+1)<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs((ntargets+1)<<1);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (ntargets+1)*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += (ntargets+1)*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);		/* get Y */
    fd_get_var_and_attr(tvec,RefAttr(0));
    dvar_init(DVAR(0), RefAttr(0), RefVar(0));
    DerefArg(tvec,X(0),2);		/* get Xs */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      TARGET(i) = i+1;
      fd_get_var_and_attr(telt,RefAttr(i+1));
    }
    for (i=0; i<ntargets; i++) {
      dvar_init(DVAR(i+1), RefAttr(i+1), RefVar(i+1));
      dvar_attach_daemon(wam, DVAR(i+1), pdata, X(1), fd.functor_minmax); /* [MC] 4.2.3: can GC */
    }
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  dvar_refresh(DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_refresh(DVAR(elt));
  }
 loop:
  count = 0;
  ymin = dvar_min_t(DVAR(0));
  ymax = dvar_max_t(DVAR(0));
  if (tismax==TaggedZero) {
    TAGGED minofmax = Sup;
    TAGGED minofmin = Sup;
    int rc;
    for (i=0; i<ntargets; i++) {
      int elt = TARGET(i);
      Dvar dv = DVAR(elt);
      if (dvar_fix_min_t(dv, ymin)<0) {
	goto ret;
      } else if (FDle(dvar_min_t(dv),ymax)) {
	count++;
	pivot = i;
	if (FDgt(minofmax,dvar_max_t(dv)))
	  minofmax = dvar_max_t(dv);
	if (FDgt(minofmin,dvar_min_t(dv)))
	  minofmin = dvar_min_t(dv);
      }
    }
    rc = dvar_fix_interval_t(DVAR(0), minofmin, minofmax);
    if (rc<0)
      goto ret;
    else if (rc>0)
      goto loop;
  } else {
    TAGGED maxofmax = Inf;
    TAGGED maxofmin = Inf;
    int rc;
    for (i=0; i<ntargets; i++) {
      int elt = TARGET(i);
      Dvar dv = DVAR(elt);
      if (dvar_fix_max_t(dv, ymax)<0) {
	goto ret;
      } else if (FDle(ymin,dvar_max_t(dv))) {
	count++;
	pivot = i;
	if (FDlt(maxofmax,dvar_max_t(dv)))
	  maxofmax = dvar_max_t(dv);
	if (FDlt(maxofmin,dvar_min_t(dv)))
	  maxofmin = dvar_min_t(dv);
      }
    }
    rc = dvar_fix_interval_t(DVAR(0), maxofmin, maxofmax);
    if (rc<0)
      goto ret;
    else if (rc>0)
      goto loop;
  }
  if (count==0) {
    goto ret;
  } else if (count==1) {
    Dvar dv = DVAR(TARGET(pivot));
    if (dvar_fix_set(DVAR(0),dvar_set(dv))<0 ||
	dvar_fix_set(dv,dvar_set(DVAR(0)))<0)
      goto ret;
    ymin = dvar_min_t(DVAR(0));
    ymax = dvar_max_t(DVAR(0));
  }
  dvar_pruning_done( DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_pruning_done( DVAR(elt));
  }
  dvar_export( DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_export( DVAR(elt));
  }
  /* Maintain targets, the Xs that can potentially equal Y */
  /* Entailment condition: integer(Y) and at least one X equals Y */
  {
    int inf = 0, hit = 0;
    int sup = ntargets-1;
    int held = TARGET(sup);		/* sup is the hole */
    int current = TARGET(inf);
    
    while (inf<=sup) {
      TAGGED xmin = dvar_min_t(DVAR(current));
      TAGGED xmax = dvar_max_t(DVAR(current));
      if (tismax==TaggedZero ? FDge(ymax,xmin) : FDle(ymin,xmax)) {
	if (ymin==ymax && xmin==ymin && xmax==ymax)
	  hit = 1;
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    CTagToArg(X(0),3) = MakeSmall(inf);
    if (inf>0) ent = hit;
  }
  if (ent==1)
    Pfree;
 ret:
  dvar_export_done(wam,Actions, ent);
}

struct bool_channel_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  int ntargets;
  int nrefs;
  SP_integer stamp;
  int top;			/* #items done */
  int offset;
  int code;
  Dvar dvar;
  Bvar bvar;
  int *target;			/* [ items done | items to do ] */
  int *tloc;			/* place of item in target */
  char *sense;
};

static void SPCDECL bool_channel_destructor(void *pdata_v)
{
  struct bool_channel_data *pdata = (struct bool_channel_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
bool_channel_daemon(Wam wam,
		    void *vdata,
		    SP_globref attr_ref,
		    TAGGED *global)
{
  struct bool_channel_data *pdata = (struct bool_channel_data *)vdata;
  int ar, varno, state_stamp;
  TAGGED tstate;

  if (pdata->code & 2) {	/* #=, #\= */
    varno = (int)((attr_ref - pdata->refbase)>>1);
    tstate = RefMutable(CTagToArg(*global,1));
    ar = Arity(TagToHeadfunctor(tstate));
    state_stamp = GetSmall_int(CTagToArg(tstate,ar));
    if (pdata->stamp!=state_stamp) { /* non-incremental */
      pdata->top = pdata->ntargets;
    }
    if (pdata->top==pdata->ntargets) {
      SP_BOOL buried;
      (void)fd_daemon_copy_state(wam, global,&buried);
      pdata->stamp = state_stamp+1;
    }
    if (varno>0) {
      int loc = pdata->tloc[varno];
      if (loc < pdata->top) {	/* otherwise, self-invocation */
	int swap = pdata->target[--pdata->top];
	pdata->target[loc] = swap;
	pdata->tloc[swap] = loc;
	pdata->target[pdata->top] = varno;
	pdata->tloc[varno] = pdata->top;
      }
    }
  }
  return DAEMON_NOFIX;
}


static int bool_channel_eq(Wam wam, struct bool_channel_data *pdata,int sense)
{
  int pivot = 0, j, ent = -1;
  TAGGED tvec, tmin, tmax, curdom, prevdom;
  FDITER it;
  
  dvar_refresh(DVAR(0));
  for (j=pdata->top; j<pdata->ntargets; j++) {
    int varno = pdata->target[j];
    REF_GET_BOUNDS(RefAttr(varno), tmin, tmax);
    if (tmin==tmax) { /* can be false when posting */
      if (Tgtz(tmin)^pdata->sense[varno]^sense) {
	if (dvar_fix_value_l(DVAR(0),varno+pdata->offset-1)<0)
	  goto ret;
      } else {
	if (dvar_prune_value_l(DVAR(0),varno+pdata->offset-1)<0)
	  goto ret;
      }
    }
  }
  if (dvar_is_integer(DVAR(0))) {
    pivot = dvar_min_int(DVAR(0))-pdata->offset+1;
    if (pivot<1 || pivot>=pdata->ntargets)
      pivot = 0;
  }
  curdom = fd_localize(wam,dvar_set(DVAR(0)));
  DerefArg(prevdom, X(0), 6);
  tvec = fd_subtract(wam, prevdom, curdom); /* prev domain \ cur domain */
  tvec = fd_localize(wam,tvec);				       /* protect from GC */
  dvar_pruning_done( DVAR(0));
  dvar_export( DVAR(0));
  fditer_init(&it, tvec);
  while (!fditer_empty(&it)) {
    int varno = GetSmall_int(fditer_next(&it))-pdata->offset+1;
    if (!bvar_export_value(wam, BVAR(varno), pdata->sense[varno]^sense))
      goto ret;
  }  
  if (pivot) {
    if (!bvar_export_value(wam, BVAR(pivot), pdata->sense[pivot]^sense^1))
      goto ret;
  }
  ent = !!dvar_is_integer(DVAR(0));
  if (!ent) {			/* update "prev domain" - slight overkill */
    TAGGED newdom = fd_intersection_interval(wam, curdom,
					     MakeSmall(pdata->offset),
					     MakeSmall(pdata->offset+pdata->ntargets-2));
    newdom = fd_globalize(wam,newdom,0,3); /* GC */
    CTagToArg(X(0),6) = newdom;
  }
 ret:
  pdata->top = pdata->ntargets;
  return ent;
}

/* sense=0 for #=<, 1 for #>= */
static int bool_channel_le(Wam wam,
			   struct bool_channel_data *pdata,
			   int sense, int yoff) 
{
  int i, ent = -1;
  int start01, start1, new01, new1; /* N.B. 1-based */
  SP_integer yminl, ymaxl;
  TAGGED ymint, ymaxt, tmin, tmax;

  dvar_refresh(DVAR(0));
  DerefArg(tmin, X(0), 6);
  new01 = start01 = GetSmall_int(tmin);
  DerefArg(tmin, X(0), 7);
  new1 = start1 = GetSmall_int(tmin);

  /* prune from Xs to Y */
  for (i=start01; i<start1; i++) {
    REF_GET_BOUNDS(RefAttr(i), tmin, tmax);
    if (tmin==tmax && Tgtz(tmin)==(pdata->sense[i]^sense))
      new01 = i+1;
  }
  for (i=start1-1; i>=start01; i--) {
    REF_GET_BOUNDS(RefAttr(i), tmin, tmax);
    if (tmin==tmax && Tgtz(tmin)!=(pdata->sense[i]^sense))
      new1 = i;
  }
  if (start01<new01 && start1>new1) { /* found '0' and '1' */
    if (dvar_fix_interval_l(DVAR(0),
			    new01+pdata->offset-yoff-1,
			    new1+pdata->offset-yoff-1)<0)
      goto ret;
  } else if (start01<new01) { /* found '0' */
    if (dvar_fix_min_l(DVAR(0), new01+pdata->offset-yoff-1)<0)
      goto ret;
  } else if (start1>new1) { /* found '1' */
    if (dvar_fix_max_l(DVAR(0), new1+pdata->offset-yoff-1)<0)
      goto ret;
  }
  dvar_pruning_done( DVAR(0));
  dvar_export( DVAR(0));
  /* prune from Y to Xs */
  ymint = dvar_min_t(DVAR(0));
  ymaxt = dvar_max_t(DVAR(0));
  yminl = dvar_min_l(DVAR(0));
  ymaxl = dvar_max_l(DVAR(0));
  if (AreSmall(ymint,ymaxt) &&
      yminl >= pdata->offset-yoff &&
      ymaxl <= pdata->offset-yoff+pdata->ntargets-2) {
    new01 = (int)yminl-pdata->offset+yoff+1;
    new1 = (int)ymaxl-pdata->offset+yoff+1;
    for (i=start01; i<new01; i++)
      if (!bvar_export_value(wam, BVAR(i), pdata->sense[i]^sense))
	goto ret;
    for (i=new1; i<start1; i++)
      if (!bvar_export_value(wam, BVAR(i), pdata->sense[i]^sense^1))
	goto ret;
  } else if (IsSmall(ymint) && yminl >= pdata->offset-yoff) {
    new01 = new1 < yminl-pdata->offset+yoff+1 ? new1 : (int)yminl-pdata->offset+yoff+1;
    for (i=start01; i<new01; i++)
      if (!bvar_export_value(wam, BVAR(i), pdata->sense[i]^sense))
	goto ret;
  } else if (IsSmall(ymaxt) && ymaxl <= pdata->offset-yoff+pdata->ntargets-2) {
    new1 = new01 > ymaxl-pdata->offset+yoff+1 ? new01 : (int)ymaxl-pdata->offset+yoff+1;
    for (i=new1; i<start1; i++)
      if (!bvar_export_value(wam, BVAR(i), pdata->sense[i]^sense^1))
	goto ret;
  }
  CTagToArg(X(0),6) = MakeSmall(new01);
  CTagToArg(X(0),7) = MakeSmall(new1);
  ent = (new01==new1);
 ret:
  return ent;
}


/*
  FlatZinc accelerator:
  '$fd_bool_channel'(+State0, +State, -Actions).
  State = state(XAs,XSs,Y-YA,Offet,YSet,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bool_channel(Wam wam,
		       SP_term_ref State0,
		       SP_term_ref State,
		       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, svec, susp;
  SP_integer state_stamp, total_size;
  int i, ntargets;
  struct bool_channel_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  /*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bool_channel_data,handle);
    ntargets = pdata->ntargets;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Xs */
    ntargets = fd_list_length(tvec)+1;
    total_size =
      sizeof(struct dvar) +
      ntargets*sizeof(struct bvar) +
      2*ntargets*sizeof(int) +
      ntargets;
    pdata = Palloc(struct bool_channel_data, total_size, handle); /* GC, clobbers tvec */
    pdata->ntargets = ntargets;
    pdata->destructor = bool_channel_destructor;
    pdata->daemon = bool_channel_daemon;
    pdata->nrefs = ntargets<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(ntargets<<1);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->bvar = (Bvar)ptr;
    ptr += ntargets*sizeof(struct bvar);
    pdata->target = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->sense = ptr;
    ptr += ntargets;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    pdata->stamp = 0;
    DerefArg(tvec,X(0),1);		/* get Xs */
    DerefArg(svec,X(0),2);		/* get Ss */
    for (i=1; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      DerefCar(telt,svec);
      DerefCdr(svec,svec);
      pdata->sense[i] = (char)GetSmall(telt);
    }
    DerefArg(tvec,X(0),3);		/* get Y */
    fd_get_var_and_attr(tvec,RefAttr(0));
    DerefArg(tvec,X(0),4);		/* get Code */
    pdata->code = GetSmall_int(tvec);
    DerefArg(tvec,X(0),5);		/* get Offset */
    pdata->offset = GetSmall_int(tvec);
    susp = pdata->code & 2 ? functor_dom1 : fd.functor_minmax;
    dvar_init(DVAR(0), RefAttr(0), RefVar(0)); /* bvar */
    dvar_attach_daemon(wam, DVAR(0), pdata, X(1), susp); /* bvar, [MC] 4.2.3: can GC */
    for (i=1; i<ntargets; i++) {
      bvar_init(BVAR(i), RefAttr(i), RefVar(i));
      bvar_attach_daemon(wam, BVAR(i), pdata, X(1), susp); /* [MC] 4.2.3: can GC */
    }
    for (i=0; i<ntargets; i++)
      pdata->target[i] = pdata->tloc[i] = i;
    pdata->top = 1;
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  pdata->stamp = state_stamp+1;
  switch (pdata->code) {
  case 0:
    ent = bool_channel_le(wam, pdata,0,0); /* #=< */
    break;
  case 1:
    ent = bool_channel_le(wam, pdata,1,1); /* #>= */
    break;
  case 2:
    ent = bool_channel_eq(wam, pdata,0); /* #= */
    break;
  case 3:
    ent = bool_channel_eq(wam, pdata,1); /* #\= */
    break;
  case 4:
    ent = bool_channel_le(wam, pdata,0,1); /* #< */
    break;
  case 5:
    ent = bool_channel_le(wam, pdata,1,0); /* #> */
    break;
  }
  if (ent==1) {
    Pfree;
  }
  dvar_export_done(wam, Actions, ent);
}

struct bool_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  int ntargets;
  int nonground;
  int ac;
  int xone;
  int yzero;
  int nrefs;
  Bvar bvar;
  int *target;
  int *tloc;
  char *sense;
};

static void SPCDECL bool_destructor(void *pdata_v)
{
  struct bool_data *pdata = (struct bool_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
bool_or_daemon(Wam wam,
	       void *vdata,
	       SP_globref attr_ref,
	       TAGGED *global)
{
  struct bool_data *pdata = (struct bool_data *)vdata;
  int varno = (int)((attr_ref - pdata->refbase)>>1);
  int loc, swap;
  TAGGED tmin, tmax;
  SP_BOOL buried;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried);
  int nonground = pdata->ntargets - GetSmall_int(CTagToArg(tstate,3));
  DAEMON_RC rc = DAEMON_FIX;
    
  if (pdata->nonground < nonground) { /* we have backtracked */
    pdata->xone = pdata->yzero = FALSE;
  }
  pdata->nonground = nonground;
  loc = pdata->tloc[varno];
  swap = pdata->target[--pdata->nonground];
  pdata->target[loc] = swap;
  pdata->tloc[swap] = loc;
  pdata->target[pdata->nonground] = varno;
  pdata->tloc[varno] = pdata->nonground;

  REF_GET_BOUNDS(attr_ref, tmin, tmax);
  (void)tmax;
  if (varno==0 && (Teqz(tmin) ^ pdata->sense[varno])) {
    pdata->yzero = TRUE;
  } else if (varno>0 && (Tgtz(tmin) ^ pdata->sense[varno])) {
    pdata->xone = TRUE;
  }
  CTagToArg(tstate,3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  if (pdata->nonground<=1 || pdata->yzero || pdata->xone)
    rc = DAEMON_NOFIX;
  return rc;
}


/*
  FlatZinc accelerator:
  '$fd_bool_or'(+State0, +State, -Actions).
  State = state(Xs,Ss,NGround,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bool_or(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, svec;
  SP_integer total_size, state_stamp;
  int i, j, k, ntargets;
  struct bool_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  /*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bool_data,handle);
    ntargets = pdata->ntargets;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Xs */
    ntargets = fd_list_length(tvec);
    total_size = ntargets*(sizeof(struct bvar)+1) + 2*ntargets*sizeof(int);
    pdata = Palloc(struct bool_data, total_size, handle); /* GC, clobbers tvec */
    pdata->ntargets = ntargets;
    pdata->destructor = bool_destructor;
    pdata->daemon = bool_or_daemon;
    pdata->nrefs = ntargets<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(ntargets<<1);
    ptr = (char *)(pdata+1);
    pdata->target = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->bvar = (Bvar)ptr;
    ptr += ntargets*sizeof(struct bvar);
    pdata->sense = ptr;
    ptr += ntargets;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);		/* get Xs */
    DerefArg(svec,X(0),2);		/* get Ss */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      DerefCar(telt,svec);
      DerefCdr(svec,svec);
      pdata->sense[i] = (char)GetSmall(telt);
    }
    pdata->yzero = pdata->xone = FALSE;
    for (i=0, j=0, k=ntargets; i<ntargets; i++) {
      bvar_init(BVAR(i), RefAttr(i), RefVar(i));
      bvar_attach_daemon(wam, BVAR(i), pdata, X(1), fd.functor_minmax); /* [MC] 4.2.3: can GC */
      if (bvar_is_integer(BVAR(i))) {
	pdata->tloc[i] = --k;
	pdata->target[k] = i;
	if (i==0 && ((bvar_min_l(BVAR(i))==0) ^ pdata->sense[i])) {
	  pdata->yzero = TRUE;
	} else if (i>0 && ((bvar_min_l(BVAR(i))>0) ^ pdata->sense[i])) {
	  pdata->xone = TRUE;
	}
      } else {
	pdata->tloc[i] = j;
	pdata->target[j++] = i;
      }
    }
    pdata->nonground = j;
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  if (pdata->xone) {
    if (pdata->yzero) {
      goto ret;
    } else {
      if (!bvar_export_value(wam, BVAR(0), pdata->sense[0]^1)) /* may be 1 already */
	goto ret;
      ent = 1;
    }
  } else if (pdata->yzero) {
    for (i=0; i<pdata->nonground; i++) {
      int elt = pdata->target[i];
      if (!bvar_export_value(wam, BVAR(elt), pdata->sense[elt]))
	goto ret;
    }
    ent = 1;
  } else if (pdata->nonground==0) {
    TAGGED tmin, tmax;
    REF_GET_BOUNDS(RefAttr(0), tmin, tmax);
    (void)tmax;
    if (Tgtz(tmin) ^ pdata->sense[0])
      goto ret;
    ent = 1;
  } else if (pdata->nonground==1) {
    int elt = pdata->target[0];
    if (!bvar_export_value(wam, BVAR(elt), (elt>0)^pdata->sense[elt]))
      goto ret;
    ent = 1;
  } else {
    ent = 0;
  }
 ret:
  pdata->xone = pdata->yzero = FALSE;
  CTagToArg(X(0),3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  if (ent==1) {
    Pfree;
  }
  dvar_export_done(wam, Actions, ent);
}

static DAEMON_RC SPCDECL 
bool_xor_daemon(Wam wam,
		void *vdata,
		SP_globref attr_ref,
		TAGGED *global)
{
  struct bool_data *pdata = (struct bool_data *)vdata;
  int varno = (int)((attr_ref - pdata->refbase)>>1);
  int loc, swap;
  TAGGED tmin, tmax;
  SP_BOOL buried;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried);
  DAEMON_RC rc = DAEMON_FIX;
  
  pdata->nonground = pdata->ntargets - GetSmall_int(CTagToArg(tstate,3));
  pdata->ac = GetSmall_int(CTagToArg(tstate,4));
  loc = pdata->tloc[varno];
  swap = pdata->target[--pdata->nonground];
  pdata->target[loc] = swap;
  pdata->tloc[swap] = loc;
  pdata->target[pdata->nonground] = varno;
  pdata->tloc[varno] = pdata->nonground;

  REF_GET_BOUNDS(attr_ref, tmin, tmax);
  (void)tmax;
  pdata->ac += (Tgtz(tmin) ^ pdata->sense[varno]);
  CTagToArg(tstate,3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  CTagToArg(tstate,4) = MakeSmall(pdata->ac); /* update NGround */
  if (pdata->nonground<=1)
    rc = DAEMON_NOFIX;
  return rc;
}


/*
  FlatZinc accelerator:
  '$fd_bool_xor'(+State0, +State, -Actions).
  State = state(Xs,Ss,NGround,Ac,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bool_xor(Wam wam,
		   SP_term_ref State0,
		   SP_term_ref State,
		   SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, svec;
  SP_integer total_size, state_stamp;
  int i, j, k, ntargets;
  struct bool_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  /*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bool_data,handle);
    ntargets = pdata->ntargets;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Xs */
    ntargets = fd_list_length(tvec);
    total_size = ntargets*(sizeof(struct bvar)+1) + 2*ntargets*sizeof(int);
    pdata = Palloc(struct bool_data, total_size, handle); /* GC, clobbers tvec */
    pdata->ntargets = ntargets;
    pdata->destructor = bool_destructor;
    pdata->daemon = bool_xor_daemon;
    pdata->nrefs = ntargets<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(ntargets<<1);
    ptr = (char *)(pdata+1);
    pdata->target = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->bvar = (Bvar)ptr;
    ptr += ntargets*sizeof(struct bvar);
    pdata->sense = ptr;
    ptr += ntargets;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);		/* get Xs */
    DerefArg(svec,X(0),2);		/* get Ss */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      DerefCar(telt,svec);
      DerefCdr(svec,svec);
      pdata->sense[i] = (char)GetSmall(telt);
    }
    pdata->ac = 0;
    for (i=0, j=0, k=ntargets; i<ntargets; i++) {
      bvar_init(BVAR(i), RefAttr(i), RefVar(i));
      bvar_attach_daemon(wam, BVAR(i), pdata, X(1), fd.functor_minmax); /* [MC] 4.2.3: can GC */
      if (bvar_is_integer(BVAR(i))) {
	pdata->tloc[i] = --k;
	pdata->target[k] = i;
	pdata->ac += (bvar_min_int(BVAR(i)) ^ pdata->sense[i]);
      } else {
	pdata->tloc[i] = j;
	pdata->target[j++] = i;
      }
    }
    pdata->nonground = j;
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  if (pdata->nonground==0) {
    if (pdata->ac & 1)
      goto ret;
    ent = 1;
  } else if (pdata->nonground==1) {
    int elt = pdata->target[0];
    if (!bvar_export_value(wam, BVAR(elt), (pdata->ac&1)^pdata->sense[elt]))
      goto ret;
    ent = 1;
  } else {
    ent = 0;
  }
  CTagToArg(X(0),3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  CTagToArg(X(0),4) = MakeSmall(pdata->ac); /* update Ac */
  if (ent==1) {
    Pfree;
  }
 ret:
  dvar_export_done(wam, Actions, ent);
}

static SP_BOOL propagate_binary_clause(Wam wam, 
				       TAGGED tval,
				       TAGGED otherpair,
				       TAGGED entflag) 
{
  TAGGED attr, var, dom;

  DerefArg(var, otherpair, 1);	/* Var */
  DerefArg(attr, otherpair, 2);	/* Attribute */
  DomFromAttr(dom,attr);
  if (DomainSize(dom)==TaggedOne) { /* already fixed */
    if (DomainMin(dom)==tval)
      goto entailed;
    else
      return FALSE;
  } else {
    request_tell_bool(wam, attr, var, tval);
  }

 entailed:
  BindHVA(entflag,TaggedOne);
  fd.resumptions++;
  fd.entailments++;
  return TRUE;  
}

static SP_BOOL propagate_nary_clause(Wam wam, 
				     TAGGED watching,
				     TAGGED entflag,
				     TAGGED clause)
{
  TAGGED watch1 = CTagToArg(clause,1);
  TAGGED watch2 = CTagToArg(clause,2);
  TAGGED poslits = CTagToArg(clause,5);
  TAGGED neglits = CTagToArg(clause,6);
  TAGGED var=0, tval, attr;
  int no=0, free=0, wix, other;

  if (watching==watch1) {
    wix = 1;
    other = GetSmall_int0(watch2);
  } else if (watching==watch2) {
    wix = 2;
    other = GetSmall_int0(watch1);
  } else {
    return TRUE;
  }
  tval = other>0 ? TaggedOne : TaggedZero;
  while (TagIsLST(poslits) && !free) {
    TAGGED vapair = CTagToCar(poslits);
    poslits = CTagToCdr(poslits);
    no++;
    DerefArg(attr, vapair, 2);	/* Attribute */
    DomFromAttr(attr,attr);
    if (Teqz(DomainMin(attr))) {
      if (Tnez(DomainMax(attr))) { /* still free */
	if (no==other)
	  var = vapair;
	else
	  free = no;
      }
    } else {			/* fixed 1 */
      goto entailed;
    }
  }
  no = 0;
  while (TagIsLST(neglits) && !free) {
    TAGGED vapair = CTagToCar(neglits);
    neglits = CTagToCdr(neglits);
    no--;
    DerefArg(attr, vapair, 2);	/* Attribute */
    DomFromAttr(attr,attr);
    if (Teqz(DomainMin(attr))) {
      if (Tnez(DomainMax(attr))) { /* still free */
	if (no==other)
	  var = vapair;
	else
	  free = no;
      } else {		/* fixed 0 */
	goto entailed;
      }
    }
  }
  if (free) {
    CTagToArg(clause,wix) = MakeSmall(free); /* update watch */
  } else if (var!=0) {
    DerefArg(attr, var, 2);
    DerefArg(var, var, 1);
    request_tell_bool(wam, attr, var, tval);
    goto entailed;
  } else {
    return FALSE;
  }
  return TRUE;

 entailed:
  BindHVA(entflag,TaggedOne);
  fd.resumptions++;
  fd.entailments++;
  return TRUE;  
}

static void
vapairs_bump_afc(TAGGED vapairs)
{
  while (TagIsLST(vapairs)) {
    TAGGED vapair = CTagToCar(vapairs);
    TAGGED attr;
    
    vapairs = CTagToCdr(vapairs);
    DerefArg(attr, vapair, 2); /* Attribute */
    AttrAFC(attr) += IStep(1);
  }
}

SP_BOOL propagate_disequations(Wam wam, TAGGED list)
{
  DECL_UPDATE_MUTABLE;
  X(0) = list;			/* X(0) is preserved over any GC here */

  while (TagIsLST(X(0))) {
    TAGGED disequation = CTagToCar(X(0));
    TAGGED countm = /* CTagToArg(disequation,1) */ disequation+WD(5);
    TAGGED count = RefMutable(countm);

    FD_UPDATE_MUTABLE(count-IStep(1),countm);
    if (count == MakeSmall(2)) {
      TAGGED coeffs = CTagToArg(disequation,2);
      TAGGED vapairs = CTagToArg(disequation,3);
      TAGGED rhs = GetSmall(CTagToArg(disequation,4));
      TAGGED c=0, var=0, attr=0;

      while (TagIsLST(coeffs)) {
	TAGGED coeff, vapair, attr;
	coeff = CTagToCar(coeffs);
	coeffs = CTagToCdr(coeffs);
	vapair = CTagToCar(vapairs);
	vapairs = CTagToCdr(vapairs);
	DerefArg(attr, vapair, 2); /* Attribute */
	DomFromAttr(attr,attr);
	if (DomainSize(attr)==MakeSmall(2)) { /* the single free var */
	  var = vapair;
	  c = coeff;
	} else if (Tgtz(DomainMin(attr))) {
	  rhs -= GetSmall(coeff);
	}
      }
  
      if (var!=0) {
	TAGGED tval = (rhs==0 ? TaggedOne : rhs==GetSmall(c) ? TaggedZero : 0);
	if (tval) {
	  DerefArg(attr, var,2);
	  DerefArg(var, var,1);
	  request_tell_bool(wam, attr, var, tval);
	}
      } else if (rhs==0) {
	goto fail;
      }
      fd.resumptions++;
      fd.entailments++;
    }
    X(0) = CTagToCdr(X(0));
    continue;
  fail:
    vapairs_bump_afc(CTagToArg(disequation,3));
    return FALSE;
  }
  return TRUE;
}

SP_BOOL propagate_watchers(Wam wam, TAGGED list, TAGGED value)
{
  X(0) = list;			/* X(0) is preserved over any GC here */

  while (TagIsLST(X(0))) {
    TAGGED watcher = CTagToCar(X(0));
    TAGGED entflag = CTagToArg(watcher,4); /* assert: dereffed */
    TAGGED watching, otherpair, tag, clause;
    
    X(0) = CTagToCdr(X(0));
    if (!IsVar(entflag) || !IsVar(CTagToREF(entflag)))
      continue;
    watching = CTagToArg(watcher,1);
    if (Teqz(value) == Tltz(watching)) {
      fd.entailments++;
      BindHVA(entflag,TaggedOne);
      continue;
    }
    otherpair = CTagToArg(watcher,2);
    tag = CTagToArg(watcher,3); /* 0: nary clause, 1,2: binary clause */
    clause = CTagToArg(watcher,5);
    if (Tgtz(tag)) {
      if (propagate_binary_clause(wam, tag & TaggedOne, otherpair, entflag))
	continue;
    } else {
      if (propagate_nary_clause(wam, watching, entflag, clause))
	continue;
    }
    vapairs_bump_afc(CTagToArg(clause,5));
    vapairs_bump_afc(CTagToArg(clause,6));
    return FALSE;
  }
  return TRUE;
}

SP_BOOL propagate_implications(Wam wam, TAGGED list)
{
  X(0) = list;			/* X(0) is preserved over any GC here */

  while (TagIsLST(X(0))) {
    TAGGED item = CTagToCar(X(0));
    TAGGED pair = CTagToArg(item,1);
    TAGGED right = CTagToArg(item,2);
    TAGGED var, attr, dom;
    
    DerefArg(var, pair, 1);
    DerefArg(attr, pair, 2);
    X(0) = CTagToCdr(X(0));
    DomFromAttr(dom,attr);
    if (DomainSize(dom)==TaggedOne) {
      var = DomainMin(dom);
      if (var==right)
	continue;
      else {
	AttrAFC(attr) += IStep(1);
	pair = CTagToArg(item,3);
	DerefArg(attr, pair, 2);
	AttrAFC(attr) += IStep(1);
	return FALSE;
      }
    }
    request_tell_bool(wam, attr, var, right);
  }
  return TRUE;
}

struct minmax_arg_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  int nvars;			/* _original_ #terms */
  int nrefs;
  Dvar dvar;
  char *mark;
};

static void SPCDECL minmax_arg_destructor(void *pdata_v)
{
  struct minmax_arg_data *pdata = (struct minmax_arg_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static void
fd_minimum_arg(Wam wam,
	       SP_term_ref State,
	       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_BOOL committed;
  SP_integer total_size;
  struct minmax_arg_data *pdata;
  int nvars, nrefs, j;
  char *ptr;
  TAGGED oval = Sup;		/* min domain ub of all */
  TAGGED pval = Inf;		/* smallest feasible min value */
  TAGGED qval = Sup;		/* min domain lb of supported */
  int o = 0;			/* first occ. of oval */
  int p = 0;			/* first occ. of pval */
  int q = 0;			/* first occ. of qval */
  SP_BOOL pexact = FALSE;	/* whether pval is max(DVAR(p)) */
  FDCONS cons;

  RefTerm(State) = fd_static_output_state(wam, &handle, &committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_arg_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);
    nvars = fd_list_length(tvec);
    nrefs = 2*nvars + 2;
    total_size = (nvars+1)*(sizeof(struct dvar)) + nvars;
    pdata = Palloc(struct minmax_arg_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (minmax_arg_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nrefs);
    pdata->nvars = nvars;
    pdata->nrefs = nrefs;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (nvars+1)*sizeof(struct dvar);
    pdata->mark = ptr;
    ptr += nvars;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);
    for (j=0; j<nvars; j++) { /* transfer Vec */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(j));
      dvar_init(DVAR(j), RefAttr(j), RefVar(j));
    }
    DerefArg(telt,X(0),2);	/* transfer Index */
    fd_get_var_and_attr(telt,RefAttr(j));
    dvar_init(DVAR(j), RefAttr(j), RefVar(j));
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }
  
  /* RESUME HERE */
  dvar_export_start(wam);
  for (j=0; j<nvars; j++)
    dvar_refresh(DVAR(j));
  dvar_refresh(DVAR(nvars));

  /* mark x[i] iff supported by Index */

  for (j=0; j<nvars; j++)
    if (!dvar_contains_value_l(DVAR(nvars),j+1))
      pdata->mark[j] = 0;
    else
      pdata->mark[j] = 1;

  /* compute <oval,o>: min element must be LE <oval,o> */
  
  for (j=0; j<nvars; j++) {
    TAGGED ub = dvar_max_t(DVAR(j));
    if (FDlt(ub,oval)) {
      oval = ub; o = j;
    }
  }

  /* compute <pval,p>: first feasible min element */
  
  if (oval==Sup) {
    pval = oval;
    p = dvar_min_int(DVAR(nvars))-1;
    pexact = TRUE;
  } else {
    for (j=0; j<nvars; j++)
      if (pdata->mark[j]) {
	TAGGED ub = fd_predecessor(dvar_set(DVAR(j)),j>o ? oval : oval+IStep(1));
	if (ub==Inf) {
	  pdata->mark[j] = 0;
	} else if (FDgt(ub,pval)) {
	  pval = ub; p=j;
	  pexact = (dvar_max_t(DVAR(p))==pval);
	}
      }
  }
  if (pval==Inf)
    goto ret;
  
  /* compute q, qval */

  for (j=0; j<nvars; j++) {
    TAGGED lb = dvar_min_t(DVAR(j));
    if (pdata->mark[j] && FDlt(lb,qval)) {
      qval = lb; q = j;
    }
  }

  /* remove infeasible candidates */

  fdcons_init(&cons);
  for (j=0; j<nvars; j++) {
    if (pdata->mark[j]) {
      TAGGED lb = dvar_min_t(DVAR(j));
      if (FDgt(lb,pval) || (lb==pval && j>p && pexact))
	pdata->mark[j] = 0;
      else
	fdcons_add(wam, &cons, MakeSmall(j+1));
    }
  }

  /* prune Index */

  if (dvar_fix_set(DVAR(nvars), fdcons_set(&cons)) < 0)
    goto ret;

  /* impose lower bounds on non-candidates */
  
  if (TagIsSmall(qval))
    for (j=0; j<nvars; j++)
      if (!pdata->mark[j]) {
	TAGGED glb = (j>q ? qval : qval+IStep(1));
	if (dvar_fix_min_t(DVAR(j), glb) < 0)
	  goto ret;
      }

  /* impose upper bound if single candidate */

  if (oval!=Inf && dvar_is_integer(DVAR(nvars)) &&
      dvar_fix_max_t(DVAR(p), p>o ? oval-IStep(1) : oval) < 0)
    goto ret;

  dvar_pruning_done(DVAR(nvars));

  ent = 0;
  if (dvar_is_integer(DVAR(nvars)) && dvar_is_integer(DVAR(p)))
    ent = 1;
  for (j=0; j<=nvars; j++)
    dvar_export(DVAR(j));
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}


static void
fd_maximum_arg(Wam wam,
	       SP_term_ref State,
	       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_BOOL committed;
  SP_integer total_size;
  struct minmax_arg_data *pdata;
  int nvars, nrefs, j;
  char *ptr;
  TAGGED oval = Inf;		/* max domain lb of all */
  TAGGED pval = Sup;		/* smallest feasible max value */
  TAGGED qval = Inf;		/* max domain ub of supported */
  int o = 0;			/* first occ. of oval */
  int p = 0;			/* first occ. of pval */
  int q = 0;			/* first occ. of qval */
  SP_BOOL pexact = FALSE;	/* whether pval is min(DVAR(p)) */
  FDCONS cons;

  RefTerm(State) = fd_static_output_state(wam, &handle, &committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_arg_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);
    nvars = fd_list_length(tvec);
    nrefs = 2*nvars + 2;
    total_size = (nvars+1)*(sizeof(struct dvar)) + nvars;
    pdata = Palloc(struct minmax_arg_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (minmax_arg_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nrefs);
    pdata->nvars = nvars;
    pdata->nrefs = nrefs;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (nvars+1)*sizeof(struct dvar);
    pdata->mark = ptr;
    ptr += nvars;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);
    for (j=0; j<nvars; j++) { /* transfer Vec */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(j));
      dvar_init(DVAR(j), RefAttr(j), RefVar(j));
    }
    DerefArg(telt,X(0),2);	/* transfer Index */
    fd_get_var_and_attr(telt,RefAttr(j));
    dvar_init(DVAR(j), RefAttr(j), RefVar(j));
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }
  
  /* RESUME HERE */
  dvar_export_start(wam);
  for (j=0; j<nvars; j++)
    dvar_refresh(DVAR(j));
  dvar_refresh(DVAR(nvars));

  /* mark x[i] iff supported by Index */

  for (j=0; j<nvars; j++)
    if (!dvar_contains_value_l(DVAR(nvars),j+1))
      pdata->mark[j] = 0;
    else
      pdata->mark[j] = 1;

  /* compute <oval,o>: max element must be GE <oval,o> */
  
  for (j=0; j<nvars; j++) {
    TAGGED lb = dvar_min_t(DVAR(j));
    if (FDgt(lb,oval)) {
      oval = lb; o = j;
    }
  }

  /* compute <pval,p>: first feasible max element */
  
  if (oval==Inf) {
    pval = oval;
    p = dvar_min_int(DVAR(nvars))-1;
    pexact = TRUE;
  } else {
    for (j=0; j<nvars; j++)
      if (pdata->mark[j]) {
	TAGGED lb = fd_successor(dvar_set(DVAR(j)),j>o ? oval : oval-IStep(1));
	if (lb==Sup) {
	  pdata->mark[j] = 0;
	} else if (FDlt(lb,pval)) {
	  pval = lb; p=j;
	  pexact = (dvar_min_t(DVAR(p))==pval);
	}
      }
  }
  if (pval==Sup)
    goto ret;
  
  /* compute q, qval */

  for (j=0; j<nvars; j++) {
    TAGGED ub = dvar_max_t(DVAR(j));
    if (pdata->mark[j] && FDgt(ub,qval)) {
      qval = ub; q = j;
    }
  }

  /* remove infeasible candidates */

  fdcons_init(&cons);
  for (j=0; j<nvars; j++) {
    if (pdata->mark[j]) {
      TAGGED ub = dvar_max_t(DVAR(j));
      if (FDlt(ub,pval) || (ub==pval && j>p && pexact))
	pdata->mark[j] = 0;
      else
	fdcons_add(wam, &cons, MakeSmall(j+1));
    }
  }

  /* prune Index */

  if (dvar_fix_set(DVAR(nvars), fdcons_set(&cons)) < 0)
    goto ret;

  /* impose upper bounds on non-candidates */
  
  if (TagIsSmall(qval))
    for (j=0; j<nvars; j++)
      if (!pdata->mark[j]) {
	TAGGED lub = (j>q ? qval : qval-IStep(1));
	if (dvar_fix_max_t(DVAR(j), lub) < 0)
	  goto ret;
      }

  /* impose lower bound if single candidate */

  if (oval!=Inf && dvar_is_integer(DVAR(nvars)) &&
      dvar_fix_min_t(DVAR(p), p>o ? oval+IStep(1) : oval) < 0)
    goto ret;

  dvar_pruning_done(DVAR(nvars));

  ent = 0;
  if (dvar_is_integer(DVAR(nvars)) && dvar_is_integer(DVAR(p)))
    ent = 1;
  for (j=0; j<=nvars; j++)
    dvar_export(DVAR(j));
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}


struct minmax_arg_bool_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  int nvars;			/* _original_ #terms */
  int nrefs;
  Bvar bvar;
  Dvar dvar;
  char *mark;
};

static void SPCDECL minmax_arg_bool_destructor(void *pdata_v)
{
  struct minmax_arg_bool_data *pdata = (struct minmax_arg_bool_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}


static void
fd_minimum_arg_bool(Wam wam,
		    SP_term_ref State,
		    SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_BOOL committed;
  SP_integer total_size;
  struct minmax_arg_bool_data *pdata;
  int nvars, nrefs, j;
  char *ptr;
  int oval = 1;			/* min domain ub of all */
  int pval = -1;		/* smallest feasible max value */
  int qval = 1;			/* min domain lb of supported */
  int o = 0;			/* first occ. of oval */
  int p = 0;			/* first occ. of pval */
  int q = 0;			/* first occ. of qval */
  SP_BOOL pexact = FALSE;	/* whether pval is max(BVAR(p)) */
  FDCONS cons;

  RefTerm(State) = fd_static_output_state(wam, &handle, &committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_arg_bool_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);
    nvars = fd_list_length(tvec);
    nrefs = 2*nvars + 2;
    total_size = nvars*(sizeof(struct bvar)) + sizeof(struct dvar) + nvars;
    pdata = Palloc(struct minmax_arg_bool_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (minmax_arg_bool_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nrefs);
    pdata->nvars = nvars;
    pdata->nrefs = nrefs;
    ptr = (char *)(pdata+1);
    pdata->bvar = (Bvar)ptr;
    ptr += nvars*sizeof(struct bvar);
    pdata->dvar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->mark = ptr;
    ptr += nvars;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);
    for (j=0; j<nvars; j++) { /* transfer Vec */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(j));
      bvar_init(BVAR(j), RefAttr(j), RefVar(j));
    }
    DerefArg(telt,X(0),2);	/* transfer Index */
    fd_get_var_and_attr(telt,RefAttr(j));
    dvar_init(DVAR(0), RefAttr(j), RefVar(j));
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }
  
  /* RESUME HERE */
  dvar_export_start(wam);
  for (j=0; j<nvars; j++)
    bvar_refresh(BVAR(j));
  dvar_refresh(DVAR(0));

  /* mark x[i] iff supported by Index */

  for (j=0; j<nvars; j++)
    if (!dvar_contains_value_l(DVAR(0),j+1))
      pdata->mark[j] = 0;
    else
      pdata->mark[j] = 1;

  /* compute <oval,o>: min element must be LE <oval,o> */
  
  for (j=0; j<nvars; j++) {
    if (bvar_max_l(BVAR(j))==0) {
      oval = 0; o = j; break;
    }
  }

  /* compute <pval,p>: first feasible min element */
  
  if (o==0) {			/* then oval must be 1 */
    pval = oval;
    p = dvar_min_int(DVAR(0))-1;
    pexact = TRUE;
  } else {
    for (j=0; j<nvars; j++)
      if (pdata->mark[j]) {
	if (bvar_min_l(BVAR(j))==1 || j>o) {
	  pdata->mark[j] = 0;
	} else if (pval<0) {
	  p = j; pexact = (bvar_max_l(BVAR(j))==0); pval = 0;
	}
      }
  }
  if (pval<0)
    goto ret;

  /* compute q, qval */

  for (j=0; j<nvars; j++) {
    if (pdata->mark[j] && bvar_min_l(BVAR(j))==0) {
      qval = 0; q = j; break;
    }
  }

  /* remove infeasible candidates */

  fdcons_init(&cons);
  for (j=0; j<nvars; j++) {
    if (pdata->mark[j]) {
      if (bvar_min_l(BVAR(j))==1) {
	if (pval==0 || j>p)
	  pdata->mark[j] = 0;
	else
	  fdcons_add(wam, &cons, MakeSmall(j+1));
      } else {
	if (pval==0 && j>p && pexact)
	  pdata->mark[j] = 0;
	else
	  fdcons_add(wam, &cons, MakeSmall(j+1));
      }
    }
  }

  /* prune Index */

  if (dvar_fix_set(DVAR(0), fdcons_set(&cons)) < 0)
    goto ret;

  dvar_pruning_done(DVAR(0));

  /* impose upper bounds on non-candidates */
  
  for (j=0; j<nvars; j++)
    if (!pdata->mark[j]) {
      if (qval==1 && j<=q) {
	goto ret;
      } else if (qval==1 || j<=q) {
	if (!bvar_export_value(wam, BVAR(j), 1))
	  goto ret;
      }
    }

  /* impose upper bound if single candidate */

  if (dvar_is_integer(DVAR(0))) {
    if (oval-(p>o) == 0 &&
	!bvar_export_value(wam, BVAR(p), 0))
      goto ret;
  }

  ent = 0;
  if (dvar_is_integer(DVAR(0)) && bvar_is_integer(BVAR(p)))
    ent = 1;
  dvar_export(DVAR(0));
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}


static void
fd_maximum_arg_bool(Wam wam,
		    SP_term_ref State,
		    SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_BOOL committed;
  SP_integer total_size;
  struct minmax_arg_bool_data *pdata;
  int nvars, nrefs, j;
  char *ptr;
  int oval = 0;			/* max domain lb of all */
  int pval = 2;			/* smallest feasible max value */
  int qval = 0;			/* max domain ub of supported */
  int o = 0;			/* first occ. of oval */
  int p = 0;			/* first occ. of pval */
  int q = 0;			/* first occ. of qval */
  SP_BOOL pexact = FALSE;	/* whether pval is min(BVAR(p)) */
  FDCONS cons;

  RefTerm(State) = fd_static_output_state(wam, &handle, &committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_arg_bool_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);
    nvars = fd_list_length(tvec);
    nrefs = 2*nvars + 2;
    total_size = nvars*(sizeof(struct bvar)) + sizeof(struct dvar) + nvars;
    pdata = Palloc(struct minmax_arg_bool_data, total_size, handle); /* GC, clobbers tvec */
    pdata->destructor = (minmax_arg_bool_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nrefs);
    pdata->nvars = nvars;
    pdata->nrefs = nrefs;
    ptr = (char *)(pdata+1);
    pdata->bvar = (Bvar)ptr;
    ptr += nvars*sizeof(struct bvar);
    pdata->dvar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->mark = ptr;
    ptr += nvars;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);
    for (j=0; j<nvars; j++) { /* transfer Vec */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(j));
      bvar_init(BVAR(j), RefAttr(j), RefVar(j));
    }
    DerefArg(telt,X(0),2);	/* transfer Index */
    fd_get_var_and_attr(telt,RefAttr(j));
    dvar_init(DVAR(0), RefAttr(j), RefVar(j));
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }
  
  /* RESUME HERE */
  dvar_export_start(wam);
  for (j=0; j<nvars; j++)
    bvar_refresh(BVAR(j));
  dvar_refresh(DVAR(0));

  /* mark x[i] iff supported by Index */

  for (j=0; j<nvars; j++)
    if (!dvar_contains_value_l(DVAR(0),j+1))
      pdata->mark[j] = 0;
    else
      pdata->mark[j] = 1;

  /* compute <oval,o>: max element must be GE <oval,o> */
  
  for (j=0; j<nvars; j++) {
    if (bvar_min_l(BVAR(j))>0) {
      oval = 1; o = j; break;
    }
  }

  /* compute <pval,p>: first feasible max element */
  
  if (o==0) {			/* then oval must be 0 */
    pval = oval;
    p = dvar_min_int(DVAR(0))-1;
    pexact = TRUE;
  } else {
    for (j=0; j<nvars; j++)
      if (pdata->mark[j]) {
	if (bvar_max_l(BVAR(j))==0 || j>o) {
	  pdata->mark[j] = 0;
	} else if (pval>1) {
	  p = j; pexact = (bvar_min_l(BVAR(j))==1); pval = 1;
	}
      }
  }
  if (pval>1)
    goto ret;

  /* compute q, qval */

  for (j=0; j<nvars; j++) {
    if (pdata->mark[j] && bvar_max_l(BVAR(j))==1) {
      qval = 1; q = j; break;
    }
  }

  /* remove infeasible candidates */

  fdcons_init(&cons);
  for (j=0; j<nvars; j++) {
    if (pdata->mark[j]) {
      if (bvar_max_l(BVAR(j))==0) {
	if (pval==1 || j>p)
	  pdata->mark[j] = 0;
	else
	  fdcons_add(wam, &cons, MakeSmall(j+1));
      } else {
	if (pval==1 && j>p && pexact)
	  pdata->mark[j] = 0;
	else
	  fdcons_add(wam, &cons, MakeSmall(j+1));
      }
    }
  }

  /* prune Index */

  if (dvar_fix_set(DVAR(0), fdcons_set(&cons)) < 0)
    goto ret;

  dvar_pruning_done(DVAR(0));

  /* impose upper bounds on non-candidates */
  
  for (j=0; j<nvars; j++)
    if (!pdata->mark[j]) {
      if (qval==0 && j<=q) {
	goto ret;
      } else if (qval==0 || j<=q) {
	if (!bvar_export_value(wam, BVAR(j), 0))
	  goto ret;
      }
    }

  /* impose lower bound if single candidate */

  if (dvar_is_integer(DVAR(0))) {
    if (oval+(p>o) == 1 &&
	!bvar_export_value(wam, BVAR(p), 1))
      goto ret;
  }

  ent = 0;
  if (dvar_is_integer(DVAR(0)) && bvar_is_integer(BVAR(p)))
    ent = 1;
  dvar_export(DVAR(0));
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam,Actions, ent);
}


/*
  '$fd_minimum_arg'(+State0, +State, -Actions).
  State = state(XVec,Index,BoolFlag)
*/
void SPCDECL
prolog_fd_minimum_arg(Wam wam,
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  TAGGED tbool;

  (void)State0;
  DerefArg(tbool,X(0),3);
  if (tbool == TaggedZero)
    fd_minimum_arg(wam, State, Actions);
  else
    fd_minimum_arg_bool(wam, State, Actions);
}


/*
  '$fd_minimum_arg'(+State0, +State, -Actions).
  State = state(XVec,Index,BoolFlag)
*/
void SPCDECL
prolog_fd_maximum_arg(Wam wam,
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  TAGGED tbool;

  (void)State0;
  DerefArg(tbool,X(0),3);
  if (tbool == TaggedZero)
    fd_maximum_arg(wam, State, Actions);
  else
    fd_maximum_arg_bool(wam, State, Actions);
}
