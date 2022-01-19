/* Copyright(C) 1999, Swedish Institute of Computer Science */
/***
Graeme Gange, Peter J. Stuckey, Radoslaw Szymanek:
MDD propagators with explanation. Constraints 16(4): 407-429 (2011)
***/


#include "fd.h"
#include "dvars.h"

#define DVAR(C) (pdata->dvar+(C))
#define ATTRIBUTE_LOC(C) (pdata->refbase + 2*(C))

struct mddi_common {
  int refcount;
  int postcount;
  int nvars;			/* arg 1 of state */
  int nnodes;			/* arg 2 of state */
  int nedges;			/* |arg 3 of state| */
  int nvarvals;			/* |arg 4 of state| */
  struct {
    SP_integer *var;		/* [nnodes+1, unused?] */
    SP_integer *in;		/* [nnodes+1] */
    SP_integer *out;		/* [nnodes+1] */
  } cnode;
  struct {
    SP_integer *source;		/* [nedges], source node */
    SP_integer *dest;		/* [nedges], destination node */
    SP_integer *varval;		/* [nedges], unique (var,val) combination, increasing */
    SP_integer *in;		/* [nedges] */
    SP_integer *out;		/* [nedges] */
    SP_integer *chunk;		/* [nvars] -- index of first edge for var */
  } cedge;
  struct {
    SP_integer *var;		/* [nvarvals] */
    TAGGED *min;		/* [nvarvals], arg 4 of state */
    TAGGED *max;		/* [nvarvals], arg 4 of state */
    SP_integer *edges;		/* [nvarvals] */
    SP_integer *chunk;		/* [nvars] -- index of first varval for var */
  } cvarval;
};
  
struct mddi_data {
  void (SPCDECL *destructor)(void*);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  struct mddi_common *common;
  SP_globref refbase;
  SP_integer stamp;		/* increases up to backtracking */
  int trail_top;
  Dvar dvar;
  unsigned char **trail;	 /* [(nedges+1+nvars)*1] */
  SP_integer *killed_from_above; /* [nedges] */
  SP_integer *killed_from_below; /* [nedges] */
  SP_integer *killed;		 /* [nvarvals] */
  int nkfa, nkfb, nk;
  SP_BOOL active;
  struct {
    SP_integer *watch_in;	/* [nnodes+1], watched edge */
    SP_integer *watch_out;	/* [nnodes+1], watched edge */
  } node;
  struct {
    unsigned char *status;	/* [nedges] */
  } edge;
  struct {
    SP_integer *support;	/* [nvarvals], watched edge */
  } varval;
  struct {
    unsigned char *status;	 /* [nvars] */
    SP_integer watch1;		/* watched var */
    SP_integer watch2;		/* watched var */
  } nonground;
};

#define EDGE_ALIVE 0x3
#define EDGE_KILLED_ABOVE 0x2
#define EDGE_KILLED_BELOW 0x1
#define EDGE_KILLED_VALUE 0x0
#define EDGE_WATCHED_ABOVE 0x4	/* aka. begin, support for node above */
#define EDGE_WATCHED_BELOW 0x8	/* aka. end, support for node below */
#define EDGE_WATCHED_VALUE 0x10	/* aka. val, support for varval */

#define TRAIL_DEC(VAR,DECR)			\
  SP_ASSERT((VAR) & EDGE_ALIVE);		\
  (VAR) = (unsigned char)((VAR) - (DECR));	\
  pdata->trail[pdata->trail_top++] = &(VAR);	\

#define TRAIL_DEC_EDGE(VAR,DECR,EDGE)		\
  TRAIL_DEC(VAR,DECR)				\

static void SPCDECL mddi_destructor(void *pdata_v)
{
  struct mddi_data *pdata = (struct mddi_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->refbase)
    SP_free_globrefs(pdata->refbase,2*pdata->common->nvars);
  if (--pdata->common->refcount==0)
    SP_free(pdata->common);
  SP_free(pdata);
}

/* the given variable of the given tuple is ground */
static void maintain_entailment(Wam wam,
				struct mddi_data *pdata,
				SP_integer var)
{
  struct mddi_common *common = pdata->common;
  int i;
  (void)wam;
  
  if (var >= 0) {
    if (pdata->nonground.status[var] & EDGE_ALIVE) {
      TRAIL_DEC(pdata->nonground.status[var],EDGE_ALIVE);
    }
    if (!(pdata->nonground.status[var] & EDGE_WATCHED_VALUE))
      return;
    for (i=0; i<common->nvars; i++) {
      if ((pdata->nonground.status[i] & (EDGE_ALIVE|EDGE_WATCHED_VALUE)) == EDGE_ALIVE) {
	pdata->nonground.status[var] = (unsigned char)
	  (pdata->nonground.status[var] - EDGE_WATCHED_VALUE);
	pdata->nonground.status[i] = (unsigned char)
	  (pdata->nonground.status[i] + EDGE_WATCHED_VALUE);
	if (pdata->nonground.watch1==var)
	  pdata->nonground.watch1 = i;
	if (pdata->nonground.watch2==var)
	  pdata->nonground.watch2 = i;
	return;
      }
    }
  }
}

static void downward_pass(Wam wam, struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nkfa = pdata->nkfa;
  int nk = pdata->nk;
  (void)wam;
  
 restart:
  while (nkfa > 0) {	/* search for a new support */
    SP_integer node = pdata->killed_from_above[--nkfa];
    SP_integer edge = common->cnode.in[node];
    SP_integer watch_in = pdata->node.watch_in[node];
    while (edge > -1) {
      if ((pdata->edge.status[edge] & 0x3) == EDGE_ALIVE) {
	/* support found; update watches */
	pdata->edge.status[watch_in] = (unsigned char)
	  (pdata->edge.status[watch_in] & ~EDGE_WATCHED_BELOW);
	pdata->edge.status[edge] |= EDGE_WATCHED_BELOW;
	pdata->node.watch_in[node] = edge;
	goto restart;
      }
      edge = common->cedge.in[edge];
    }
    /* the node is still dead; kill outgoing edges */
    edge = common->cnode.out[node];
    while (edge > -1) {
      unsigned char status = pdata->edge.status[edge];
      if ((status & 0x3) == EDGE_ALIVE) {
	TRAIL_DEC_EDGE(pdata->edge.status[edge],EDGE_ALIVE - EDGE_KILLED_ABOVE,edge);
	if (status & EDGE_WATCHED_BELOW)
	  pdata->killed_from_above[nkfa++] = common->cedge.dest[edge];
	if (status & EDGE_WATCHED_VALUE)
	  pdata->killed[nk++] = common->cedge.varval[edge];
      }
      edge = common->cedge.out[edge];
    }
  }
  pdata->nkfa = 0;
  pdata->nk = nk;
}

static void upward_pass(Wam wam, struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nkfb = pdata->nkfb;
  int nk = pdata->nk;
  (void)wam;
  
 restart:
  while (nkfb > 0) {	/* search for a new support */
    SP_integer node = pdata->killed_from_below[--nkfb];
    SP_integer edge = common->cnode.out[node];
    SP_integer watch_out = pdata->node.watch_out[node];
    while (edge > -1) {
      if ((pdata->edge.status[edge] & 0x3) == EDGE_ALIVE) {
	/* support found; update watches */
	pdata->edge.status[watch_out] = (unsigned char)
	  (pdata->edge.status[watch_out] & ~EDGE_WATCHED_ABOVE);
	pdata->edge.status[edge] |= EDGE_WATCHED_ABOVE;
	pdata->node.watch_out[node] = edge;
	goto restart;
      }
      edge = common->cedge.out[edge];
    }
    /* the node is still dead; kill ingoing edges */
    edge = common->cnode.in[node];
    while (edge > -1) {
      unsigned char status = pdata->edge.status[edge];
      if ((status & 0x3) == EDGE_ALIVE) {
	TRAIL_DEC_EDGE(pdata->edge.status[edge],EDGE_ALIVE - EDGE_KILLED_BELOW,edge);
	if (status & EDGE_WATCHED_ABOVE)
	  pdata->killed_from_below[nkfb++] = common->cedge.source[edge];
	if (status & EDGE_WATCHED_VALUE)
	  pdata->killed[nk++] = common->cedge.varval[edge];
      }
      edge = common->cedge.in[edge];
    }
  }
  pdata->nkfb = 0;
  pdata->nk = nk;
}

static void collect(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nk0=0, nk=0;

  while (nk0 < pdata->nk) {
    SP_integer varval = pdata->killed[nk0++];
    SP_integer edge = pdata->varval.support[varval];
    SP_integer e = common->cvarval.edges[varval];
    while (e<common->nedges && common->cedge.varval[e]==varval) {
      if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
	pdata->edge.status[edge] = (unsigned char)
	  (pdata->edge.status[edge] & ~EDGE_WATCHED_VALUE);
	pdata->edge.status[e] |= EDGE_WATCHED_VALUE;
	pdata->varval.support[varval] = edge = e;
	break;
      }
      e++;
    }
    if ((pdata->edge.status[edge] & 0x3) != EDGE_ALIVE) {
      pdata->killed[nk++] = varval;
    }
  }
  pdata->nk = nk;  
}

#if SP_ASSERTIONS
static SP_BOOL mddi_assertion_above(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=0; i<common->nnodes; i++) { /* exclude bottom node */
    int j = 0;
    SP_integer e = common->cnode.out[i];
    if (e > -1) {
      while (e > -1) {
	if (pdata->edge.status[e] & EDGE_WATCHED_ABOVE) j++;
	e = common->cedge.out[e];
      }
      if (j!=1)
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL mddi_assertion_below(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=1; i<=common->nnodes; i++) { /* exclude top node */
    int j = 0;
    SP_integer e = common->cnode.in[i];
    if (e > -1) {
      while (e > -1) {
	if (pdata->edge.status[e] & EDGE_WATCHED_BELOW) j++;
	e = common->cedge.in[e];
      }
      if (j!=1)
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL mddi_assertion_varval(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=0; i<common->nvarvals; i++) {
    int j = 0;
    SP_integer e = pdata->varval.support[i];
    while (e<common->nedges && common->cedge.varval[e]==i) {
      if (pdata->edge.status[e] & EDGE_WATCHED_VALUE) j++;
      e++;
    }
    if (j!=1)
      return FALSE;
  }
  return TRUE;
}
#endif /* SP_ASSERTIONS */

static void kill_edge(Wam wam,
		      struct mddi_data *pdata,
		      SP_integer edge,
		      SP_BOOL ignore)
{
  struct mddi_common *common = pdata->common;
  unsigned char status = pdata->edge.status[edge];
  (void)wam;
  (void)ignore;
  
  TRAIL_DEC_EDGE(pdata->edge.status[edge],EDGE_ALIVE - EDGE_KILLED_VALUE,edge);
  if (status & EDGE_WATCHED_ABOVE)
    pdata->killed_from_below[pdata->nkfb++] = common->cedge.source[edge];
  if (status & EDGE_WATCHED_BELOW)
    pdata->killed_from_above[pdata->nkfa++] = common->cedge.dest[edge];
}


static void kill_all_support_integer(Wam wam,
				     struct mddi_data *pdata,
				     SP_integer var,
				     TAGGED value)
{
  struct mddi_common *common = pdata->common;
  SP_integer e = common->cedge.chunk[var];
  SP_integer last = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer vvfalse = -1, vvtrue = -1;

  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) { /* killed due to external pruning */
      SP_integer varval = common->cedge.varval[e];
      if (varval==vvtrue) {
      } else if (varval==vvfalse) {
	kill_edge(wam,pdata,e,FALSE);
      } else if (Tgt(common->cvarval.min[varval],value) && IsSmall(common->cvarval.min[varval])) {
	break;			/* condition now monotone */
      } else if (Tlt(common->cvarval.max[varval],value) && IsSmall(common->cvarval.max[varval])) {
	vvfalse = varval;
	kill_edge(wam,pdata,e,FALSE);
      } else {
	vvtrue = varval;
      }
    }
  }
  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
      kill_edge(wam,pdata,e,FALSE);
    }
  }
}

static void kill_all_support_interval(Wam wam,
				      struct mddi_data *pdata,
				      SP_integer var,
				      TAGGED min,
				      TAGGED max)
{
  struct mddi_common *common = pdata->common; 
  SP_integer e = common->cedge.chunk[var];
  SP_integer last = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer vvfalse = -1, vvtrue = -1;

  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) { /* killed due to external pruning */
      SP_integer varval = common->cedge.varval[e];
      if (varval==vvtrue) {
      } else if (varval==vvfalse) {
	kill_edge(wam,pdata,e,FALSE);
      } else if (FDgt(common->cvarval.min[varval],max)) {
	break;			/* condition now monotone */
      } else if (FDlt(common->cvarval.max[varval],min)) {
	vvfalse = varval;
	kill_edge(wam,pdata,e,FALSE);
      } else {
	vvtrue = varval;
      }
    }
  }
  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
      kill_edge(wam,pdata,e,FALSE);
    }
  }
}

static void kill_all_support_general(Wam wam,
				     struct mddi_data *pdata,
				     SP_integer var,
				     Dvar dv) 
{
  struct mddi_common *common = pdata->common; 
  SP_integer e = common->cedge.chunk[var];
  SP_integer last = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer vvfalse = -1, vvtrue = -1;
  TAGGED min = dv->min;
  TAGGED max = dv->max;
  TAGGED set = dvar_set(dv);

  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) { /* killed due to external pruning */
      SP_integer varval = common->cedge.varval[e];
      TAGGED vmin = common->cvarval.min[varval];
      TAGGED vmax = common->cvarval.max[varval];
      if (varval==vvtrue) {
      } else if (varval==vvfalse) {
	kill_edge(wam,pdata,e,FALSE);
      } else if (FDgt(vmin,max)) {
	break;			/* remaining edges must all be dead */
      } else if (FDlt(vmax,min)) {
	vvfalse = varval;
	kill_edge(wam,pdata,e,FALSE);
      } else if (!fd_intersects_else(vmin,vmax,set,&set)) {
	if (set==EmptySet)
	  break;			/* remaining edges must all be dead */
	min = RangeMin(CTagToCar(set)); /* fd_min(set) */
	vvfalse = varval;
	kill_edge(wam,pdata,e,FALSE);
      } else {
	min = RangeMin(CTagToCar(set)); /* fd_min(set) */
	vvtrue = varval;
      }
    }
  }
  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
      kill_edge(wam,pdata,e,FALSE);
    }
  }
}

static void kill_all_support(Wam wam,
			     struct mddi_data *pdata,
			     SP_integer var) 
{
  Dvar dv = DVAR(var);

  if (dvar_is_integer(dv)) {	/* ground case */
    kill_all_support_integer(wam,pdata,var,dv->min);
    maintain_entailment(wam, pdata,var);
  } else if (dvar_is_interval(dv)) { /* interval case */
    kill_all_support_interval(wam,pdata,var,dv->min,dv->max);
  } else {			/* general case */
    kill_all_support_general(wam,pdata,var,dv);
  }
}



static int
mddi_propagate(Wam wam,
	       struct mddi_data *pdata,
	       SP_BOOL norefresh)
{
  struct mddi_common *common = pdata->common;
  int i, j;
  int nvars = common->nvars;
  int nnodes = common->nnodes;
  SP_BOOL idempotent = (!(RefMutable(CTagToArg(X(1),3))&IStep(4))); /* STATUS: idempotent */
  
  {
    SP_integer edge;

    downward_pass(wam, pdata);
    edge = pdata->node.watch_in[nnodes];
    if ((pdata->edge.status[edge] & 0x3) != EDGE_ALIVE)
      return -1;
    upward_pass(wam, pdata);
    collect(pdata); /* pdata->killed contains a list of varvals */
    SP_ASSERT(pdata->nk <= common->nvarvals);
    SP_ASSERT(mddi_assertion_above(pdata));
    SP_ASSERT(mddi_assertion_below(pdata));
    SP_ASSERT(mddi_assertion_varval(pdata));
    /* sort and form sets for batch pruning? */
    if (pdata->nk + pdata->active > 0) {
      if (!norefresh) {
	for (i=0; i<nvars; i++) {	/* i: var index */
	  Dvar dv = DVAR(i);
	  dvar_refresh(dv);
	}
      }
      for (j=0; j<pdata->nk; j++) {
	SP_integer varval = pdata->killed[j];
	SP_integer var = common->cvarval.var[varval];
	Dvar dv = DVAR(var);
	TAGGED min = common->cvarval.min[varval];
	TAGGED max = common->cvarval.max[varval];
	if (dvar_prune_interval_t(dv,min,max)< 0) /* can happen if there are corefs? */
	  return -1;
	if (dvar_is_integer(dv) && idempotent)
	  maintain_entailment(wam, pdata,var);
      }
      pdata->nk = 0;
      pdata->active = FALSE;
      for (i=0; i < nvars; i++) {
	Dvar dv = DVAR(i);
	dvar_pruning_done( dv);
      }
      for (i=0; i < nvars; i++) {
	Dvar dv = DVAR(i);
	dvar_export(dv);
      }
    }
  }
  CTagToArg(X(0),3) = MakeSmall(pdata->trail_top);
  if ((pdata->nonground.status[pdata->nonground.watch1] & EDGE_ALIVE) &&
      (pdata->nonground.status[pdata->nonground.watch2] & EDGE_ALIVE)) {
    return 0;
  /* here, at most one var is nonground */
  } else {
    return 1;
  }
}

static DAEMON_RC SPCDECL 
mddi_daemon(Wam wam,
	    void *vdata,
	    SP_globref attr_ref,
	    TAGGED *global)
{
  struct mddi_data *pdata = (struct mddi_data *)vdata;
  TAGGED tstate;
  int ar, state_stamp;
  DAEMON_RC rc = DAEMON_FIX;

  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall_int(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    int trail_top = GetSmall_int(CTagToArg(tstate,3));
    int f = pdata->trail_top;
    
    while (f>trail_top) {
      *(pdata->trail[--f]) |= EDGE_ALIVE;
    }
    pdata->trail_top = f;
    pdata->nkfa = 0;
    pdata->nkfb = 0;
    pdata->nk = 0;
  }
  {
    int col = (int)((attr_ref - pdata->refbase))>>1;
    if (pdata->nonground.status[col] & EDGE_ALIVE) { /* could have been processed already */
      Dvar dv = DVAR(col);
      SP_BOOL buried;
    
      tstate = fd_daemon_copy_state(wam, global,&buried);
      pdata->stamp = GetSmall_int(CTagToArg(tstate,ar));
      dvar_refresh(dv);
      kill_all_support(wam, pdata,col);
      CTagToArg(tstate,3) = MakeSmall(pdata->trail_top);
      if (pdata->nkfa + pdata->nkfb + pdata->nk + pdata->active > 0 ||
	  !(pdata->nonground.status[pdata->nonground.watch1] & EDGE_ALIVE) ||
	  !(pdata->nonground.status[pdata->nonground.watch2] & EDGE_ALIVE)
	  ) {
	rc = DAEMON_NOFIX;
      }
    }
  }
  return rc;
}

static struct mddi_data *
mddi_alloc_state(Wam wam, struct mddi_common *common, TAGGED handle)
{
  char *ptr;
  struct mddi_data *pdata;
  int nvars = common->nvars;
  int nnodes = common->nnodes;
  int nedges = common->nedges;
  int nvarvals = common->nvarvals;
  SP_integer total_size = nvars*sizeof(struct dvar) + /* dvar */
    nedges*sizeof(SP_integer) + /* killed_from_above */
    nedges*sizeof(SP_integer) + /* killed_from_below */
    nvarvals*sizeof(SP_integer) + /* killed */
    (nedges+1+nvars)*sizeof(unsigned char *) + /* trail */
    nvarvals*sizeof(SP_integer) +  /* varval */
    2*(nnodes+1)*sizeof(SP_integer) + /* node */
    nvars +			    /* nonground */
    nedges;			    /* edge */
  pdata = Palloc(struct mddi_data, total_size, handle); /* GC */
  ptr = (char *)(pdata+1);
  pdata->destructor = mddi_destructor;
  pdata->daemon = mddi_daemon;
  FD_STORE_SPENV(pdata->spenv);
  pdata->common = common;
  pdata->stamp = 0;
  pdata->refbase = SP_alloc_globrefs(2*nvars);
  pdata->dvar = (Dvar)ptr; ptr += nvars*sizeof(struct dvar);
  pdata->trail = (unsigned char **)ptr; ptr += (nedges+1+nvars)*sizeof(unsigned char *);
  pdata->node.watch_in = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  pdata->node.watch_out = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  pdata->varval.support = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  pdata->killed_from_above = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  pdata->killed_from_below = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  pdata->killed = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  pdata->edge.status = (unsigned char *)ptr; ptr += nedges;
  pdata->nonground.status = (unsigned char *)ptr; ptr += nvars;
  SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
  pdata->nkfa = 0;
  pdata->nkfb = 0;
  pdata->nk = 0;
  pdata->active = TRUE;
  return pdata;
}

static void
mddi_init_state(Wam wam, struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nvars = common->nvars;
  int nnodes = common->nnodes;
  int nedges = common->nedges;
  int nvarvals = common->nvarvals;
  int j;
  {
				/* tuple is entailed when < 2 nonground */
    for (j=0; j<nvars; j++) {
      pdata->nonground.status[j] = EDGE_ALIVE;
    }
    pdata->nonground.status[0] |= EDGE_WATCHED_VALUE;
    pdata->nonground.watch1 = 0;
    pdata->nonground.watch2 = 0;
    if (nvars>1) {
      pdata->nonground.status[1] |= EDGE_WATCHED_VALUE;
      pdata->nonground.watch2 = 1;
    }
    for (j=0; j<nedges; j++) {
      pdata->edge.status[j] = EDGE_ALIVE;
    }
    for (j=0; j<(nnodes+1); j++) {
      SP_integer edge = common->cnode.in[j];
      if (edge > -1) {	/* not for top node */
	pdata->node.watch_in[j] = edge;
	pdata->edge.status[edge] |= EDGE_WATCHED_BELOW;
      }
      edge = common->cnode.out[j];
      if (edge > -1) {	/* not for bot node */
	pdata->node.watch_out[j] = edge;
	pdata->edge.status[edge] |= EDGE_WATCHED_ABOVE;
      }
    }
    for (j=0; j<nvarvals; j++) {
      SP_integer edge = common->cvarval.edges[j];
      pdata->varval.support[j] = edge;
      pdata->edge.status[edge] |= EDGE_WATCHED_VALUE;
    }
    for (j=0; j<nvars; j++) {
      SP_globref ref = ATTRIBUTE_LOC(j);
      Dvar dv = DVAR(j);
	
      dvar_init(dv, ref, ref+1);
      dvar_attach_daemon(wam, dv, pdata, X(1), functor_dom1);
    }
  }
  pdata->trail_top = 0;
}

/*
'$fd_mddi_common'(NVars, Nodes, Edges, VarVals, Common), % Common = state([_ | '$free'(Ptr)], 0)
*/
void SPCDECL
prolog_fd_mddi_common(Wam wam,
		      SP_integer nvars,
		      SP_term_ref nodes_ref,
		      SP_term_ref edges_ref,
		      SP_term_ref varvals_ref,
		      SP_integer  postcount,
		      SP_term_ref common_ref)
{
  TAGGED nodes, edges, varvals, *h;
  int nnodes, nedges, nvarvals, i;
  struct mddi_common *common;
  struct mddi_data *pdata;
  SP_integer total_size;
  char *ptr;
    
  DEREF(nodes, RefTerm(nodes_ref));
  DEREF(edges, RefTerm(edges_ref));
  DEREF(varvals, RefTerm(varvals_ref));
  nnodes = fd_list_length(nodes);
  nedges = fd_list_length(edges);
  nvarvals = fd_list_length(varvals);

  total_size = 
    3*(nnodes+1)*sizeof(SP_integer) +	     /* cnode */
    5*nedges*sizeof(SP_integer) +	     /* cedge */
    nvars*sizeof(SP_integer) +		     /* cedge */
    4*nvarvals*sizeof(SP_integer) +	     /* cvarval */
    nvars*sizeof(SP_integer);		     /* cvarval */
  common = fd_malloc(wam, sizeof(struct mddi_common) + total_size);
  ptr = (char *)(common+1);
  common->refcount = 1;
  common->postcount = (int)postcount;
  common->nvars = (int)nvars;
  common->nnodes = nnodes;
  common->nedges = nedges;
  common->nvarvals = nvarvals;
  common->cnode.var = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  common->cnode.in  = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  common->cnode.out = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  common->cedge.source = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.dest = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.varval = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.in = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.out = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.chunk = (SP_integer *)ptr; ptr += nvars*sizeof(SP_integer);
  common->cvarval.var = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  common->cvarval.min = (TAGGED *)ptr; ptr += nvarvals*sizeof(TAGGED);
  common->cvarval.max = (TAGGED *)ptr; ptr += nvarvals*sizeof(TAGGED);
  common->cvarval.edges = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  common->cvarval.chunk = (SP_integer *)ptr; ptr += nvars*sizeof(SP_integer);
  for (i=0; i<nnodes; i++) {
    TAGGED item;
    
    DerefCar(item,nodes);
    DerefCdr(nodes,nodes);
    common->cnode.var[i] = GetSmall(item);
    common->cnode.in[i] = -1;
    common->cnode.out[i] = -1;
  }
  common->cnode.in[nnodes] = -1;
  common->cnode.out[nnodes] = -1;
  
  for (i=0; i<nedges; i++) {
    TAGGED item, tmp;
    
    DerefCar(item,edges);
    DerefCdr(edges,edges);
    DerefArg(tmp,item,1);	/* source */
    common->cedge.source[i] = GetSmall(tmp);
    DerefArg(tmp,item,2);	/* dest */
    common->cedge.dest[i] = GetSmall(tmp);
    DerefArg(tmp,item,3);	/* varval */
    common->cedge.varval[i] = GetSmall(tmp);
    common->cedge.in[i] = -1;
    common->cedge.out[i] = -1;
  }
    
  for (i=0; i<nvarvals; i++) {
    TAGGED item, tmp;
    
    DerefCar(item,varvals);
    DerefCdr(varvals,varvals);
    DerefArg(tmp,item,1);	/* var */
    common->cvarval.var[i] = GetSmall(tmp);
    DerefArg(tmp,item,2);	/* min */
    common->cvarval.min[i] = tmp;
    DerefArg(tmp,item,3);	/* max */
    common->cvarval.max[i] = tmp;
    common->cvarval.edges[i] = -1;
  }
				/* build linked lists */
  for (i=nvarvals-1; i>=0; --i) {
    common->cvarval.chunk[common->cvarval.var[i]] = i;
  }
  for (i=nedges-1; i>=0; --i) {
    SP_integer node = common->cedge.dest[i];
    SP_integer next = common->cnode.in[node];
    SP_integer varval = common->cedge.varval[i];
    common->cedge.in[i] = next;
    common->cnode.in[node] = i;
    node = common->cedge.source[i];
    next = common->cnode.out[node];
    common->cedge.out[i] = next;
    common->cnode.out[node] = i;
    common->cvarval.edges[varval] = i;
    common->cedge.chunk[common->cvarval.var[varval]] = i;
  }
  h = w->global_top;
  RefTerm(common_ref) = MakeStructure(h);
  h[0] = functor_minus;
  h[1] = TagREF(h+1);
  h[2] = TaggedZero;
  w->global_top = h+3;
  pdata = Palloc(struct mddi_data, 0, h[1]); /* GC */
  pdata->destructor = mddi_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->refbase = NULL;
  pdata->common = common;
}

/*
state(Tuple, Common, TrailTop , _Handle,0)
*/
void SPCDECL
prolog_fd_mddi(Wam wam,
	       SP_term_ref State0,
	       SP_term_ref State,
	       SP_term_ref Actions)
{
  struct mddi_data *pdata;
  struct mddi_common *common;
  int i, j, nnodes, nvars;
  int ent = -1;
  TAGGED cur, handle;
  SP_BOOL committed, posted;
  
/*    X(0) = RefTerm(State0); */
  (void)State0;
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct mddi_data,handle);
    common = pdata->common;
    nvars = common->nvars;
    nnodes = common->nnodes;
  } else {			/* build persistent state */
    TAGGED item, tvars;
    
    posted = TRUE;
    DerefArg(cur,X(0),2);	/* get state(...) */
    DerefArg(cur,cur,1);	/* get [_ | '$free'(_)] */
    common = Pdata(struct mddi_data,cur)->common;
    common->refcount++;
    if (--common->postcount==0)
      fd_common_done(wam,2);
    nvars = common->nvars;
    nnodes = common->nnodes;
    pdata = mddi_alloc_state(wam, common,handle); /* GC, clobbers cur */
    DerefArg(tvars,X(0),1);	/* refresh */
    {
      for (j=0; j<nvars; j++) {
	SP_globref ref = ATTRIBUTE_LOC(j);

	DerefCar(item,tvars);
	DerefCdr(tvars,tvars);
	fd_get_var_and_attr(item,ref);
      }
    }    
    mddi_init_state(wam, pdata);	/* can GC */
  }
  
  /* RESUME */
  dvar_export_start(wam);
  if (posted) {
    {
      for (i=0; i<nvars; i++) {
	Dvar dv = DVAR(i);
	
	dvar_refresh(dv);
	kill_all_support(wam, pdata,i);
      }
      if (nvars<2)
	maintain_entailment(wam, pdata,-1);
				/* clean up orphants */
      for (j=1; j<nnodes; j++) {
	if (common->cnode.in[j] == -1) {
	  pdata->node.watch_in[j] = -1;
	  pdata->killed_from_above[pdata->nkfa++] = j;
	}
	if (common->cnode.out[j] == -1) {
	  pdata->node.watch_out[j] = -1;
	  pdata->killed_from_below[pdata->nkfb++] = j;
	}
      }
    }
  }
  ent = mddi_propagate(wam, pdata, FALSE);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

struct table_compile {
  struct sw_on_key *cache;
  TAGGED *matrix;
  int *template;
  int *rowfilter;
  int *rowtmp;
  TAGGED **pointa;
  TAGGED **pointb;
  int *pop;
  TAGGED *domain1;
  TAGGED *domain2;
  int nrows;
  int ncols;
  int id3;
  int aux;
};

static void
fd_table_order(Wam wam, struct table_compile *frame) {
  int nrows = frame->nrows;
  int ncols = frame->ncols;
  TAGGED *tmp_col = (TAGGED *)SP_malloc(nrows*sizeof(TAGGED));
  TAGGED *new_matrix = (TAGGED *)SP_malloc(nrows*ncols*sizeof(TAGGED));
  int *tally = (int *)SP_malloc(ncols*sizeof(int));
  int i, j, k;

  for (j=0; j<ncols; j++) {
    int tallyj = 1;
    
    for (i=0; i<nrows; i++)
      tmp_col[i] = frame->matrix[i*ncols + j];
    fd_qsort_asc_tagged(wam, tmp_col, nrows);
    for (i=1; i<nrows; i++)
      if (tmp_col[i-1] != tmp_col[i])
	tallyj++;
    tally[j] = tallyj;
  }
  SP_free(tmp_col);
				/* lazy sort */
  for (j=0; j<ncols; j++) {
    int argmax = 0;
    for (k=1; k<ncols; k++)
      if (tally[k] > tally[argmax])
	argmax = k;
    frame->template[j] = argmax;
    tally[argmax] = 0;
  }
  SP_free(tally);

  for (i=0; i<nrows; i++)
    for (j=0; j<ncols; j++)
      new_matrix[i*ncols + j] = frame->matrix[i*ncols + frame->template[j]];

  SP_free(frame->matrix);
  frame->matrix = new_matrix;
}

static void
fd_table_augment(Wam wam, struct table_compile *frame) {
  int nrows = frame->nrows;
  int ncols0 = frame->ncols;
  int ncols = ncols0+1;
  TAGGED *new_matrix = (TAGGED *)SP_malloc(nrows*ncols*sizeof(TAGGED));
  int *new_template = (int *)SP_malloc(ncols*sizeof(int));
  int i, j;

  frame->ncols = ncols;
  
  for (i=0; i<nrows; i++)
    for (j=0; j<ncols; j++)
      if (j==0)
	new_matrix[i*ncols + j] = MakeSmall(i+1);
      else
	new_matrix[i*ncols + j] = frame->matrix[i*ncols0 + j-1];

  for (j=0; j<ncols; j++)
    if (j==0)
      new_template[j] = ncols-1;
    else
      new_template[j] = frame->template[j-1];

  SP_free(frame->matrix);
  frame->matrix = new_matrix;
  SP_free(frame->template);
  frame->template = new_template;
}

static int
fd_enum_expression_need(Wam wam, TAGGED x, int sofar) {
 start:
  if (TagIsSmall(x)) {
    return sofar+4;
  } else if (!TagIsSTR(x) || TagToHeadfunctor(x)!=SetArity(atom_comma,2)) {
    return -1;
  } else {
    TAGGED y;
    int need;

    DerefArg(y,x,2);
    DerefArg(x,x,1);
    need = fd_enum_expression_need(wam, y, 0);
    if (need<0)
      return -1;
    sofar += need;
    goto start;
  }
}

static int
fd_set_expression_need(Wam wam, TAGGED x, int sofar) {
  TAGGED f;
 start:
  f = (TagIsSTR(x) ? TagToHeadfunctor(x) : 0);
  
  if (x == SetArity(fd.functor_set1,0)) {
    return 0;
  } else if (!TagIsSTR(x)) {
    return -1;
  } else if (f == fd.functor_set1) {
    DerefArg(x,x,1);
    return fd_enum_expression_need(wam, x, sofar);
  } else if (f == fd.functor_interval2) {
    TAGGED y;
    
    DerefArg(y,x,2);
    DerefArg(x,x,1);
    if ((!IsSmall(x) && x!=Inf) || (!IsSmall(y) && y!=Sup))
      return -1;
    else
      return sofar+4;
  } else if (f == fd.functor_not1) {
    DerefArg(x,x,1);
    sofar += 4;			/* may need at most one more interval */
    goto start;
  } else if (f == fd.functor_and2 || f == fd.functor_or2) {
    TAGGED y;
    int need;
    
    DerefArg(y,x,2);
    DerefArg(x,x,1);
    need = fd_set_expression_need(wam, y, 0);
    if (need<0)
      return -1;
    sofar += need;
    goto start;
  } else {
    return -1;
  }
}

static int
fd_set_expression_need_top(Wam wam, TAGGED x, struct table_compile *frame) {
  struct sw_on_key_node *keyval = dyn_puthash(&frame->cache, x);

  if (keyval->value.arities) {
    return (int)keyval->value.arities;
  } else if (TagIsSmall(x)) {
    keyval->value.arities = 4;
    return 4;
  } else {
    int need = fd_set_expression_need(wam, x, 0);
    if (need > 0)
      dyn_puthash(&frame->cache, x)->value.arities = need;
    return need;
  }
}

static TAGGED
fd_enum_expression_rec(Wam wam, TAGGED x, TAGGED sofar) {
 start:
  if (TagIsSmall(x)) {
    return fd_insert_into(wam, x, sofar);
  } else {
    TAGGED y;

    DerefArg(y,x,2);
    DerefArg(x,x,1);
    sofar = fd_enum_expression_rec(wam, y, sofar);
    goto start;
  }
}

static TAGGED
fd_set_expression_rec(Wam wam, TAGGED x, TAGGED sofar) {
  TAGGED f;
  
 start:
  f = (TagIsSTR(x) ? TagToHeadfunctor(x) : 0);
  
  if (x == SetArity(fd.functor_set1,0)) {
    return EmptySet;
  } else if (f == fd.functor_set1) {
    DerefArg(x,x,1);
    return fd_enum_expression_rec(wam, x, sofar);
  } else if (f == fd.functor_interval2) {
    TAGGED y;
    
    DerefArg(y,x,2);
    DerefArg(x,x,1);
    if (FDle(x,y))
      return fd_union_interval(wam, sofar, x, y);
    else
      return sofar;
  } else if (f == fd.functor_not1) {
    DerefArg(x,x,1);
    return fd_union(wam, sofar, fd_complement(wam, fd_set_expression_rec(wam, x, EmptySet)));
  } else if (f == fd.functor_and2) {
    TAGGED y;
    
    DerefArg(y,x,2);
    DerefArg(x,x,1);
    return fd_union(wam,
		    sofar,
		    fd_intersection(wam,
				    fd_set_expression_rec(wam, x, EmptySet),
				    fd_set_expression_rec(wam, y, EmptySet)));
  } else if (f == fd.functor_or2) {
    TAGGED y;
    
    DerefArg(y,x,2);
    DerefArg(x,x,1);
    sofar = fd_set_expression_rec(wam, y, sofar);
    goto start;
  } else {
    return 0;
  }
}

static TAGGED
fd_set_expression_cons(Wam wam, TAGGED x, struct table_compile *frame) {
  struct sw_on_key_node *keyval = incore_gethash(frame->cache, x);
  TAGGED value = keyval->value.arities;

  if (value==EmptySet || TagIsLST(value))
    return value;
  if (TagIsSmall(x)) {
    value = fd_interval(wam, x, x);
  } else {
    value = fd_set_expression_rec(wam, x, EmptySet);
  }
  keyval->value.arities = value;
  return value;
}

static SP_WORD pop_table_size(SP_WORD pop)
{
  SP_WORD j=3, newsize=4;
  
  while (pop>j) {
    j<<=1; newsize<<=1;
  }
  return newsize;
}

static int cmp_asc_interval(Wam wam, TAGGED *x, TAGGED *y) {
  (void)wam;
  if (RangeMin(*x) != RangeMin(*y))
    return (FDlt(RangeMin(*x),RangeMin(*y)) ? -1 : 1);
  else if (RangeMax(*x) != RangeMax(*y))
    return (FDlt(RangeMax(*x),RangeMax(*y)) ? -1 : 1);
  else
    return 0;
}

#define QType TAGGED
#define QCmp  cmp_asc_interval
#define QSort qsort_asc_interval
#include "qsort.ic"

static int compare_rows(TAGGED *x, TAGGED *y, int from, int ncols, int *pivot) {
  int j;

  for (j=from; j<ncols; j++) {
    TAGGED xj = x[j];
    TAGGED yj = y[j];

    *pivot = j;
    while (TagIsLST(xj) && TagIsLST(yj)) {
      TAGGED xr = CTagToCar(xj);
      TAGGED yr = CTagToCar(yj);

      xj = CTagToCdr(xj);
      yj = CTagToCdr(yj);
      if (xr!=yr) {
	TAGGED xra = RangeMin(xr);
	TAGGED xrb = RangeMax(xr);
	TAGGED yra = RangeMin(yr);
	TAGGED yrb = RangeMax(yr);
	if (xra != yra)
	  return (FDlt(xra,yra) ? -1 : 1);
	else if (xrb != yrb)
	  return (FDlt(xrb,yrb) ? -1 : 1);
      }
    }
    if (xj!=yj)
      return (xj==EmptySet ? -1 : 1);
  }
  *pivot = j;
  return 0;
}

static int cmp_asc_row(Wam wam, int *x, int *y) {
  int ignore;
  struct table_compile *frame = (struct table_compile *)fd.gdata;
  int ncols = frame->ncols;

  return compare_rows(frame->matrix + (*x)*ncols, frame->matrix + (*y)*ncols, 0, ncols, &ignore);
}

#define QType int
#define QCmp  cmp_asc_row
#define QSort qsort_asc_row
#include "qsort.ic"

static SP_BOOL
decide_is_element(struct table_compile *frame) {
  int nrows = frame->nrows;
  int nrows_if_compressed = frame->nrows;
  int i;
  
  for (i=1; i<nrows; i++) {
    TAGGED x = frame->matrix[2*i - 1];
    TAGGED y = frame->matrix[2*i + 1];

    if (fd_min(x)==fd_min(y))
      --nrows_if_compressed;
  }
  // fprintf(stderr, "ELEMENT: no compress: %d, compress: %d\n", nrows, nrows_if_compressed);
  return nrows < 4*nrows_if_compressed;
}

SP_integer SPCDECL
prolog_fd_table_compile(Wam wam,
			SP_term_ref Extension1Ref, /* + */
			SP_term_ref Extension2Ref, /* - */
			SP_term_ref LitExtensionRef, /* - */
			SP_term_ref LiteralsRef,     /* - */
			SP_term_ref Domains1Ref,     /* + */
			SP_term_ref Domains2Ref,     /* - */
			SP_term_ref EltValuesRef,    /* - */
			SP_term_ref Template1Ref,    /* - */
			SP_term_ref Template2Ref,    /* - */
			char const *Order,	     /* + */
			char const *Method,	     /* + */
			SP_term_ref CulpritRef) {    /* - */
  TAGGED extension1 = RefTerm(Extension1Ref);
  TAGGED domains1 = RefTerm(Domains1Ref);
  TAGGED telt, trow;
  TAGGED *p, *h, *h1, *h2, *h3, *h4, *h5, *h6, *h7;
#if SP_ASSERTIONS
  TAGGED *hcap;
#endif
  SP_BOOL is_element = TRUE;
  int rc = 0, total_size = 0, litno = 0;
  int ncols0, ncols, nrows0, nrows, i, j;
  struct table_compile frame;

  DEREF(extension1,extension1);
  DEREF(domains1,domains1);
  nrows0 = nrows = frame.nrows = fd_list_length(extension1);
  ncols0 = ncols = frame.ncols = fd_list_length(domains1);
  p = frame.matrix = (TAGGED *)SP_malloc(nrows*ncols*sizeof(TAGGED));
  frame.domain1 = (TAGGED *)SP_malloc(ncols*sizeof(TAGGED));
  frame.template = (int *)SP_malloc(ncols*sizeof(int));
  frame.rowfilter = (int *)SP_malloc(nrows*sizeof(int));
  frame.rowtmp = (int *)SP_malloc(nrows*sizeof(int));
  for (j=0; j<ncols; j++) {
    DerefCar(telt,domains1);
    DerefCdr(domains1,domains1);
    frame.domain1[j] = telt;
    frame.template[j] = j;
  }
  while (TagIsLST(extension1)) {
    DerefCar(trow,extension1);
    DerefCdr(extension1,extension1);
    while (TagIsLST(trow)) {
      DerefCar(telt,trow);
      DerefCdr(trow,trow);
      *p++ = telt;
    }
  }
  frame.id3 = !strcmp(Order,"id3");
  frame.aux = !strcmp(Method,"aux");
  frame.cache = new_switch_on_key(32,NULL);
  frame.pointa = NULL;
  frame.pointb = NULL;
  frame.pop = NULL;
  frame.domain2 = NULL;
  if (frame.id3)
    fd_table_order(wam, &frame);
  if (frame.aux)
    fd_table_augment(wam, &frame);
  ncols = frame.ncols;
				/* type check matrix entries and store them in frame.cache */
  for (i=0; i<nrows*ncols; i++) {
    int need = fd_set_expression_need_top(wam, frame.matrix[i], &frame);
    if (need < 0) {
      RefTerm(CulpritRef) = frame.matrix[i];
      rc = 1;
      goto cleanup;
    }
  }

  FdMemInit;
  nrows = 0;
  is_element = (ncols==2);
				/* internalize matrix entries; prune row if some element is outside var domain */  
  for (i=0; i<nrows0; i++) {
    int inc = 1;
    for (j=0; j<ncols; j++) {
      int j1 = frame.template[j];
      int rix = i*ncols + j;
      int wix = nrows*ncols + j;
      TAGGED fdset = fd_set_expression_cons(wam, frame.matrix[rix], &frame);

      frame.matrix[wix] = fdset;
      if (fdset==EmptySet)
	inc = 0;
      else if (j1<ncols0 && !fd_intersect(fdset,frame.domain1[j1]))
	inc = 0;
      else if (j==0 && is_element && !(fd_singleton(fdset) && fd_min(fdset)==MakeSmall(nrows+1)))
	is_element = FALSE;	
      else if (j==1 && is_element && !fd_singleton(fdset))
	is_element = FALSE;	
    }
    frame.rowfilter[i] = inc;
    frame.rowtmp[nrows] = nrows;
    nrows += inc;
  }
  if (nrows==0) {
    SP_fail();
    goto cleanup;
  } else if (nrows==1) {
    is_element = FALSE;
  }
  frame.nrows = nrows;
  frame.pointa = (TAGGED **)SP_malloc(ncols*sizeof(TAGGED *));
  frame.pointb = (TAGGED **)SP_malloc(ncols*sizeof(TAGGED *));
  frame.pop = (int *)SP_malloc(ncols*sizeof(int));
  frame.domain2 = (TAGGED *)SP_malloc(ncols*sizeof(TAGGED));
  if (is_element)
    is_element = decide_is_element(&frame);
  if (!is_element) {
    int todo = 0, done = 0;

				/* greedy pairwise compression of matrix rows */
				/* first, lex sort the rows */
    fd.gdata = &frame;
    qsort_asc_row(wam, frame.rowtmp, nrows);
    p = frame.matrix;
    frame.matrix = (TAGGED *)SP_malloc(nrows*ncols*sizeof(TAGGED));
    for (i=0; i<nrows; i++) {
      int src = frame.rowtmp[i];
      for (j=0; j<ncols; j++)
	frame.matrix[i*ncols + j] = p[src*ncols + j];
    }
    SP_free(p);
				/* then, merge rows pairwise greedily */
    while (todo < nrows) {
      int pivot1=ncols, pivot2=ncols;
      if (done>0) {
	compare_rows(&frame.matrix[(done-1)*ncols], &frame.matrix[(todo)*ncols], 0, ncols, &pivot1);
	if (pivot1 < ncols-1)
	  compare_rows(&frame.matrix[(done-1)*ncols], &frame.matrix[(todo)*ncols], pivot1+1, ncols, &pivot2);
      }
      if (done>0 && pivot1==ncols) {
	--done;
      } else if (pivot1<ncols && pivot2==ncols) {
	frame.matrix[(todo)*ncols+pivot1] = fd_union(wam, frame.matrix[(todo)*ncols+pivot1], frame.matrix[(done-1)*ncols+pivot1]);
	--done;
      } else if (done<todo) {
	for (j=0; j<ncols; j++)
	  frame.matrix[done*ncols + j] = frame.matrix[todo*ncols + j];
	done++;
	todo++;
      } else {
	done++;
	todo++;
      }
    }
    frame.nrows = nrows = done;
  }

				/* compute literals per column: */
				/* first, collect all intervals and their start and end+1 points */
				/* then, build union domain, build literals */
  for (j=0; j<ncols; j++) {
    int lim = 2*nrows;
    int ilim = nrows;
    int pop = 0;
    int ipop = 0;
    int j1 = frame.template[j];
    TAGGED *points = (TAGGED *)SP_malloc(lim*sizeof(TAGGED));
    TAGGED *intervals = (TAGGED *)SP_malloc(ilim*sizeof(TAGGED));
    TAGGED preva, prevb, *pointa, *pointb;
    int k;
    FDCONS cons;
    
    fdcons_init(&cons);
    for (i=0; i<nrows; i++) {
      TAGGED fdset = frame.matrix[i*ncols + j];

      while (TagIsLST(fdset)) {
	TAGGED interval = CTagToCar(fdset);
	TAGGED a = CTagToCar(interval);
	TAGGED b = CTagToCdr(interval);

	if (b != Sup)
	  b = FDincr(b);
	fdset = CTagToCdr(fdset);
	if (pop+2 >= lim) {
	  lim *= 2;
	  points = (TAGGED *)SP_realloc(points, lim*sizeof(TAGGED));
	}
	points[pop++] = (a==Inf ? InfAsINT : a);
	points[pop++] = (b==Sup ? SupAsINT : b);
	if (ipop+1 >= ilim) {
	  ilim *= 2;
	  intervals = (TAGGED *)SP_realloc(intervals, ilim*sizeof(TAGGED));
	}
	intervals[ipop++] = interval;
      }
    }
    fd_qsort_asc_tagged(wam, points, pop);
    for (i=k=0; i<pop; i++)
      if (i==0 || points[i]!=points[i-1])
	points[k++] = points[i];
    pop = k;
    qsort_asc_interval(wam, intervals, ipop);
    preva = RangeMin(intervals[0]);
    prevb = RangeMax(intervals[0]);
    for (i=1; i<ipop; i++) {
      if (FDlt(prevb, RangeMin(intervals[i]))) {
	fdcons_add_interval(wam,&cons,preva,prevb);
	preva = RangeMin(intervals[i]);
	prevb = RangeMax(intervals[i]);
      } else if (FDlt(prevb, RangeMax(intervals[i]))) {
	prevb = RangeMax(intervals[i]);
      }
    }
    fdcons_add_interval(wam,&cons,preva,prevb);
    pointa = (TAGGED *)SP_malloc(pop*sizeof(TAGGED));
    pointb = (TAGGED *)SP_malloc(pop*sizeof(TAGGED));
    for (i=k=0; i<pop-1; i++) {
      TAGGED x = (points[i] == InfAsINT ? Inf : points[i]);
      if (i==0 || fd_member(x, fdcons_set(&cons))) {
	pointa[k] = x;
	pointb[k++] = (points[i+1] == SupAsINT ? Sup : points[i+1]-IStep(1));
      }
    }
    pop = k;
    frame.pointa[j] = pointa;
    frame.pointb[j] = pointb;
    frame.pop[j] = pop;
    frame.domain2[j] = fdcons_set(&cons);
    if (j1<ncols0)
      frame.domain2[j] = fd_intersection(wam, frame.domain1[j1], frame.domain2[j]);
    total_size += 4*fd_list_length(frame.domain2[j]);
    SP_free(points);
    SP_free(intervals);
  }
				/* replace every matrix element by sorted list of literals */
  for (j=0; j<ncols; j++) {
    TAGGED *pointa = frame.pointa[j];
    int pop = frame.pop[j];
    int litbase = litno;
    struct sw_on_key *point_hash = new_switch_on_key(pop_table_size(pop),NULL);

    for (i=0; i<pop; i++)
      dyn_puthash(&point_hash, pointa[i])->value.arities = MakeSmall(litno++);

    for (i=0; i<nrows; i++) {
      TAGGED fdset = frame.matrix[i*ncols + j];
      TAGGED *target = &frame.matrix[i*ncols + j];

      while (TagIsLST(fdset)) {
	TAGGED interval = CTagToCar(fdset);
	TAGGED a = CTagToCar(interval);
	TAGGED b = CTagToCdr(interval);
	int succ = 0;

	fdset = CTagToCdr(fdset);
	do {
	  TAGGED e = incore_gethash(point_hash, a)->value.arities;
	  TAGGED *h;
	  
	  total_size += 2;
	  NumstackAlloc(2,h);
	  *target = MakeList(h);
	  h[0] = e;
	  target = &h[1];
	  succ = GetSmall_int(e)-litbase+1;
	  a = pointa[succ];
	} while (succ < pop && FDle(a,b));
      }
      *target = EmptySet;
    }
    dispose_switch_on_key(point_hash);
  }

  total_size += 2*nrows0 + 6*ncols;
  if (!is_element)
    total_size += 2*nrows*ncols + 2*nrows + 6*litno;
  if (is_element)
    total_size += 2*nrows;
  FdMemRequireHeap(total_size, 0);
  h = w->global_top;
#if SP_ASSERTIONS
  hcap = h+total_size;
#endif
  if (!is_element) {
    /* globalize matrix elements */
    for (i=0; i<nrows*ncols; i++) {
      TAGGED encoding = frame.matrix[i];

      frame.matrix[i] = MakeList(h);
      while (TagIsLST(encoding)) {
	h[0] = CTagToCar(encoding);
	h[1] = MakeList(h + 2);
	h += 2;
	encoding = CTagToCdr(encoding);
      }
      h[-1] = EmptySet;
    }
    h1 = h;			/* base for extension2 matrix */
    for (i=0; i<nrows*ncols; i++) {
      h1[2*i] = frame.matrix[i];
      if ((i+1) % ncols)
	h1[2*i + 1] = MakeList(&h1[2*i + 2]);
      else
	h1[2*i + 1] = EmptySet;
    }
    h2 = h1 + 2*nrows*ncols;	/* base for list of rows */
    for (i=0; i<nrows; i++) {
      h2[2*i] = MakeList(&h[2*i*ncols]);
      if ((i+1) < nrows)
	h2[2*i + 1] = MakeList(&h2[2*i + 2]);
      else
	h2[2*i + 1] = EmptySet;
    }
    RefTerm(LitExtensionRef) = MakeList(h2);
    h = h2 + 2*nrows;
  }
  h7 = h;		 /* base for filtered extension, nrows0 long */
  DEREF(extension1,RefTerm(Extension1Ref));
  for (i=0; i<nrows0; i++) {
    DerefCar(h[0],extension1);
    DerefCdr(extension1,extension1);
    if (frame.rowfilter[i]) {
      h[1] = MakeList(h+2);
      h += 2;
    }
  }
  h[-1] = EmptySet;
  RefTerm(Extension2Ref) = MakeList(h7);
  h3 = h;			/* base for template1, ncols0 long */
  for (i=0; i<ncols; i++) {
    h3[2*i] = TagREF(h3 + 2*i);
    if ((i+1) < ncols0)
      h3[2*i + 1] = MakeList(&h3[2*i + 2]);
    else
      h3[2*i + 1] = EmptySet;
  }
  RefTerm(Template1Ref) = MakeList(h3);
  h4 = h3 + 2*ncols;		/* base for template2, ncols long */
  for (i=0; i<ncols; i++) {
    h4[2*i] = TagREF(h3 + 2*frame.template[i]);
    if ((i+1) < ncols)
      h4[2*i + 1] = MakeList(&h4[2*i + 2]);
    else
      h4[2*i + 1] = EmptySet;
  }
  RefTerm(Template2Ref) = MakeList(h4);
  h = h4 + 2*ncols;
  if (!is_element) {
    h5 = h;			/* base for literals */
    for (j=0; j<ncols; j++) {
      TAGGED *pointa = frame.pointa[j];
      TAGGED *pointb = frame.pointb[j];
      int pop = frame.pop[j];

      for (i=0; i<pop; i++) {
	h[0] = MakeStructure(h + 2);
	h[1] = MakeList(h + 6);
	h[2] = fd.functor_lit3;
	h[3] = MakeSmall(j);
	h[4] = pointa[i];
	h[5] = pointb[i];
	h += 6;
      }
    }
    h[-5] = EmptySet;
    RefTerm(LiteralsRef) = MakeList(h5);
  }
  h6 = h1 = h;			/* base for domains */
  for (j=0; j<ncols; j++) {
    TAGGED domain = frame.domain2[j];
    
    h1 = h;
    h[0] = MakeList(h+2);
    h[1] = 0;
    h += 2;
    while (domain!=EmptySet) {
      TAGGED r1 = CTagToCar(domain);
      domain = CTagToCdr(domain);
      h[0] = MakeList(h+2);
      h[1] = MakeList(h+4);
      h[2] = RangeMin(r1);
      h[3] = RangeMax(r1);
      h += 4;
    }
    h[-3] = EmptySet;
    h1[1] = MakeList(h);
  }
  h1[1] = EmptySet;
  RefTerm(Domains2Ref) = MakeList(h6);

  if (is_element) {
    for (i=0; i<nrows; i++) {
      TAGGED y = frame.matrix[2*i + 1];
      int iy = GetSmall_int(CTagToCar(y));
      h[2*i] = frame.pointa[1][iy - nrows];
      h[2*i+1] = MakeList(h+2*i+2);
    }
    h[2*i-1] = EmptySet;
    RefTerm(EltValuesRef) = MakeList(h);
    h += 2*i;
  }
  
  w->global_top = h;
  SP_ASSERT(h <= hcap);
    
 cleanup:
  if (frame.pointa) {
    for (j=0; j<ncols; j++) {
      SP_free(frame.pointa[j]);
      SP_free(frame.pointb[j]);
    }
    SP_free(frame.pointa);
    SP_free(frame.pointb);
    SP_free(frame.pop);
    SP_free(frame.domain2);
  }
  SP_free(frame.matrix);
  SP_free(frame.rowfilter);
  SP_free(frame.rowtmp);
  SP_free(frame.domain1);
  SP_free(frame.template);
  dispose_switch_on_key(frame.cache);
  return rc;
}

