/* Copyright(C) 2006, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

/* regions 0, 1 are reserved bounding boxes */
#define FIRST_BB 2

struct diffn_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_globref refbase;		/* [nobjects*4*kdims] */
  SP_integer stamp;
  int kdims;
  int strict;
  int nobjects;
  int nregions;
  int ntargets;
  int nrefs;
  int *target;			/* [nobjects] */
  SP_integer *current;		/* [kdims] */
  SP_integer *next;		/* [kdims] */
  Dvar dvar;			/* [(nobjects)*(kdims+1)] */
  int *flags;			/* [nobjects] */
  struct {
    unsigned int *min;
    unsigned int *max;
  } sweepset;
  struct {
    int end_bb;
    int end_relative;
    int end_absolute;
    int top_of_stack;
    int round_robin;
    SP_integer *origin;	/* [nregions*kdims] : region origin, inclusive volatile */
    SP_integer *end; /* [nregions*kdims] : region end, inclusive volatile */
    int *obj;	     /* [nregions] volatile */
  } fr;
};

static void SPCDECL 
diffn_destructor(void *pdata_v)
{
  struct diffn_data *pdata = (struct diffn_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

#define TARGET(i) (pdata->target[i])
#define O_FLAGS(o) (pdata->flags[o]) /* 1=pruned, 2=queued, 4=inactive due to zero length and not strict */
#define O_ORIG_ATTR(o,j) (pdata->refbase + 4*kdims*(o) + 4*(j))
#define O_ORIG_DVAR(o,j) (pdata->dvar + 2*kdims*(o) + 2*(j))
#define FR_ORIGIN(i,j) (pdata->fr.origin[kdims*(i) + (j)])
#define FR_END(i,j) (pdata->fr.end[kdims*(i) + (j)])
#define FR_OBJ(i) (pdata->fr.obj[i])

#if DBG
extern void diffn_dump_region(struct diffn_data *pdata, int i);

void
diffn_dump_region(struct diffn_data *pdata,
	    int i)
{
  int j, kdims=pdata->kdims;
  printf("region %d: (", i);
  for (j=0; j<kdims-1; j++)
    printf("%" SPRIdINTEGER ",", (SP_integer)FR_ORIGIN(i,j));
  printf("%" SPRIdINTEGER ") .. (", (SP_integer)FR_ORIGIN(i,kdims-1));
  for (j=0; j<kdims-1; j++)
    printf("%" SPRIdINTEGER ",", (SP_integer)FR_END(i,j));
  printf("%" SPRIdINTEGER ")\n", (SP_integer)FR_END(i,kdims-1));
}

extern void diffn_dump_objects(struct diffn_data *pdata);

void 
diffn_dump_objects(struct diffn_data *pdata)
{
  int i, j;
  int kdims = pdata->kdims;

  for (i=0; i<pdata->nobjects; i++) {
    Dvar dv = O_ORIG_DVAR(i,0);

    for (j=0; j<kdims; j++) {
      printf("*** object %d.x[%d]\n\n", i,j);
      dvar_dump(dv+2*j);
      printf("\n");
      printf("*** object %d.s[%d]\n\n", i,j);
      dvar_dump(dv+2*j+1);
      printf("\n");
    }
  }
}
#endif

/* -1 - no join, 0 - equal, 1 - join */
static int
join_regions(struct diffn_data *pdata, int fr1, int fr2)
{
  int kdims = pdata->kdims;
  int pivot = -1;
  int j;
  for (j=0; j<kdims; j++) {
    if (FR_ORIGIN(fr1,j)==FR_ORIGIN(fr2,j) && FR_END(fr1,j)==FR_END(fr2,j)) {
    } else if (pivot > -1) {
      return -1;
    } else if (FR_END(fr1,j)+1 < FR_ORIGIN(fr2,j)) {
      return -1;
    } else if (FR_END(fr2,j)+1 < FR_ORIGIN(fr1,j)) {
      return -1;
    } else {
      pivot = j;
    }
  }
  if (pivot == -1)
    return 0;
  if (FR_ORIGIN(fr1,pivot) > FR_ORIGIN(fr2,pivot))
    FR_ORIGIN(fr1,pivot) = FR_ORIGIN(fr2,pivot);
  if (FR_END(fr1,pivot) < FR_END(fr2,pivot))
    FR_END(fr1,pivot) = FR_END(fr2,pivot);
  return 1;
}

static SP_BOOL
fr_intersects_object(struct diffn_data *pdata,
		     int fr1,
		     int object)
{
  int kdims = pdata->kdims;
  int j;
  Dvar dv = O_ORIG_DVAR(object,0);

  for (j=0; j<kdims; j++) {
    SP_integer minsize = dvar_min_l(dv+2*j+1);
    SP_integer orig = dvar_min_l(dv+2*j);
    SP_integer end  = dvar_max_l(dv+2*j) + minsize - 1;
    if (end<FR_ORIGIN(fr1,j) || FR_END(fr1,j)<orig)
      return FALSE;
  }
  return TRUE;
}

static void
expand_regions(Wam wam,
	       struct diffn_data *pdata)
{
  int kdims = pdata->kdims;
  int nregions = pdata->nregions;
  SP_integer total_size =
    2*nregions*sizeof(int) +
    2*kdims*nregions*sizeof(SP_integer) +
    2*kdims*nregions*sizeof(SP_integer);
  char *ptr = SP_realloc(pdata->fr.origin, total_size);
  
  pdata->fr.origin = (SP_integer *)ptr;
  ptr += 2*kdims*nregions*sizeof(SP_integer);
  pdata->fr.end = (SP_integer *)ptr;
  ptr += 2*kdims*nregions*sizeof(SP_integer);
  pdata->fr.obj = (int *)ptr;
  ptr += 2*nregions*sizeof(int);
  SP_ASSERT(ptr == (char *)(pdata->fr.origin)+total_size);
  memmove(pdata->fr.end   , pdata->fr.origin+nregions*kdims, nregions*kdims*sizeof(SP_integer) + nregions*sizeof(int));
  memmove(pdata->fr.obj   , pdata->fr.end   +nregions*kdims, nregions*sizeof(int));
  pdata->nregions *= 2;
}

static int
store_region(Wam wam,
	     struct diffn_data *pdata,
	     int new,
	     int object)
{
  int q = new+1;

  FR_OBJ(new) = object;
  if (new>pdata->fr.end_relative && join_regions(pdata,new-1,new)!=-1)
    return new;

  if (q > new) { /* INVARIANT: ensure that region q is available */
    if (q == pdata->nregions)
      expand_regions(wam, pdata);
  }
  return q;
}

/* returns -1 for failure, 0 for no change, >0 for pruned */
static int
pre_sweep(Wam wam,
	  struct diffn_data *pdata,
	  int object)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int rc=0, fr1, fr2, j;

  for (fr1=fr2=pdata->fr.end_relative; rc>=0 && fr1<pdata->fr.end_absolute; fr1++) {
    int coverdims=0, p=0;
    for (j=0; j<kdims; j++) {
      if (dvar_min_l(dv+2*j) >= FR_ORIGIN(fr1,j) && dvar_max_l(dv+2*j) <= FR_END(fr1,j))
	coverdims++;
      else
	p = j;
    }
    if (coverdims==kdims) {
      rc = -1;
    } else if (coverdims==kdims-1) {
      int rc1 = dvar_prune_interval_l(dv+2*p,FR_ORIGIN(fr1,p),FR_END(fr1,p));
      rc |= rc1;
      if (rc1>0) {
	O_FLAGS(object) |= 0x3;	/* pruned, queued */
      }
    } else {
      if (fr1>fr2) {
	FR_OBJ(fr2) = FR_OBJ(fr1);
	for (j=0; j<kdims; j++) {
	  FR_ORIGIN(fr2,j) = FR_ORIGIN(fr1,j);
	  FR_END(fr2,j) = FR_END(fr1,j);
	}
      }
      fr2++;
    }
  }
  pdata->fr.end_absolute = fr2;
  pdata->fr.top_of_stack = fr2;
  pdata->fr.round_robin = fr2;
  return rc;
}

static SP_BOOL
useful_absolute_fr(struct diffn_data *pdata,
		   int fr,
		   Dvar dv,
		   int j)
{
  int kdims = pdata->kdims;
  if (FR_ORIGIN(fr,j) < dvar_min_l(dv+2*j))
    FR_ORIGIN(fr,j) = dvar_min_l(dv+2*j);
  if (FR_END(fr,j) > dvar_max_l(dv+2*j))
    FR_END(fr,j) = dvar_max_l(dv+2*j);
  if (FR_ORIGIN(fr,j) > FR_END(fr,j))
    return FALSE;
  return TRUE;
}

static void
relative_fr(Wam wam,
	    struct diffn_data *pdata,
	    int object,
	    int *region)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int fr = *region;
  int j;
  
  for (j=0; j<kdims; j++) {
    FR_ORIGIN(fr,j) = dvar_max_l(dv+2*j)+1;
    FR_END(fr,j) = dvar_min_l(dv+2*j)+dvar_min_l(dv+2*j+1)-1;
  }
  *region = store_region(wam, pdata,*region,object);
}

static void
all_relative_fr(Wam wam,
		struct diffn_data *pdata)
{
  int i;
  
  pdata->fr.end_relative = pdata->fr.end_bb;
  /* Nicolas says: generate FR for ground objects before nonground */
  for (i=pdata->nobjects-1; i>=0; i--) {
    int object = TARGET(i);
    if (!(O_FLAGS(object) & 0x4) && fr_intersects_object(pdata,/*bounding box*/1,object)) /* STRICT */
      relative_fr(wam, pdata,object,&pdata->fr.end_relative);
  }
}

static void
absolute_fr(Wam wam,
	    struct diffn_data *pdata,
	    int object,
	    int *region)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int i, j;

  /* forbidden regions wrt. other objects */
  for (i=pdata->fr.end_bb; i<pdata->fr.end_relative; i++) {
    if (FR_OBJ(i)!=object) {
      int fr = *region;
      for (j=0; j<kdims; j++) {
	FR_ORIGIN(fr,j) =  FR_ORIGIN(i,j) - dvar_min_l(dv+2*j+1);
	FR_END(fr,j) = FR_END(i,j);
	if (!useful_absolute_fr(pdata,fr,dv,j))
	  goto next_fr;
      }
      *region = store_region(wam, pdata,fr,FR_OBJ(i));
    }
  next_fr: ;
  }
}

static SP_BOOL
point_in_some_fr(struct diffn_data *pdata,
		 SP_integer *point,
		 int *region)
{
  int kdims = pdata->kdims;
  int i, j, k;
  int nbfr = pdata->fr.end_absolute - pdata->fr.end_relative;
  
  for (i=0, k=pdata->fr.round_robin; i<nbfr; i++, k++) {
    if (k==pdata->fr.end_absolute)
      k = pdata->fr.end_relative;
    for (j=0; j<kdims; j++)
      if (point[j]<FR_ORIGIN(k,j) || point[j]>FR_END(k,j))
	goto next;
    *region = k;
    pdata->fr.round_robin = k;
    return TRUE;
  next: ;
  }
  return FALSE;
}

static void
bb_init(struct diffn_data *pdata,
	int bb)
{
  int kdims = pdata->kdims;
  int j;

  for (j=0; j<kdims; j++) {
    FR_ORIGIN(bb,j) = CLPFD_MAXINT;
    FR_END(bb,j)    = -CLPFD_MAXINT;
  }
}

static void
bb_add_object(struct diffn_data *pdata,
	      int bb,
	      int object)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int j;

  for (j=0; j<kdims; j++) {
    SP_integer orig = dvar_min_l(dv+2*j);
    SP_integer end  = dvar_max_l(dv+2*j) + dvar_min_l(dv+2*j+1) - 1;
    if (FR_ORIGIN(bb,j) > orig)
      FR_ORIGIN(bb,j) = orig;
    if (FR_END(bb,j) < end)
      FR_END(bb,j) = end;
  }
}

static SP_BOOL
point_not_in_domain(struct diffn_data *pdata,
		    SP_integer *point,
		    int object,
		    int *region)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int j, k;
  
  for (j=0; j<kdims; j++)
    if (!dvar_contains_value_l(dv+2*j,point[j])) {
      int r = *region = pdata->fr.top_of_stack;
      for (k=0; k<kdims; k++)
	if (j==k) {
	  DVITER it;
	  SP_integer min, max;
	  SP_integer fr_end =     CLPFD_MAXINT-1;
	  SP_integer fr_origin = -fr_end;
	  
	  dviter_init(&it, dv+2*k);
	  while (!dviter_empty(&it)) {
	    dviter_next_interval_l(&it, &min, &max);
	    if (point[j]<min) {
	      fr_end = min-1;
	      break;
	    } else {
	      fr_origin = max+1;
	    }
	  }
	  FR_ORIGIN(r,k) = fr_origin;
	  FR_END(r,k) = fr_end;
	} else {
	  FR_ORIGIN(r,k) = dvar_min_l(dv+2*k);
	  FR_END(r,k) = dvar_max_l(dv+2*k);
	}
      return TRUE;
    }
  return FALSE;
}

/* Let O be flagged iff O_FLAGS(object)&0x1.

   We must resweep an object if any of the following holds:
   - object is flagged
   - object's bounding box intersects BB 0 (BB of all flagged objects)
 */
static SP_BOOL
must_sweep(struct diffn_data *pdata,
	   int object)
{
  if (O_FLAGS(object) & 0x4) {	/* STRICT */
    return FALSE;
  } else if (O_FLAGS(object) & 0x1) {
    return TRUE;
  } else if (fr_intersects_object(pdata,/*bounding box*/0,object)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

#define SWEEPSETNW(K) (((unsigned int)(K) + 31) >> 5)
#define SWEEPSETWORD(D) ((unsigned int)(D) >> 5)
#define SWEEPSETBIT(D) (1U << ((unsigned int)(D) & 31))

static void
init_sweepset(struct diffn_data *pdata)
{
  int n = SWEEPSETNW(pdata->kdims);
  int i;

  for (i=0; i<n; i++) {
    pdata->sweepset.min[i] = ~0U;
    pdata->sweepset.max[i] = ~0U;
  }
}

static SP_BOOL
test_sweepset(struct diffn_data *pdata, int d, SP_BOOL min)
{
  int wd = SWEEPSETWORD(d);
  int b = SWEEPSETBIT(d);
  
  if (min)
    return pdata->sweepset.min[wd] & b;
  else
    return pdata->sweepset.max[wd] & b;
}

static void
clear_sweepset(struct diffn_data *pdata, int d, SP_BOOL min)
{
  int wd = SWEEPSETWORD(d);
  int b = SWEEPSETBIT(d);
  
  if (min)
    pdata->sweepset.min[wd] &= ~b;
  else
    pdata->sweepset.max[wd] &= ~b;
}

static void
init_up(struct diffn_data *pdata,
	Dvar dv,
	int dim)
{
  int kdims = pdata->kdims;
  SP_BOOL atmin = TRUE;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    if (atmin)
      pdata->next[j1] = dvar_max_l(dv+2*j1)+1;
    else
      pdata->next[j1] = pdata->current[j1]+1;
    if (pdata->current[j1]>dvar_min_l(dv+2*j1))
      atmin = FALSE;
  }
}

static int
adjust_up(struct diffn_data *pdata,
	  Dvar dv,
	  int dim)
{
  int kdims = pdata->kdims;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    pdata->current[j1] = pdata->next[j1];
    pdata->next[j1] = dvar_max_l(dv+2*j1)+1;
    if (pdata->current[j1] < pdata->next[j1])
      return 0;			/* SUCCESS */
    pdata->current[j1] = dvar_min_l(dv+2*j1);
  }
  return -1;			/* FAILURE */
}

static SP_BOOL
inside_forbidden(struct diffn_data *pdata,
		 SP_integer *p,
		 int object,
		 int *region)
{
  return (point_not_in_domain(pdata,p,object,region) ||
	  point_in_some_fr(pdata,p,region));
}

/* returns -1 for failure, 0 for no change, >0 for pruned */
static int
prune_min(struct diffn_data *pdata,
	  int object,
	  int dim)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int region;
  int rc = 0;
  int j;

  for (j=0; j<kdims; j++)
    pdata->current[j] = dvar_min_l(dv+2*j);
  init_up(pdata,dv,dim);
  while (rc>=0 &&
	 inside_forbidden(pdata,pdata->current,object,&region)) {
    for (j=0; j<kdims; j++)
      if (pdata->next[j] > FR_END(region,j)+1)
	pdata->next[j] = FR_END(region,j)+1;
    rc = adjust_up(pdata, dv, dim);
  }
  if (rc>=0) {
    rc = dvar_fix_min_l(dv+2*dim,pdata->current[dim]);
    if (rc>0) {
      O_FLAGS(object) |= 0x3;	/* pruned, queued */
    }
    for (j=0; j<kdims; j++) {
      if (pdata->current[j] == dvar_min_l(dv+2*j))
	clear_sweepset(pdata,j,TRUE/*min*/);
      if (pdata->current[j] == dvar_max_l(dv+2*j))
	clear_sweepset(pdata,j,FALSE/*max*/);
    }
  }
  return rc;
}

static void
init_down(struct diffn_data *pdata,
	  Dvar dv,
	  int dim)
{
  int kdims = pdata->kdims;
  SP_BOOL atmax = TRUE;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    if (atmax)
      pdata->next[j1] = dvar_min_l(dv+2*j1)-1;
    else
      pdata->next[j1] = pdata->current[j1]-1;
    if (pdata->current[j1]<dvar_max_l(dv+2*j1))
      atmax = FALSE;
  }
}

static int
adjust_down(struct diffn_data *pdata,
	    Dvar dv,
	    int dim)
{
  int kdims = pdata->kdims;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    pdata->current[j1] = pdata->next[j1];
    pdata->next[j1] = dvar_min_l(dv+2*j1)-1;
    if (pdata->current[j1] > pdata->next[j1])
      return 0;			/* SUCCESS */
    pdata->current[j1] = dvar_max_l(dv+2*j1);
  }
  return -1;			/* FAILURE */
}

/* returns -1 for failure, 0 for no change, >0 for pruned */
static int
prune_max(struct diffn_data *pdata,
	  int object,
	  int dim)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int region;
  int rc = 0;
  int j;

  for (j=0; j<kdims; j++)
    pdata->current[j] = dvar_max_l(dv+2*j);
  init_down(pdata,dv,dim);
  while (rc>=0 &&
	 inside_forbidden(pdata,pdata->current,object,&region)) {
    for (j=0; j<kdims; j++)
      if (pdata->next[j] < FR_ORIGIN(region,j)-1)
	pdata->next[j] = FR_ORIGIN(region,j)-1;
    rc = adjust_down(pdata, dv, dim);
  }
  if (rc>=0) {
    rc = dvar_fix_max_l(dv+2*dim,pdata->current[dim]);
    if (rc>0) {
      O_FLAGS(object) |= 0x3;	/* pruned, queued */
    }
    for (j=0; j<kdims; j++) {
      if (pdata->current[j] == dvar_min_l(dv+2*j))
	clear_sweepset(pdata,j,TRUE/*min*/);
      if (pdata->current[j] == dvar_max_l(dv+2*j))
	clear_sweepset(pdata,j,FALSE/*max*/);
    }
  }
  return rc;
}

static int
filter_object(Wam wam,
	      struct diffn_data *pdata,
	      int object)
{
  int kdims = pdata->kdims;
  int dim, change=0;
  Dvar dv = O_ORIG_DVAR(object,0);

  pdata->fr.end_absolute = pdata->fr.end_relative;
  absolute_fr(wam, pdata,object,&pdata->fr.end_absolute);
  pdata->fr.top_of_stack = pdata->fr.end_absolute;
  pdata->fr.round_robin = pdata->fr.end_absolute;
  switch (pre_sweep(wam, pdata,object)) {
  case -1:
    return 2;
  case 0:
    break;
  default:
    change = 1;
  }
  init_sweepset(pdata);
  for (dim=0; dim<kdims-1; dim++)
    if (!dvar_is_integer(dv+2*dim))
      break;
  for (; dim<kdims; dim++) {
    if (test_sweepset(pdata,dim,TRUE/*min*/)) {
      switch (prune_min(pdata,object,dim)) {
      case -1:
	return 2;
      case 0:
	break;
      default:
	change = 1;
      }
    }
    if (test_sweepset(pdata,dim,FALSE/*max*/)) {
      switch (prune_max(pdata,object,dim)) {
      case -1:
	return 2;
      case 0:
	break;
      default:
	change = 1;
      }
    }
  }
  return change;
}

static int
filter_diffn(Wam wam, struct diffn_data *pdata)
{
  int i, rc = 0;

  all_relative_fr(wam, pdata);
  for (i=0; i<pdata->ntargets && rc<2; i++)
    if (must_sweep(pdata,TARGET(i)))
      rc |= filter_object(wam, pdata,TARGET(i));
  return rc;
}


static void
refresh_object(struct diffn_data *pdata,
	       int object)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int j;

  O_FLAGS(object) &= ~0x4;
  for (j=0; j<kdims; j++) {
    dvar_refresh(dv+2*j);
    dvar_refresh(dv+2*j+1);
    if (!pdata->strict && dvar_min_l(dv+2*j+1) <= 0)
      O_FLAGS(object) |= 0x4;
  }
}

static void
diffn_init(struct diffn_data *pdata)
{
  int i;

  /* Compute in region 0 the bounding box of all recently pruned objects, TEMPORARY */
  /* Compute in region 1 the bounding box of all objects not known to be ground,
     PERSISTENT during this iteration of the "loop". */
  pdata->fr.end_bb = FIRST_BB;
  bb_init(pdata,/*bounding box*/0); /* bounding box of recently queued objects */
  bb_init(pdata,/*bounding box*/1); /* bounding box of TARGET objects */
  for (i=0; i<pdata->ntargets; i++) {
    int object = TARGET(i);
    if (!(O_FLAGS(object) & 0x4)) { /* STRICT */
      bb_add_object(pdata,/*bounding box*/1,object);
      if (O_FLAGS(object) & 0x1) { /* queued? */
	bb_add_object(pdata,/*bounding box*/0,object);
      }
    }
  }
}

#define ARG_NT 4

static DAEMON_RC SPCDECL 
diffn_daemon(Wam wam,
	     void *vdata,
	     SP_globref attr_ref,
	     TAGGED *global)
{
  struct diffn_data *pdata = (struct diffn_data *)vdata;
  int kdims = pdata->kdims;
  TAGGED tstate;
  int incremental, ar, object;
  SP_integer state_stamp;
  SP_BOOL buried;

  object = (int)((attr_ref - pdata->refbase)/(4*kdims));
  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  incremental = (pdata->stamp==state_stamp);
  if (!incremental) {
    TAGGED tmp;
    int i;
    
    pdata->stamp = state_stamp;
    DerefArg(tmp,tstate,ARG_NT);
    pdata->ntargets = GetSmall_int(tmp);
    for (i=0; i<pdata->ntargets; i++) {
      int obj = TARGET(i);
      O_FLAGS(obj) &= ~0x3;	/* not queued, not pruned */
    }
  }
  O_FLAGS(object) |= 0x1;	/* queued */
  (void)fd_daemon_copy_state(wam,global,&buried);
  pdata->stamp++;
  return DAEMON_NOFIX;
}

/*
   '$fd_diffn'(+State0, -State, -Actions) :-
   State0 = State = f(KDims,Objects,Strict,NTargets,Handle,Stamp)
   Object = list of Facet
   Facet = [Orig-OrigAttr,Size-SizeAttr]
 */
void SPCDECL
prolog_fd_diffn(Wam wam,
		SP_term_ref State0,
		SP_term_ref State,
		SP_term_ref Actions)
{
  struct diffn_data *pdata;
  int kdims, i, j, rc;
  int ent = -1;			/* disentailed unless otherwise */
  int ntargets_at_entry;
  SP_BOOL committed;
  TAGGED handle, tmp;
  
  /*    X(0) = RefTerm(State0); */
  (void)State0;
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct diffn_data,handle);
    kdims = pdata->kdims;
  } else {			/* build persistent state */
    int strict, nobjects;
    SP_integer total_size;
    char *ptr;
    SP_globref ref;
    
    DerefArg(tmp,X(0),1);	/* KDims */
    kdims = GetSmall_int(tmp);
    DerefArg(tmp,X(0),3);	/* Strict */
    strict = GetSmall_int(tmp);
    DerefArg(tmp,X(0),ARG_NT);	/* NTargets */
    nobjects = GetSmall_int(tmp);
    total_size =
      2*kdims*nobjects*sizeof(struct dvar) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      2*SWEEPSETNW(kdims)*sizeof(unsigned int) +
      nobjects*sizeof(int) +
      nobjects*sizeof(int);
    pdata = Palloc(struct diffn_data, total_size, handle); /* GC */
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += 2*kdims*nobjects*sizeof(struct dvar);
    pdata->current = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->next = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->target = (int *)ptr;
    ptr += nobjects*sizeof(int);
    pdata->flags = (int *)ptr;
    ptr += nobjects*sizeof(int);
    pdata->sweepset.min = (unsigned int *)ptr;
    ptr += SWEEPSETNW(kdims)*sizeof(unsigned int);
    pdata->sweepset.max = (unsigned int *)ptr;
    ptr += SWEEPSETNW(kdims)*sizeof(unsigned int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    for (i=0; i<nobjects; i++) {
      O_FLAGS(i) = 0x1;		/* queued */
    }
    pdata->destructor = diffn_destructor;
    pdata->daemon = diffn_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = nobjects*4*kdims;
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    pdata->stamp = 0;
    pdata->kdims = kdims;
    pdata->strict = strict;
    pdata->nobjects = nobjects;
    pdata->ntargets = nobjects;
    DerefArg(tmp,X(0),2);	/* get Objects */
    ref = pdata->refbase;
    for (i=0; i<nobjects; i++) {
      TAGGED object, facet, t3;
      DerefCar(object,tmp);
      DerefCdr(tmp,tmp);
      for (j=0; j<kdims; j++) {
	DerefCar(facet,object);
	DerefCdr(object,object);
	DerefCar(t3,facet);	/* get orig var */
	fd_get_var_and_attr(t3,ref);
	ref += 2;
	DerefCdr(facet,facet);
	DerefCar(t3,facet);	/* get size var */
	fd_get_var_and_attr(t3,ref);
	ref += 2;
      }
    }
    for (i=0; i<nobjects; i++) {
      SP_globref ref = O_ORIG_ATTR(i,0);
      Dvar dv = O_ORIG_DVAR(i,0);
      TARGET(i) = i;
      for (j=0; j<kdims; j++) {
	dvar_init(dv+2*j, ref, ref+1);
	dvar_attach_daemon(wam, dv+2*j, pdata, X(1), fd.functor_minmax);
	ref += 2;
	dvar_init(dv+2*j+1, ref, ref+1);
	dvar_attach_daemon(wam, dv+2*j+1, pdata, X(1), fd.functor_min);
	ref += 2;
      }
    }
  }
  
  /* RESUME */
  dvar_export_start(wam);
  {
    int nregions = pdata->nobjects;
    size_t total_size;
    char *ptr;

    if (nregions<FIRST_BB+2)
      nregions = FIRST_BB+2; /* regions 0 and 1 are used by the while loop below */
    pdata->nregions = nregions;
    total_size =
      nregions*sizeof(int) +
      kdims*nregions*sizeof(SP_integer) +
      kdims*nregions*sizeof(SP_integer);

    ptr = SP_malloc(total_size);
    pdata->fr.origin = (SP_integer *)ptr;
    ptr += kdims*nregions*sizeof(SP_integer);
    pdata->fr.end = (SP_integer *)ptr;
    ptr += kdims*nregions*sizeof(SP_integer);
    pdata->fr.obj = (int *)ptr;
    ptr += nregions*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata->fr.origin)+total_size);
  }

  for (i=0; i<pdata->ntargets; i++)
    refresh_object(pdata, TARGET(i));
 loop:
  diffn_init(pdata);

  pdata->fr.end_relative = pdata->fr.end_bb;
  pdata->fr.end_absolute = pdata->fr.end_bb;
  pdata->fr.top_of_stack = pdata->fr.end_bb;

  rc = filter_diffn(wam, pdata);
  switch (rc) {
  case 0x3:
  case 0x2:
    goto ret;
  case 0x1:
    goto loop;
  }

  ntargets_at_entry = pdata->ntargets;
  {
    int inf = 0;
    int sup = pdata->ntargets-1;
    int held = TARGET(sup); /* sup is the hole */
    int current = TARGET(inf);
    
    while (inf<=sup) {
      Dvar dv = O_ORIG_DVAR(current,0);
      SP_BOOL ground = TRUE;
      for (j=0; j<kdims && ground; j++) {
	ground &= (dvar_is_integer(dv+2*j) ? 1 : 0);
	ground &= (dvar_is_integer(dv+2*j+1) ? 1 : 0);
      }
      if (!ground) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    pdata->ntargets = inf;
  }
  for (i=0; i<ntargets_at_entry; i++) {
    int object = TARGET(i);
    if (O_FLAGS(object) & 0x2) { /* pruned? */
      Dvar dv = O_ORIG_DVAR(object,0);
      for (j=0; j<kdims; j++)
	dvar_pruning_done( dv+2*j);
    }
  }
  for (i=0; i<ntargets_at_entry; i++) {
    int current = TARGET(i);
    if (O_FLAGS(current) & 0x2) { /* pruned? */
      Dvar dv = O_ORIG_DVAR(current,0);
      for (j=0; j<kdims; j++)
	dvar_export(dv+2*j);
    }
    O_FLAGS(current) &= ~0x3;	/* not queued, not pruned */
  }
  
  CTagToArg(X(0),ARG_NT) = MakeSmall(pdata->ntargets);
  ent = (pdata->ntargets==0);
 ret:
  SP_free(pdata->fr.origin);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}
