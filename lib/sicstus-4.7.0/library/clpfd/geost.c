/* TODO: some SP_integer -> int */

/* Copyright(C) 2006, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

/* OPT_LONGEST_HOLE_ALL implies OPT_LONGEST_HOLE */

#define OPT_LEX              0x1     // noop
#define OPT_DISJUNCTIVE      0x2
#define OPT_CUMULATIVE       0x10
#define OPT_POLYMORPHISM     0x20    // noop
#define OPT_PARCONFLICT      0x40
#define OPT_VISAVIS_INIT     0x80
#define OPT_VISAVIS          0x100   // noop
#define OPT_CORNERS          0x200   // noop
#define OPT_TASK_INTERVALS   0x400   // noop
#define OPT_DP               0x800
#define OPT_LONGEST_HOLE     0x1000
#define OPT_LONGEST_HOLE_ALL 0x2000
#define OPT_VISAVIS_FLOATING 0x4000  // noop
#define OPT_PALLET_LOADING   0x8000
#define OPT_RULES            0x10000
#define OPT_OVERLAP          0x20000
#define OPT_VOLUME           0x40000



#define CUMULATIVE_METHOD 1

#define LONGEST_HOLE 1

#define PARCONFLICT_METHOD 1

#define HOLE_METHOD 1


/* the rest are not settable by options */

#define PALLET_LOADING 1

#define EXPLICIT_FBOXES 1

#define REFS_PER_OBJECT (2*kdims+2)

/* used in overflow tests */
#define MAXSPINT ((SP_integer)((((SP_uinteger)1)<<(WORDSIZE-1))-5))

/* regions 0, 1, 2, 3 are reserved bounding boxes */
#define FIRST_BB 4

struct geost_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_globref refbase;		/* [nobjects*REFS_PER_OBJECT] */
  SP_integer stamp;
  SP_integer tick;
  SP_integer exact_slack;		/* (placement space) - (total size of objects in it) */
  SP_integer slack;			/* (placement space) - (total size of DECOMPOSABLE objects in it) */
  SP_integer target_avail;		/* (placement space) - (total overlap from non-targets) */
  SP_integer rest_volume;
  int kdims;
  int nobjects;
  int nsboxes_posted;		/* number of sboxes given */
  int nsboxes_allocated;	/* number of sboxes that we have memory for */
  int max_nsboxes;		/* max number of sboxes in any shape */
  int nregions;
  int ntargets;
  int nrefs;
  int opt;
  int overflow;
  int curdim;
  int norder;
  int prunedsid;
  struct sw_on_key *shape_hash;
  struct task *task;		/* volatile */
  SP_integer *fixorder;		/* [norder] */
  SP_integer *current_point;		/* [kdims], used in continuations */
  int  current_object;		/*          used in continuations */
  SP_integer current_sid;		/*          used in continuations */
  SP_integer *current;		/* [kdims] */
  SP_integer *next;			/* [kdims] */
  SP_integer *dvar_min;		/* [kdims] */
  SP_integer *dvar_max;		/* [kdims] */
  SP_integer *pruneddim;		/* [kdims] */
  SP_integer *target;			/* [nobjects] */
  SP_integer *byclass;		/* [nobjects] */
  SP_integer *byarea;			/* [nobjects] */
  SP_integer *maxeps;			/* [kdims] */
  SP_integer **lch_tables;		/* [kdims] */
  SP_integer **lh_tables;		/* [kdims] */
  SP_integer **lh_tables_max;		/* [kdims] */
  SP_integer maxbacks;
  SP_integer check_cumulative_set_count;   /* #array entries */
  SP_integer *check_cumulative_set;   /* [nobjects*max_nsboxes+1] */
  SP_integer *cumulative_sbox;	/* [max_nsboxes] */
  struct sw_on_key **dp_hash;	/* [kdims] */
  Dvar dvar;			/* [(nobjects+2)*(kdims+1)] */
  Dvar fix_dvar;		/* [1] */
  Dvar volume_dvar;		/* [1] */
  Dvar lower_dvar;		/* [kdims] */
  Dvar upper_dvar;		/* [kdims] */
  SP_integer *virtual_min;	/* [kdims] */
  SP_integer *virtual_max;	/* [kdims] */
  struct {
    unsigned int *min;		/* TODO for >32 */
    unsigned int *max;		/* TODO for >32 */
  } sweepset;
  struct {
    struct geost_proc **proc;		/* [nobjects+2] */
    SP_integer *flags;		/* [nobjects+2] */
    SP_integer *oid;			/* [nobjects+2] */
    SP_integer *min_vol_sid;		/* [nobjects+2] */
    SP_integer *max_vol_sid;		/* [nobjects+2] */
  } object;
  struct {
    SP_integer *max_size;		/* [kdims] */
    SP_integer *max_trans_size;	/* [kdims] */
    SP_integer *min_trans;		/* [kdims] */
    SP_integer *sid;			/* [nsboxes_allocated] */
    SP_integer *translation;		/* [nsboxes_allocated*kdims] */
    SP_integer *size;			/* [nsboxes_allocated*kdims] */
    SP_integer *size_multiset;	/* [nsboxes_allocated*kdims] */
    SP_integer *min;			/* [nsboxes_allocated] */
    SP_integer *max;			/* [nsboxes_allocated] */
    SP_integer *cur;			/* [nsboxes_allocated] */
    SP_integer *useful;		/* [nsboxes_allocated] */
    SP_integer *volume;		/* [nsboxes_allocated] */
    SP_integer *svolume;		/* [nsboxes_allocated] */
    SP_integer *boundaries;		/* [nsboxes_allocated*kdims*2] */
  } shape;
  struct {
    int end_bb;
#if EXPLICIT_FBOXES
    int end_relative;
    int end_absolute;
#endif
    int top_of_stack;
    int round_robin;
    int heapsize;
    int nactive;
    SP_integer *obj;			/* [nregions] volatile */
    SP_integer *origin;               /* [nregions*kdims] : region origin, inclusive volatile */
    SP_integer *end;		        /* [nregions*kdims] : region end, inclusive volatile */
    SP_integer *heap;			/* [2*nregions] volatile */
  } fr;
  struct size_mem size_mem[3];	/* [0]: callers of parallel_conflict,
				        unary_alloc from fd_discrete_filtering,
				   [1]: parallel_conflict,
				        fd_discrete_filtering,
				        slack_filtering,
				   [2]: unary_alloc from unary_filtering,
				*/
#if PALLET_LOADING
  struct {
    int  sbox;
    int  nholes;
    SP_integer pslack;
    SP_integer *bborig;		/* FR_ORIGIN(1,j) at post time */
    SP_integer *bbend;		/* FR_END(1,j)    at post time */
    SP_integer *holes;
    SP_integer *islin;
  } pl;
#endif
};

typedef SP_BOOL (geost_cont)(Wam wam, struct geost_data *pdata, int *region);

typedef SP_BOOL (geost_inside_fr) (struct geost_data *pdata, int o2, SP_integer limit, SP_integer *point);

typedef void    (geost_maximize_fbox) (struct geost_data *pdata, int o1, int o2, SP_integer limit, int opt, int *region);


struct geost_proc {
  struct geost_proc *next;
  SP_integer self;
  SP_integer dest;
  SP_integer nreg;
  SP_integer nstack;
  SP_integer ncode;
  SP_integer ndep;
  SP_integer *code;
  SP_integer *dep;
};

struct dp {
  struct dp *next;
  SP_integer lslack;
  SP_integer rslack;
  SP_integer sslack;
  int n;
}; /* followed by n unsigned shorts */

static void
dispose_dp_hash(Wam wam,
		      struct geost_data *pdata)
{
  int d, kdims = pdata->kdims;
  SP_WORD i;

  for (d=0; d<kdims; d++) {
    struct sw_on_key *sw = pdata->dp_hash[d];
    if (sw) {
      pdata->dp_hash[d] = NULL;
      for (i=SwitchSize(sw)-1; i>=0; --i) {
	struct sw_on_key_node *keyval = &sw->tab.asnode[i];
	struct dp *node = keyval->value.any;
	
	while (node) {
	  struct dp *next = node->next;
	  sp_checkdealloc((TAGGED *)node,
			  sizeof(*node)+node->n*sizeof(unsigned short), FALSE);
	  node = next;
	}
      }
      dispose_switch_on_key(sw);
    }
  }
}

static void
geost_alloc_procs(Wam wam,
			struct geost_data *pdata,
			TAGGED list);

static void
geost_free_procs(Wam wam,
		       struct geost_data *pdata);

static SP_BOOL
point_breaks_rule(Wam wam,
			struct geost_data *pdata,
			int object,
			SP_integer sid,
			SP_integer *point,
			int *region);

static SP_BOOL
emulate_rule(Wam wam,
		   struct geost_data *pdata,
		   int object,
		   SP_integer sid,
		   SP_integer *point,
		   int *region,
		   struct geost_proc *geost_proc);

static void SPCDECL 
geost_destructor(void *pdata_v)
{
  struct geost_data *pdata = (struct geost_data *)pdata_v;
  int d;
  FD_SETUP_SPENV(pdata->spenv);

  for (d=0; d<3; d++)
    SP_free(pdata->size_mem[d].mem);

#if LONGEST_HOLE
  for (d=0; d<pdata->kdims; d++) {
    if (pdata->lch_tables[d])
      Free(pdata->lch_tables[d]);
    if (pdata->lh_tables[d])
      Free(pdata->lh_tables[d]);
    if (pdata->lh_tables_max[d])
      Free(pdata->lh_tables_max[d]);
  }
#endif
#if PALLET_LOADING
  if (pdata->pl.holes)
    Free(pdata->pl.holes);
  if (pdata->pl.islin)
    Free(pdata->pl.islin);
#endif
  dispose_switch_on_key(pdata->shape_hash);
  dispose_dp_hash(wam, pdata);
  if (pdata->opt & OPT_RULES)
    geost_free_procs(wam, pdata);
  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

#define TARGET(i) (pdata->target[i])
#define O_PROC(o) (pdata->object.proc[o])
#define O_FLAGS(o) (pdata->object.flags[o]) /* 1=pruned, 2=queued, 4=subject to sweep */
#define O_OID(o) (pdata->object.oid[o])
#define O_MIN_VOL_SID(o) (pdata->object.min_vol_sid[o])
#define O_MAX_VOL_SID(o) (pdata->object.max_vol_sid[o])
#define O_SID_DVAR(o) (pdata->dvar + (kdims+1)*(o))
#define O_SID_ATTR(o) (pdata->refbase + REFS_PER_OBJECT*(o))
#define O_ORIG_DVAR(o,j) (pdata->dvar + (kdims+1)*(o) + (j) + 1)
#define O_ORIG_ATTR(o,j) (pdata->refbase + REFS_PER_OBJECT*(o) + 2*(j) + 2)
#define S_SID(i) (pdata->shape.sid[i])
#define S_TRANSLATION(i,j) (pdata->shape.translation[kdims*(i)+(j)])
#define S_SIZE(i,j) (pdata->shape.size[kdims*(i)+(j)])
#define S_SIZE_MULTISET(i,j) (pdata->shape.size_multiset[kdims*(i)+(j)])
#define S_MAX_SIZE(i) (pdata->shape.max_size[i])
#define S_MAX_TRANS_SIZE(i) (pdata->shape.max_trans_size[i])
#define S_MIN_TRANS(i) (pdata->shape.min_trans[i])
#define S_VOLUME(i) (pdata->shape.volume[i])
#define SHAPE_VOLUME(i) (pdata->shape.svolume[i])
#define FR_ORIGIN(i,j) (pdata->fr.origin[kdims*(i) + (j)])
#define FR_END(i,j) (pdata->fr.end[kdims*(i) + (j)])
#define FR_OBJ(i) (pdata->fr.obj[i])
#define FR_OID(i) O_OID(FR_OBJ(i))
#define get_first_sbox(sid) ((int)incore_gethash(pdata->shape_hash,MakeSmall(sid))->value.arities)
#define AREF2(Arr,Dim,Size,Len) Arr[(Len)*(Dim) + (Size)]


/* N.B. xref pdata->nrefs */
#define FIX_ATTR (pdata->refbase + REFS_PER_OBJECT*pdata->nobjects)
#define VOLUME_ATTR (FIX_ATTR + 2)
#define LOWER_ATTR (VOLUME_ATTR + 2)
#define UPPER_ATTR (LOWER_ATTR + 2*kdims)

#if PARCONFLICT_METHOD
static int
parallel_conflict(Wam wam,
		  int k,
		  int n,
		  int heavy_check,
		  SP_integer *sizes,
		  SP_integer *earliest,
		  SP_integer *latest,
		  int sizrowlen,
		  struct size_mem *size_mem);
#endif


static SP_BOOL
object_is_ground(struct geost_data *pdata,
		 int object)
{
  int kdims = pdata->kdims;
  Dvar dvs = O_SID_DVAR(object);
  Dvar dv = O_ORIG_DVAR(object,0);
  int j;

  if (!dvar_is_integer(dvs))
    return FALSE;
  for (j=0; j<kdims; j++)
    if (!dvar_is_integer(dv+j))
      return FALSE;

  return TRUE;
}


#if HOLE_METHOD || CUMULATIVE_METHOD || GTI_METHOD
static SP_integer 
min_intersection(struct geost_data *pdata,
		 int r1,
		 int r2,
		 SP_integer *size1,
		 SP_integer *size2,
		 int dim)
{
  int kdims = pdata->kdims;
  SP_integer o1 = FR_ORIGIN(r1,dim);
  SP_integer e1 = FR_END(r1,dim)+1;
  SP_integer s1 = size1[dim];
  SP_integer o2 = FR_ORIGIN(r2,dim);
  SP_integer e2 = FR_END(r2,dim)+1;
  SP_integer s2 = size2[dim];
  SP_integer j1 = o1 - e2 + s1+s2;
  SP_integer j2 = o2 - e1 + s1+s2;
  if (j1 > j2)
    j1 = j2;
  if (j1 > s1)
    j1 = s1;
  if (j1 > s2)
    j1 = s2;
  if (j1 < 0)
    j1 = 0;
  return j1;
}
  

/* compute max intersection between two rectangles
   r1 - region of rect 1
   r2 - region of rect 2
   size1 - size of rect 1
   size2 - size of rect 2
   dim - dimension to consider
*/
static SP_integer 
max_intersection(struct geost_data *pdata,
		 int r1,
		 int r2,
		 SP_integer *size1,
		 SP_integer *size2,
		 int dim)
{
  int kdims = pdata->kdims;
  SP_integer j1 = FR_END(r2,dim) - FR_ORIGIN(r1,dim) + 1;
  SP_integer j2 = FR_END(r1,dim) - FR_ORIGIN(r2,dim) + 1;
  if (j1 > j2)
    j1 = j2;
  if (j1 <= 0)
    return 0;
  if (j1 > size1[dim])
    j1 = size1[dim];
  if (j1 > size2[dim])
    j1 = size2[dim];
  return j1;
}

/* compute max intersection between an sbox and a rectangle
   dv - origin dvar vector of object
   sbox - sbox of object
   r0 - scratchpad region
   r1 - region of rectangle
   size1 - size of rectangle
   dim - dimension to consider
*/
static SP_integer 
sbox_max_intersection(struct geost_data *pdata,
		      Dvar dv,	/* origin of an object */
		      int sbox,	/* its sbox */
		      int r0,	/* scratchpad sbox */
		      int r1,	/* absolute region */
		      SP_integer *size1, /* its size */
		      int dim)
{
  int kdims = pdata->kdims;
  SP_integer limit = S_SIZE(sbox,dim) < size1[dim] ? S_SIZE(sbox,dim) : size1[dim];
  SP_integer sofar=0;

  if (dvar_is_interval(dv+dim)) {
    FR_ORIGIN(r0,dim) = dvar_min_l(dv+dim) + S_TRANSLATION(sbox,dim);
    FR_END(r0,dim) = dvar_max_l(dv+dim) + S_TRANSLATION(sbox,dim) + S_SIZE(sbox,dim) - 1;
    sofar = max_intersection(pdata,r0,r1,&S_SIZE(sbox,0),size1,dim);
  } else {
    SP_integer min, max, next;
    DVITER it;
    
    dviter_init(&it, dv+dim);
    while (!dviter_empty(&it) && sofar<limit) {
      dviter_next_interval_l(&it, &min, &max);
      FR_ORIGIN(r0,dim) = min + S_TRANSLATION(sbox,dim);
      FR_END(r0,dim) = max + S_TRANSLATION(sbox,dim) + S_SIZE(sbox,dim) - 1;
      next = max_intersection(pdata,r0,r1,&S_SIZE(sbox,0),size1,dim);
      sofar = sofar > next ? sofar : next;
    }
  }
  return sofar;
}
#endif

#if DBG
extern void dump_region(struct geost_data *pdata, int i);

extern void dump_objects(struct geost_data *pdata);

extern void check_solution(struct geost_data *pdata);
#endif


#if EXPLICIT_FBOXES
/* -1 - no join, 0 - equal, 1 - join */
static int
join_regions(struct geost_data *pdata,
	     int fr1,
	     int fr2)
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
#endif

static void
init_first_sbox(Wam wam,
		       struct geost_data *pdata)
{
  int j, size=4, pop=2;

  for (j=1; j<pdata->nsboxes_posted; j++)
    if (S_SID(j-1)!=S_SID(j))
      pop++;
  for (j=3; pop>j; j<<=1)
    size<<=1;
  pdata->shape_hash = new_switch_on_key(size,NULL);
  dyn_puthash(&pdata->shape_hash,MakeSmall(S_SID(0)))->value.arities = 0;
  for (j=1; j<pdata->nsboxes_posted; j++)
    if (S_SID(j-1)!=S_SID(j))
      dyn_puthash(&pdata->shape_hash,MakeSmall(S_SID(j)))->value.arities = j;
  /* add hash entry for shape beyond last one; xref "sid+1" */
  dyn_puthash(&pdata->shape_hash,MakeSmall(S_SID(j-1)+1))->value.arities = j;
}


static SP_BOOL
sbox_intersects_region(struct geost_data *pdata,
		       Dvar dv,
		       int s,
		       int fr1)
{
  int j;
  int kdims = pdata->kdims;
  for (j=0; j<kdims; j++) {
    SP_integer orig = dvar_min_l(dv+j) + S_TRANSLATION(s,j);
    SP_integer end  = dvar_max_l(dv+j) + S_TRANSLATION(s,j) + S_SIZE(s,j) - 1;
    if (end<FR_ORIGIN(fr1,j) || FR_END(fr1,j)<orig)
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
fr_intersects_object(Wam wam,
			  struct geost_data *pdata,
			  int fr1,
			  int object)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  DVITER it;

  dviter_init(&it, O_SID_DVAR(object));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    int s;
  
    for (s=sbox0; S_SID(s)==sid; s++)
      if (sbox_intersects_region(pdata,dv,s,fr1))
	return TRUE;
  }
  return FALSE;
}

static SP_BOOL
decomposable(struct geost_data *pdata,
	     int object)
{
  int kdims = pdata->kdims;
  Dvar dvs = O_SID_DVAR(object);
  /*
  SP_integer sid = dvar_min_l(dvs);
  int sbox0 = get_first_sbox(sid);
  */
  return (dvar_is_integer(dvs)/* && SHAPE_VOLUME(sbox0)==SHAPE_TOTAL_VOLUME(sbox0)*/);
}

static void
expand_regions(Wam wam,
		     struct geost_data *pdata)
{
  int n = pdata->nregions;
  int kdims = pdata->kdims;
  
  size_t total_size =
    2*n*sizeof(SP_integer)+
    2*n*kdims*sizeof(SP_integer)+
    2*n*kdims*sizeof(SP_integer)+
    4*n*sizeof(SP_integer);
  char *ptr = SP_realloc(pdata->fr.obj, total_size);
      
  pdata->fr.obj = (SP_integer *)ptr;
  ptr += 2*n*sizeof(SP_integer);
  pdata->fr.origin = (SP_integer *)ptr;
  ptr += 2*n*kdims*sizeof(SP_integer);
  pdata->fr.end = (SP_integer *)ptr;
  ptr += 2*n*kdims*sizeof(SP_integer);
  pdata->fr.heap = (SP_integer *)ptr;
  ptr += 4*n*sizeof(SP_integer);
  SP_ASSERT(ptr == (char *)(pdata->fr.obj)+total_size);
  memmove(pdata->fr.heap,   pdata->fr.obj+n*(1+2*kdims), n*2*sizeof(SP_integer));
  memmove(pdata->fr.end,    pdata->fr.obj+n*(1+kdims),   n*kdims*sizeof(SP_integer));
  memmove(pdata->fr.origin, pdata->fr.obj+n,             n*kdims*sizeof(SP_integer));
/*   memmove(pdata->fr.obj   , pdata->fr.obj  ,          n*sizeof(SP_integer)); */
  pdata->nregions = 2*n;
}

#if EXPLICIT_FBOXES

static int
store_region(Wam wam,
		   struct geost_data *pdata,
		   int new,
		   int object,
		   SP_BOOL subsumption)
{
  int q = new+1;
  int kdims = pdata->kdims;
  int p, j;
  SP_BOOL p_subsumes = FALSE;

  FR_OBJ(new) = object;
  if (new>pdata->fr.end_relative && join_regions(pdata,new-1,new)!=-1)
    return new;

  if (subsumption) {
    q = pdata->fr.end_relative;
    for (p=q; p<new && !p_subsumes; p++) {
      SP_BOOL p_subsumed = TRUE;
      p_subsumes = TRUE;
      for (j=0; j<kdims; j++) {
	if (FR_ORIGIN(p,j) > FR_ORIGIN(new,j) || FR_END(p,j) < FR_END(new,j))
	  p_subsumes = FALSE;
	if (FR_ORIGIN(p,j) < FR_ORIGIN(new,j) || FR_END(p,j) > FR_END(new,j))
	  p_subsumed = FALSE;
      }
      if (!p_subsumed || p_subsumes) {
	if (p>q) {
	  FR_OBJ(q) = FR_OBJ(p);
	  for (j=0; j<kdims; j++) {
	    FR_ORIGIN(q,j) = FR_ORIGIN(p,j);
	    FR_END(q,j) = FR_END(p,j);
	  }
	}
	q++;
      }
    }
    for (; p<new+1; p++) {
      if (p<new || !p_subsumes) {
	if (p>q) {
	  FR_OBJ(q) = FR_OBJ(p);
	  for (j=0; j<kdims; j++) {
	    FR_ORIGIN(q,j) = FR_ORIGIN(p,j);
	    FR_END(q,j) = FR_END(p,j);
	  }
	}
	q++;
      }
    }
  }
  if (q > new) { /* INVARIANT: ensure that region q is available */
    if (q == pdata->nregions)
      expand_regions(wam, pdata);
  }
  return q;
}

static SP_BOOL
inside_forbidden_cont(Wam wam,
			    struct geost_data *pdata,
			    int *region)
{
  (void)wam;
  (void)pdata;
  (void)region;
  return FALSE;
}

static SP_BOOL
absolute_cont(Wam wam,
		    struct geost_data *pdata,
		    int *region)
{
  *region = store_region(wam, pdata,*region,pdata->current_object,0);
  return FALSE;
}

static SP_BOOL
relative_cont(Wam wam,
		    struct geost_data *pdata,
		    int *region)
{
  *region = store_region(wam, pdata,*region,pdata->current_object,0);
  return FALSE;
}

static SP_BOOL
relative_fr(Wam wam,
		  struct geost_data *pdata,
		  int object,
		  int *region,
		  geost_cont *cont);

static void
all_relative_fr(Wam wam,
		      struct geost_data *pdata,
		      int fixall)
{
  int i;
  
  pdata->fr.end_relative = pdata->fr.end_bb;
  /* Nicolas says; generate FR for ground objects before nonground */
  for (i=pdata->nobjects-1; i>=0; i--) {
    int object = (int)TARGET(i);
    if ((!fixall || object_is_ground(pdata,object)) &&
	fr_intersects_object(wam, pdata,/*bounding box*/1,object))
      relative_fr(wam, pdata,object,&pdata->fr.end_relative,relative_cont);
  }
}

/* returns -1 for failure, 0 for no change, >0 for pruned */
static int
pre_sweep(Wam wam,
	  struct geost_data *pdata,
	  int object,
	  int monomorphic)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int rc=0, fr1, fr2, j;

  for (fr1=fr2=pdata->fr.end_relative; rc>=0 && fr1<pdata->fr.end_absolute; fr1++) {
    int coverdims=0, p=0;
    for (j=0; j<kdims; j++) {
      if (dvar_min_l(dv+j) >= FR_ORIGIN(fr1,j) && dvar_max_l(dv+j) <= FR_END(fr1,j))
	coverdims++;
      else
	p = j;
    }
    if (coverdims==kdims) {
      rc = -1;
    } else if (coverdims==kdims-1) {
      int rc1 = dvar_prune_interval_l(dv+p,FR_ORIGIN(fr1,p),FR_END(fr1,p));
      rc |= rc1;
      if (rc1>0 && monomorphic) {
	O_FLAGS(object) |= 0x3;	/* pruned, queued */
	pdata->pruneddim[p] = 1;
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

#else /* EXPLICIT_FBOXES */

static SP_BOOL
inside_forbidden_cont(Wam wam,
			    struct geost_data *pdata,
			    int *region)
{
  SP_integer *p = pdata->current_point;
  int fr = *region;
  int j, kdims = pdata->kdims;

  for (j=0; j<kdims; j++)
    if (p[j] < FR_ORIGIN(fr,j) || p[j] > FR_END(fr,j))
      return FALSE;
  return TRUE;
}

static SP_BOOL
inside_abs_cont(Wam wam,
		      struct geost_data *pdata,
		      int *region)
{
  SP_integer sid = pdata->current_sid;
  SP_integer *p = pdata->current_point;
  int fr = *region;
  int kdims = pdata->kdims;
  int sbox0 = get_first_sbox(sid);
  int j, s;

  for (s=sbox0; S_SID(s)==sid; s++) {
    for (j=0; j<kdims; j++)
      if (p[j] < FR_ORIGIN(fr,j) - S_TRANSLATION(s,j) - S_SIZE(s,j) || 
	  p[j] > FR_END(fr,j) - S_TRANSLATION(s,j))
	goto nexts;
    for (j=0; j<kdims; j++) {
      FR_ORIGIN(fr,j) -= S_TRANSLATION(s,j) + S_SIZE(s,j);
      FR_END(fr,j) -= S_TRANSLATION(s,j);
    }
    return TRUE;
  nexts: ;
  }
  return FALSE;
}

#endif

static SP_BOOL
useful_relative_fr(struct geost_data *pdata,
		   SP_integer fr_origin,
		   SP_integer fr_end,
		   int j)
{
  if (fr_origin - S_MAX_SIZE(j) > fr_end)
    return FALSE;
  if (fr_end - S_MIN_TRANS(j) < pdata->dvar_min[j])
    return FALSE;
  if (fr_origin - S_MAX_TRANS_SIZE(j) > pdata->dvar_max[j])
    return FALSE;
  return TRUE;
}

static SP_BOOL
useful_absolute_fr(struct geost_data *pdata,
		   int fr,
		   Dvar dv,
		   int j)
{
  int kdims = pdata->kdims;
  if (FR_ORIGIN(fr,j) < dvar_min_l(dv+j))
    FR_ORIGIN(fr,j) = dvar_min_l(dv+j);
  if (FR_END(fr,j) > dvar_max_l(dv+j))
    FR_END(fr,j) = dvar_max_l(dv+j);
  if (FR_ORIGIN(fr,j) > FR_END(fr,j))
    return FALSE;
  return TRUE;
}

static SP_BOOL
filter_sboxes(Wam wam,
	      struct geost_data *pdata,
	      int object,
	      int *nsboxesp)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  Dvar dvs = O_SID_DVAR(object);
  DVITER it;
  int sbox=0;
  int i, j;
  dviter_init(&it, dvs);
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    int s;
    for (s=sbox0; S_SID(s)==sid; s++)
      pdata->shape.useful[s] = 1;
    pdata->shape.min[sbox] = sbox0;
    pdata->shape.max[sbox] = s;
    pdata->shape.cur[sbox] = pdata->shape.min[sbox];
    sbox++;
  }
  *nsboxesp = sbox;
  /* For each sbox, check whether any intersection in which it takes
     part is doomed to produce a useelss region. */
  for (i=0; i<sbox; i++) {
    int s, i2, s2;
    for (s=(int)pdata->shape.min[i]; s<pdata->shape.max[i]; s++) {
      for (j=0; j<kdims; j++) {
	SP_integer fr_origin = dvar_max_l(dv+j)+S_TRANSLATION(s,j)+1;
	SP_integer fr_end = dvar_min_l(dv+j)+S_TRANSLATION(s,j)+S_SIZE(s,j)-1;
	if (!useful_relative_fr(pdata,fr_origin,fr_end,j)) {
	  pdata->shape.useful[s] = 0;
	  goto next_s;
	}
	for (i2=0; i2<sbox; i2++) {
	  if (i!=i2) {
	    for (s2=(int)pdata->shape.min[i2]; s2<pdata->shape.max[i2]; s2++) {
	      SP_integer fr_origin2 = dvar_max_l(dv+j)+S_TRANSLATION(s2,j)+1;
	      SP_integer fr_end2 = dvar_min_l(dv+j)+S_TRANSLATION(s2,j)+S_SIZE(s2,j)-1;
	      if (fr_origin < fr_origin2)
		fr_origin = fr_origin2;
	      if (fr_end > fr_end2)
		fr_end = fr_end2;
	      if (useful_relative_fr(pdata,fr_origin,fr_end,j)) {
		/* combination(s,j,i2) is useful; */
		goto next_i2;
	      }
	    }
	    pdata->shape.useful[s] = 0;
	    goto next_s;
	  }
	next_i2: ;
	}
      }
    next_s:
      if (!pdata->shape.useful[s] && pdata->shape.min[i]==s)
	pdata->shape.min[i] = s+1;
    }
    if (pdata->shape.min[i] >= pdata->shape.max[i]) {
      return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL
relative_fr_intersect(Wam wam,
			    struct geost_data *pdata,
			    int object, /* invariant: monomorphic */
			    int *region,
			    geost_cont *cont)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  Dvar dvs = O_SID_DVAR(object);
  SP_integer sid = dvar_min_l(dvs);
  int sbox0 = get_first_sbox(sid);
  int s, j;
  
  for (s=sbox0; S_SID(s)==sid; s++) {
    int fr = *region;
    for (j=0; j<kdims; j++) {
      FR_ORIGIN(fr,j) = dvar_max_l(dv+j)+S_TRANSLATION(s,j)+1;
      FR_END(fr,j) = dvar_min_l(dv+j)+S_TRANSLATION(s,j)+S_SIZE(s,j)-1;
	if (!useful_relative_fr(pdata,FR_ORIGIN(fr,j),FR_END(fr,j),j))
	  goto nostore1;
    }
    if ((*cont)(wam, pdata,region))
      return TRUE;
  nostore1: ;
  }
  return FALSE;
}

static SP_BOOL
relative_fr_enum(Wam wam,
		       struct geost_data *pdata,
		       int object, /* invariant: polymorphic */
		       int *region,
		       geost_cont *cont)

{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  Dvar dvs = O_SID_DVAR(object);
  SP_integer *p = pdata->shape.boundaries;
  int pivot = 1;
  int i, nsboxes;
  DVITER it;

  if (!filter_sboxes(wam, pdata,object,&nsboxes))
    goto out;

  for (i=0; i<2*kdims; i++) {
    int j = i>>1;
    SP_integer bound = (i & 1) ? CLPFD_MAXINT : -CLPFD_MAXINT;
    SP_integer *p0=p, *p1, *p2;

    dviter_init(&it, dvs);
    while (!dviter_empty(&it)) {
      SP_integer sid = dviter_next_value_l(&it);
      int sbox0 = get_first_sbox(sid);
      SP_integer lbound = (i & 1) ? -CLPFD_MAXINT : CLPFD_MAXINT;
      int s;
      
      for (s=sbox0; S_SID(s)==sid; s++)
	if (pdata->shape.useful[s]) {
	  if (i & 1) {
	    if (lbound < S_TRANSLATION(s,j) + S_SIZE(s,j))
	      lbound = S_TRANSLATION(s,j) + S_SIZE(s,j);
	  } else {
	    if (lbound > S_TRANSLATION(s,j))
	      lbound = S_TRANSLATION(s,j);
	  }
	}
      if (i & 1) {
	if (bound > lbound)
	  bound = lbound;
      } else {
	if (bound < lbound)
	  bound = lbound;
      }
    }

    dviter_init(&it, dvs);
    while (!dviter_empty(&it)) {
      SP_integer sid = dviter_next_value_l(&it);
      int sbox0 = get_first_sbox(sid);
      int s;
      
      for (s=sbox0; S_SID(s)==sid; s++)
	if (pdata->shape.useful[s]) {
	  if (i & 1) {
	    SP_integer point = S_TRANSLATION(s,j) + S_SIZE(s,j);
	    if (point <= bound)
	      *p++ = point;
	  } else {
	    SP_integer point = S_TRANSLATION(s,j);
	    if (point >= bound)
	      *p++ = point;
	  }
	}
    }

    fd_qsort_asc_long(wam, p0, (int)(p-p0));
    for (p1=p2=p0+1; p1<p; p1++)
      if (p1[0]!=p1[-1])
	*p2++ = p1[0];
    pdata->shape.min[i] = p0-pdata->shape.boundaries;
    pdata->shape.max[i] = p2-pdata->shape.boundaries;
    pdata->shape.cur[i] = p0-pdata->shape.boundaries;
  }
  for (i=2*kdims-1; i>=0 && pivot==1; i-=2)
    if (pdata->shape.min[i]+1 < pdata->shape.max[i])
      pivot = i;

 combine:
  /* If the cell indicated by pdata->shape.cur[] is contained by
     _some_ sbox for _all_ shapes, then store it as a fr. */
  dviter_init(&it, dvs);
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    int s;
    
    for (s=sbox0; S_SID(s)==sid; s++) {
      if (pdata->shape.useful[s]) {
	for (i=0; i<2*kdims; i++) {
	  int j = i>>1;
	  SP_integer point = pdata->shape.boundaries[pdata->shape.cur[i]];
	  if (i & 1) {
	    if (point > S_TRANSLATION(s,j) + S_SIZE(s,j))
	      goto next_sbox;
	  } else {
	    if (point < S_TRANSLATION(s,j))
	      goto next_sbox;
	  }
	}
	goto next_shape;
      }
    next_sbox: ;
    }
    /* skip some combinations that can't succeed */
/*     pdata->shape.cur[pivot] = pdata->shape.max[pivot]; */
    goto nostore;
  next_shape: ;
  }
  {
    int fr = *region;
    for (i=0; i<2*kdims; i++) {
      int j = i>>1;
      SP_integer point = pdata->shape.boundaries[pdata->shape.cur[i]];
      if (i & 1) {
        FR_END(fr,j) = dvar_min_l(dv+j)+point-1;
        if (!useful_relative_fr(pdata,FR_ORIGIN(fr,j),FR_END(fr,j),j))
          goto nostore;
      } else {
        FR_ORIGIN(fr,j) = dvar_max_l(dv+j)+point+1;
      }
    }
  }
  if ((*cont)(wam, pdata,region))
    return TRUE;
 nostore:
  for (i=2*kdims-1; i>=0; i--) {
    pdata->shape.cur[i]++;
    if (pdata->shape.cur[i]<pdata->shape.max[i])
      goto combine;
    pdata->shape.cur[i] = pdata->shape.min[i];
  }
 out:
  return FALSE;
}

static SP_BOOL
relative_fr(Wam wam,
		  struct geost_data *pdata,
		  int object,
		  int *region,
		  geost_cont *cont)
{
  int kdims = pdata->kdims;
  Dvar dvs = O_SID_DVAR(object);
  
  pdata->current_object = object;
  if (dvar_is_integer(dvs)) {
    return
      relative_fr_intersect(wam, pdata,object,region,cont);
  } else {
    return
      relative_fr_enum(wam, pdata,object,region,cont);
  }
}


static SP_BOOL
absolute_fr_kernel(Wam wam,
			 struct geost_data *pdata,
			 int object,
			 SP_integer sid,
			 int *region,
			 geost_cont *cont)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int sbox0 = get_first_sbox(sid);
  int i, j, s;

  /* forbidden regions wrt. the bounding box upper bounds */
  for (i=0; i<kdims; i++) {
    for (s=sbox0; S_SID(s)==sid; s++) {
      int fr = *region;
      for (j=0; j<kdims; j++) {
	if (i==j) {
	  FR_ORIGIN(fr,j) = dvar_max_l(pdata->upper_dvar+i) - S_TRANSLATION(s,j) - S_SIZE(s,j) + 1;
	  FR_END(fr,j) = dvar_max_l(dv+j);
	  if (!useful_absolute_fr(pdata,fr,dv,j))
	    goto next_shape1;
	} else {
	  FR_ORIGIN(fr,j) = dvar_min_l(dv+j);
	  FR_END(fr,j) = dvar_max_l(dv+j);
	}
      }
      if ((*cont)(wam, pdata,region))
	return TRUE;
    next_shape1: ;
    }
  }
  /* forbidden regions wrt. the bounding box lower bounds */
  for (i=0; i<kdims; i++) {
    for (s=sbox0; S_SID(s)==sid; s++) {
      int fr = *region;
      for (j=0; j<kdims; j++) {
	if (i==j) {
	  FR_ORIGIN(fr,j) = dvar_min_l(dv+j);
	  FR_END(fr,j) = dvar_min_l(pdata->lower_dvar+i) - S_TRANSLATION(s,j) - 1;
	  if (!useful_absolute_fr(pdata,fr,dv,j))
	    goto next_shape2;
	} else {
	  FR_ORIGIN(fr,j) = dvar_min_l(dv+j);
	  FR_END(fr,j) = dvar_max_l(dv+j);
	}
      }
      if ((*cont)(wam, pdata,region))
	return TRUE;
    next_shape2: ;
    }
  }
  /* forbidden regions wrt. other objects */
#if EXPLICIT_FBOXES
  for (i=pdata->fr.end_bb; i<pdata->fr.end_relative; i++) {
    if (FR_OID(i)!=O_OID(object)) {
      for (s=sbox0; S_SID(s)==sid; s++) {
	int fr = *region;
	for (j=0; j<kdims; j++) {
	  FR_ORIGIN(fr,j) =  FR_ORIGIN(i,j) - S_TRANSLATION(s,j) - S_SIZE(s,j);
	  FR_END(fr,j) = FR_END(i,j) - S_TRANSLATION(s,j);
	  if (!useful_absolute_fr(pdata,fr,dv,j))
	    goto next_shape;
	}
	*region = store_region(wam, pdata,fr,(int)FR_OBJ(i),0);
      next_shape: ;
      }
    }
  }
#else
  for (i=pdata->nobjects-1; i>=0; i--) {
    int interferer = TARGET(i);
    if (O_OID(interferer)!=O_OID(object) &&
	relative_fr(wam, pdata,interferer,/*object,sid,*/region,inside_abs_cont))
      return TRUE;
  }
#endif
  return FALSE;
}



#if PALLET_LOADING
static SP_BOOL
pl_absolute_fr(Wam wam,
		     struct geost_data *pdata,
		     int object,
		     SP_integer sid,
		     int *region,
		     geost_cont *cont)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int sbox = get_first_sbox(sid);
  int i, j;

  for (i=0; i<pdata->pl.nholes; i++) {
    int fr = *region;
    for (j=0; j<2; j++) {
      SP_integer x = pdata->pl.holes[2*i+j] + pdata->pl.bborig[j];
      
      FR_ORIGIN(fr,j) = x - S_SIZE(sbox,j) + 1;
      FR_END(fr,j) = x;
      if (!useful_absolute_fr(pdata,fr,dv,j))
	goto nexti;
    }
    if ((*cont)(wam, pdata,region))
      return TRUE;
  nexti: ;
  }
  return FALSE;
}
#endif

static SP_BOOL
absolute_fr(Wam wam,
	    struct geost_data *pdata,
	    int object,
	    SP_integer sid,
	    int *region,
	    geost_cont *cont)
{
  pdata->current_object = object;
  pdata->current_sid = sid;
  if (absolute_fr_kernel(wam, pdata,object,sid,region,cont))
    return TRUE;
#if PALLET_LOADING
  if ((pdata->opt & OPT_PALLET_LOADING) &&
      pdata->pl.nholes==pdata->pl.pslack &&
      pl_absolute_fr(wam, pdata,object,sid,region,cont))
    return TRUE;
#endif
  return FALSE;
}

static SP_BOOL
point_in_some_fr(Wam wam,
		 struct geost_data *pdata,
		 int object,
		 SP_integer sid,
		 SP_integer *point,
		 int *region,
		 geost_cont *cont)
{
#if EXPLICIT_FBOXES
  int kdims = pdata->kdims;
  int i, j, k;
  int nbfr = pdata->fr.end_absolute - pdata->fr.end_relative;
  
  for (i=0, k=pdata->fr.round_robin; i<nbfr; i++, k++) {
    if (k==pdata->fr.end_absolute)
      k = pdata->fr.end_relative;
    for (j=0; j<kdims; j++) {
      if (point[j]<FR_ORIGIN(k,j) || point[j]>FR_END(k,j))
	goto next;
    }
    *region = k;
    pdata->fr.round_robin = k;	/* want k NOT k+1 if called by valid_witness */
    return TRUE;
  next: ;
  }
#else
  int kdims = pdata->kdims;
  int j;

  for (j=0; j<kdims; j++)
    pdata->current_point[j] = point[j];
  *region = pdata->fr.end_bb;
  if (absolute_fr(wam, pdata,object,sid,region,cont))
    return TRUE;
#endif
  (void)cont;
  if ((pdata->opt & OPT_RULES) &&
      point_breaks_rule(wam, pdata,object,sid,point,region))
    return TRUE;
  return FALSE;
}

#if PARCONFLICT_METHOD
struct value_index {
  SP_integer value;
  SP_integer index;
};

static int
cmp_value_index(Wam wam,
		struct value_index *t1,
		struct value_index *t2)
{
  (void)wam;
  return CMP(t1->value,t2->value);
}

#define QType struct value_index
#define QCmp  cmp_value_index
#define QSort qsort_value_index
#include "qsort.ic"

static void
get_nb_slices_parallel_conflict(int d,
				int i,
				int n,
				int *j,
				SP_integer *sum,
				SP_integer *available,
				struct value_index *sort,
				SP_integer *ipos,
				struct value_index *vol,
				SP_integer *nb_slices)
{
  SP_integer divide;
  int ii;
  
  if (sum[d] > AREF2(available,d,i,n)) {
    SP_integer maxl;
    SP_integer gap = sum[d] - AREF2(available,d,i,n);
    int k = j[d] - 1;
    while (k>=0 && AREF2(sort,d,k,n).value==0)
      k--;
    if (k>=0) {
      maxl = AREF2(sort,d,k,n).value;
      /* invariant maxl>0 */
    } else {
      maxl = 0;
      /* invariant j[d]==n || sort[d,j[d]] > available[d,i] */
    }
    divide = gap+1 > maxl ? gap+1 : maxl;
  } else {
    divide = 0;
  }
  for (ii=i; ii<n; ii++) {
    SP_integer p = AREF2(ipos,d,vol[ii].index,n);
    /* invariant sort[d,p]>0 */
    AREF2(nb_slices,d,AREF2(sort,d,p,n).index,n) =
      (p<j[d] || divide==0) ? 1 : AREF2(sort,d,p,n).value/divide;
  }
}

static int
too_many_slices_parallel_conflict(int k,
				  int n,
				  int i,
				  int c, /* max that can be placed */
				  struct value_index *sort,
				  SP_integer *ipos,
				  struct value_index *vol,
				  SP_integer *nb_slices)
{
  int to_place = 0;
  int ii;
  
  for (ii=i; ii<n && to_place<=c; ii++) {
    int nslices = 1;
    int d;
    for (d=0; d<k; d++) {
      SP_integer p = AREF2(ipos,d,vol[ii].index,n);
      /* invariant sort[d,p]>0 */
      nslices *= (int)AREF2(nb_slices,d,AREF2(sort,d,p,n).index,n);
    }
    /* invariant nslices>0 */
    to_place += nslices;
  }
  return to_place>c;
}

static void
init_parallel_conflict(Wam wam,
			     int k,
			     int n,
			     SP_integer *sizes,
			     SP_integer *earliest,
			     SP_integer *latest,
			     int sizrowlen,
			     struct value_index *sort,
			     SP_integer *ipos,
			     struct value_index *vol,
			     SP_integer *available,
			     SP_integer *sum,
			     int *conflict,
			     int *j)
{
  int d, i;
  for (i=0; i<n; i++) {
    vol[i].value = 1;
    vol[i].index = i;
  }
  for (d=0; d<k; d++) {
    for (i=0; i<n; i++) {
      SP_integer sizentry = AREF2(sizes,d,i,sizrowlen);
      AREF2(sort,d,i,n).value = sizentry;
      AREF2(sort,d,i,n).index = i;
      vol[i].value *= sizentry;
    }
    qsort_value_index(wam, &AREF2(sort,d,0,n),n);
    for (i=0; i<n; i++)
      AREF2(ipos,d,AREF2(sort,d,i,n).index,n) = i;
  }
  qsort_value_index(wam, vol,n);
  for (d=0; d<k; d++) {
    SP_integer min_earliest = AREF2(earliest,d,vol[n-1].index,sizrowlen);
    SP_integer max_latest = AREF2(latest,d,vol[n-1].index,sizrowlen);
    AREF2(available,d,n-1,n) = max_latest - min_earliest + 1;
    for (i=n-2; i>=0; i--) {
      SP_integer early = AREF2(earliest,d,vol[i].index,sizrowlen);
      SP_integer late = AREF2(latest,d,vol[i].index,sizrowlen);
      min_earliest = early < min_earliest ? early : min_earliest;
      max_latest = late > max_latest ? late : max_latest;
      AREF2(available,d,i,n) = max_latest - min_earliest + 1;
    }
  }
  for (d=0; d<k; d++) {
    sum[d] = 0; conflict[d] = 0; j[d] = 0;
  }
}

static SP_integer
init_parallel_conflict_1d(int d,
			  int n,
			  SP_integer *sizes,
			  SP_integer *earliest,
			  SP_integer *latest,
			  struct value_index *sort,
			  int sizrowlen,
			  SP_integer *width,
			  SP_integer *height,
			  SP_integer *sumh_cum,
			  SP_integer *available)
{
  SP_integer *e = available+n;
  SP_integer *l = available+2*n;
  int i, c=(d^1);
  SP_integer mine, maxl;

  for (i=0; i<n; i++) {
    int isort = (int)AREF2(sort,d,i,n).index;
    width[i] = AREF2(sort,d,i,n).value;
    height[i] = AREF2(sizes,c,isort,sizrowlen);
    e[i] = AREF2(earliest,d,isort,sizrowlen);
    l[i] = AREF2(latest,d,isort,sizrowlen);
  }
  sumh_cum[n-1] = height[n-1];
  mine = e[n-1];
  maxl = l[n-1];
  available[n-1] = maxl-mine+1;
  for (i=n-2; i>=0; i--) {
    sumh_cum[i] = height[i]+sumh_cum[i+1];
    mine = e[i] < mine ? e[i] : mine;
    maxl = l[i] > maxl ? l[i] : maxl;
    available[i] = maxl-mine+1;
  }
  mine = AREF2(earliest,c,0,sizrowlen);
  maxl = AREF2(latest,c,0,sizrowlen);
  for (i=1; i<n; i++) {
    SP_integer ei = AREF2(earliest,c,i,sizrowlen);
    SP_integer li = AREF2(latest,c,i,sizrowlen);
    mine = mine < ei ? mine : ei;
    maxl = maxl > li ? maxl : li;
  }
  return maxl-mine+1;
}


static int
check_parallel_conflict_1d(int n,
			   SP_integer *sizes,
			   SP_integer *earliest,
			   SP_integer *latest,
			   struct value_index *sort,
			   int sizrowlen)
{
  SP_integer *width = (SP_integer *)(sort+2*n);
  SP_integer *height = width+n;
  SP_integer *sumh_cum = height+n;
  SP_integer *available = sumh_cum+n;
  int d;

  for (d=0; d<2; d++) {
    int conflict=0, j=0, last, i;
    SP_integer sumw=0, havail, sumh, limit;
    
    havail = init_parallel_conflict_1d(d,n,sizes,earliest,latest,sort,sizrowlen,
				       width,height,sumh_cum,available);
    last = n-1;
    for (i=0; i<n; i++) {
      while (sumw <= available[i] && j<n) {
	sumw += width[j++];
	conflict++;
      }
      if (conflict>1 && sumw>available[i]) {
	while (last>=j && width[i]+width[last]>available[i])
	  last--;
	if (last+1<n && width[i]+width[last+1]>available[i]) {
	  sumh = sumh_cum[i]-sumh_cum[last+1];
	  limit = havail-sumh_cum[last+1];
	} else {
	  sumh = sumh_cum[i];
	  limit = havail;
	}
	if (sumh > (conflict-1)*limit)
	  return TRUE;
	else if (conflict>2 &&
		 sumw-width[j-2]>available[i] &&
		 sumh-height[j-2] > (conflict-2)*limit)
	  return TRUE; 
      }
      sumw -= width[i];
      conflict--;
    }
  }
  return FALSE;
}

static int
parallel_conflict(Wam wam,
		  int k,
		  int n,
		  int heavy_check,
		  SP_integer *sizes,
		  SP_integer *earliest,
		  SP_integer *latest,
		  int sizrowlen,
		  struct size_mem *size_mem)
{
  int i, rc=0;

  if (n>0) {
    SP_integer sum[256];
    int  conflict[256];
    int  j[256];
    size_t nlongs = 3*k*n + (k+1)*n*sizeof(struct value_index)/sizeof(SP_integer) + 6*n;
    SP_integer *ipos = PermAlloc(nlongs,SP_integer,size_mem);
    SP_integer *available = ipos + k*n;
    SP_integer *nb_slices = ipos + 2*k*n;
    struct value_index *vol  = (struct value_index *)(ipos + 3*k*n);
    struct value_index *sort = vol+n; /* sort must be last! check_parallel_conflict_1d uses memory after it */
    
    init_parallel_conflict(wam, k,n,sizes,earliest,latest,sizrowlen,
			   sort,ipos,vol,available,sum,conflict,j);
    if (k==2 && check_parallel_conflict_1d(n,sizes,earliest,latest,sort,sizrowlen))
      rc = 1;
    else
      for (i=0; i<n && !rc; i++) {
	int d, c=1;
	for (d=0; d<k; d++) {
	  while (sum[d]<=AREF2(available,d,i,n) && j[d]<n) {
	    if (AREF2(sort,d,j[d],n).value>0) {
	      sum[d] += AREF2(sort,d,j[d],n).value;
	      conflict[d]++;
	    }
	    j[d]++;
	  }
	  if (heavy_check)
	    get_nb_slices_parallel_conflict(d,i,n,j,sum,available,sort,ipos,vol,nb_slices);
	  c *= conflict[d] - (sum[d] > AREF2(available,d,i,n));
	  {
            int p = (int)AREF2(ipos,d,vol[i].index,n);
            if (p<j[d]) {
              /* invariant sort[d,p]>0 */
              sum[d] -= AREF2(sort,d,p,n).value;
              conflict[d]--;
            }
            AREF2(sort,d,p,n).value = 0;
          }
	}
	if (c<n-i ||
	    (heavy_check &&
	     too_many_slices_parallel_conflict(k,n,i,c,sort,ipos,vol,nb_slices)))
	  rc = 1;
      }
    PermFree(ipos,size_mem);
  }
  return rc;
}

static int
check_wrt_parallel_conflict(Wam wam,
			    struct geost_data *pdata,
			    int posted)
{
  int kdims = pdata->kdims;
  int i, d, res;
  int ix = 0;
  int sizrowlen = pdata->nobjects*pdata->max_nsboxes;
  SP_integer *sizes = PermAlloc(3*kdims*sizrowlen,SP_integer,&pdata->size_mem[0]);
  SP_integer *earliest = sizes + kdims*sizrowlen;
  SP_integer *latest = sizes + 2*kdims*sizrowlen;

  for (i=0; i<pdata->nobjects; i++) {
    int object = (int)TARGET(i);
    Dvar dvs = O_SID_DVAR(object);
    SP_integer sid = dvar_min_l(dvs);
    int sbox0 = get_first_sbox(sid);
    if (decomposable(pdata,object) && 
	fr_intersects_object(wam, pdata,/*bounding box*/1,object)) {
      Dvar dv = O_ORIG_DVAR(object,0);
      int s;
      for (s=sbox0; S_SID(s)==sid; s++) {
	for (d=0; d<kdims; d++) {
	  SP_integer size = S_SIZE(s,d);
	  SP_integer orig = dvar_min_l(dv+d) + S_TRANSLATION(s,d);
	  SP_integer end  = dvar_max_l(dv+d) + S_TRANSLATION(s,d) + size - 1;
	  if (orig < FR_ORIGIN(/*bounding box*/1,d))
	    orig = FR_ORIGIN(/*bounding box*/1,d);
	  if (end > FR_END(/*bounding box*/1,d))
	    end = FR_END(/*bounding box*/1,d);
	  if (orig>end)
	    goto nexts;
	  if (size > end-orig+1)
	    size = end-orig+1;
	  AREF2(sizes,d,ix,sizrowlen) = size;
	  AREF2(earliest,d,ix,sizrowlen) = orig;
	  AREF2(latest,d,ix,sizrowlen) = end;
	}
	ix++;
      nexts: ;
      }
    }
  }
  res = !parallel_conflict(wam,kdims,ix,posted,sizes,earliest,latest,sizrowlen,
			   &pdata->size_mem[1]);
  PermFree(sizes,&pdata->size_mem[0]);
  return res;
}
#endif

static void
bb_init(struct geost_data *pdata,
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
bb_add_object(Wam wam,
		    struct geost_data *pdata,
		    int bb,
		    int object)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  DVITER it;

  dviter_init(&it, O_SID_DVAR(object));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    int j, s;

    for (s=sbox0; S_SID(s)==sid; s++) {
      for (j=0; j<kdims; j++) {
	SP_integer orig = dvar_min_l(dv+j) + S_TRANSLATION(s,j);
	SP_integer end  = dvar_max_l(dv+j) + S_TRANSLATION(s,j) + S_SIZE(s,j) - 1;
	if (orig<dvar_min_l(pdata->lower_dvar+j))
	  orig = dvar_min_l(pdata->lower_dvar+j);
	if (end>dvar_max_l(pdata->upper_dvar+j)-1)
	  end = dvar_max_l(pdata->upper_dvar+j)-1;
	if (FR_ORIGIN(bb,j) > orig)
	  FR_ORIGIN(bb,j) = orig;
	if (FR_END(bb,j) < end)
	  FR_END(bb,j) = end;
      }
    }
  }
}

#if PALLET_LOADING

static int
same_rotated_unshifted_single_box(Wam wam,
					struct geost_data *pdata,
					int object,
					int sbox)
{
  int kdims = pdata->kdims;
  Dvar dvs = O_SID_DVAR(object);
  DVITER it;
  int j;

  dviter_init(&it, dvs);
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int s = get_first_sbox(sid);
    
    if (S_SID(s+1)==sid)
      return FALSE;
    for (j=0; j<kdims; j++)
      if (S_TRANSLATION(s,j) || S_SIZE_MULTISET(s,j)!=S_SIZE_MULTISET(sbox,j))
	return FALSE;
  }
  return TRUE;
}

/* pseudocode name: RecognizePalletLoading */
static SP_BOOL
pl_recognize(Wam wam,
		   struct geost_data *pdata,
		   SP_integer *available,
		   SP_integer *item_vol,
		   SP_integer *placement_vol)
{
  int kdims = pdata->kdims;
  SP_integer sid = dvar_min_l(O_SID_DVAR(0));
  int o, s, i, j, k;
  SP_integer maxindex;
  SP_integer *islin;

  s = get_first_sbox(sid);
  pdata->pl.sbox = s;
  if (S_SID(s+1)==sid)
    return FALSE;
  for (j=0; j<kdims; j++)
    if (S_TRANSLATION(s,j))
      return FALSE;
  for (o=1; o<pdata->nobjects; o++)
    if (!same_rotated_unshifted_single_box(wam, pdata,o,s))
      return FALSE;
  (*item_vol) = 1;
  (*placement_vol) = 1;
  for (j=0; j<kdims; j++) {
    pdata->pl.bborig[j] = FR_ORIGIN(/*bounding box*/1,j);
    pdata->pl.bbend[j] = FR_END(/*bounding box*/1,j);
    available[j] = FR_END(/*bounding box*/1,j) - FR_ORIGIN(/*bounding box*/1,j) + 1;
    (*item_vol) *= S_SIZE(s,j);
    (*placement_vol) *= available[j];
  }
  fd_qsort_asc_long(wam, available,kdims);
  
  maxindex = available[kdims-1];
  islin = Malloc(maxindex+1, SP_integer);

  islin[0] = TRUE;
  for (i=1; i<=maxindex; i++)
    islin[i] = FALSE;

  for (i=0; i<kdims; i++) {
    SP_integer a = S_SIZE_MULTISET(s,i);

    for (j=0; j+a<=maxindex; j++)
      if (islin[j])
	for (k=(int)(j+a); k<=maxindex && !islin[k]; k+=(int)a)
	  islin[k] = TRUE;
  }
  pdata->pl.islin = islin;
  return TRUE;
}

/* pseudocode name: CheckDeBruijnTheorem */
static SP_BOOL
pl_check_debruijn(struct geost_data *pdata,
		  SP_integer item_vol,
		  int  nb_item,
		  SP_integer placement_vol,
		  SP_integer *available)
{
  int kdims = pdata->kdims;
  int s = pdata->pl.sbox;
  int i, j;
  
  if (item_vol*nb_item==placement_vol) {
    for (j=0; j<kdims; j++) {
      SP_BOOL divide = FALSE;

      for (i=0; !divide && i<kdims; i++)
	divide = (available[i] % S_SIZE_MULTISET(s,j) == 0);
      if (!divide)
	return TRUE;
    }
  }
  return FALSE;
}

/* pseudocode name: CheckPalletLoadingBounds, called when kdims==2 */
static SP_BOOL
pl_check_bounds(struct geost_data *pdata,
		SP_integer item_vol,
		int  nb_item,
		SP_integer placement_vol,
		SP_integer *available)
{
  int kdims = pdata->kdims;

  if (item_vol*nb_item<placement_vol) {
    int s = pdata->pl.sbox;
    SP_integer x = available[1];
    SP_integer y = available[0];
    SP_integer a = S_SIZE_MULTISET(s,1);
    SP_integer b = S_SIZE_MULTISET(s,0);
    SP_integer xa = x%a;
    SP_integer xb = x%b;
    SP_integer ya = y%a;
    SP_integer yb = y%b;
    SP_integer area_ratio = (x*y)/(a*b);
    SP_integer max_prod = (x/b)*(y/b);
    SP_integer na = (xa+ya<=a) ? (x*y - xa*ya)/a : (x*y - (a-xa)*(a-ya))/a;
    SP_integer nb = (xb+yb<=b) ? (x*y - xb*yb)/b : (x*y - (b-xb)*(b-yb))/b;
    if (nb_item>area_ratio || nb_item>max_prod || nb_item>na/b || nb_item>nb/a)
      return TRUE;
  }
  return FALSE;
}

/* pseudocode name: PalletReduceHoles1 */
/* precondition: pslack>0 */
static int
pl_reduce_holes1(SP_integer x,
		 SP_integer y,
		 SP_integer b,
		 int  pslack,
		 int  nh,
		 SP_integer *holes)
{
  if (nh>=pslack) {
    int h, m=0;
    
    for (h=0; h<nh; h++) {
      SP_integer col = holes[2*h];
      SP_integer row = holes[2*h+1];
      SP_integer f = col < x-col-1 ? col : x-col-1;
      SP_integer g = row < y-row-1 ? row : y-row-1;
      if (!((pslack<=f && f<b) ||
	    (pslack<=g && g<b) ||
	    (pslack<(f+1)*(g+1) && f<b && g<b) ||
	    (pslack<b && f*g==0))) {
	holes[2*m] = col;
	holes[2*m+1] = row;
	m++;
      }
    }
    return m;
  } else {
    return nh;
  }
}

typedef struct {
  SP_integer x, y; 
} lpair;

/* for qsorting by ascending SP_integer */
static int cmp_asc_pair(Wam wam, lpair *l1, lpair *l2)
{
  (void)wam;
  if (l1->x<l2->x)
    return -1;
  else if (l1->x>l2->x)
    return 1;
  else if (l1->y<l2->y)
    return -1;
  else if (l1->y>l2->y)
    return 1;
  else
    return 0;
}

#define QType lpair
#define QCmp  cmp_asc_pair
#define QSort qsort_asc_pair
#include "qsort.ic"

/* pseudocode name: PalletReduceHoles2a */
static int
pl_reduce_holes2a(int  nh,
		 SP_integer p,
		 SP_integer q,
		 SP_integer *holes,
		 SP_integer *islin)
{
  int n, m=0;
  SP_integer skip=-1;

  for (n=0; n<nh; n++) {
    SP_integer col = holes[2*n];
    SP_integer row = holes[2*n+1];
    if (col==skip || islin[p*row+q]) {
      skip = col;
      holes[2*m] = col;
      holes[2*m+1] = row;
      m++;
    }
  }
  return m;
}

/* pseudocode name: PalletReduceHoles2 */
static int
pl_reduce_holes2(Wam wam,
		       SP_integer x,
		       SP_integer y,
		       int  pslack,
		       int  nh,
		       SP_integer *holes,
		       SP_integer *islin)
{
  int m;
  
  if (nh>=pslack) {
    nh = pl_reduce_holes2a(nh,1,0,holes,islin);
    
    for (m=0; m<nh/2; m++) {	/* reverse (col,row) */
      SP_integer x1 = holes[2*m];
      SP_integer y1 = holes[2*m+1];
      holes[2*m]= holes[2*(nh-m-1)];
      holes[2*m+1]= holes[2*(nh-m-1)+1];
      holes[2*(nh-m-1)] = x1;
      holes[2*(nh-m-1)+1] = y1;
    }
    nh = pl_reduce_holes2a(nh,-1,y-1,holes,islin);
    
    for (m=0; m<nh; m++) {	/* (col,-row) to (-row,col) */
      SP_integer col = holes[2*m];
      holes[2*m] = holes[2*m+1];
      holes[2*m+1] = col;
    }
    qsort_asc_pair(wam, (lpair *)holes,nh); /* sort (-row,col) */
    nh = pl_reduce_holes2a(nh,1,0,holes,islin);
    
    for (m=0; m<nh/2; m++) {	/* reverse (-row,col) */
      SP_integer x1 = holes[2*m];
      SP_integer y1 = holes[2*m+1];
      holes[2*m]= holes[2*(nh-m-1)];
      holes[2*m+1]= holes[2*(nh-m-1)+1];
      holes[2*(nh-m-1)] = x1;
      holes[2*(nh-m-1)+1] = y1;
    }
    nh = pl_reduce_holes2a(nh,-1,x-1,holes,islin);
    
    for (m=0; m<nh; m++) {	/* (-row,-col) to (col,row) */
      SP_integer row = holes[2*m];
      holes[2*m] = holes[2*m+1];
      holes[2*m+1] = row;
    }
    qsort_asc_pair(wam, (lpair *)holes,nh);
  }
  return nh;
}

/* precond: kdims==2 */
static int
pl_color_latin_bar(SP_integer x,
		   SP_integer y,
		   SP_integer a,
		   int  nh,
		   SP_integer *holes)
{
  SP_integer m = x % a;		/* #columns in residual square */
  SP_integer n = y % a;		/* #rows in residual square */
  SP_BOOL keepa, keepd;
  int g, h;
  
  if ((n==1 || m==1) && n+m==a) {
    keepa = TRUE;  keepd = TRUE;
  } else if (a>n*m) {
    keepa = TRUE;  keepd = FALSE;
  } else if (a>(a-n)*(a-m)) {
    keepa = FALSE; keepd = TRUE;
  } else {
    keepa = FALSE; keepd = FALSE;
  }      
  
  for (g=0, h=0; h<nh; h++) {
    SP_integer col = holes[2*h];
    SP_integer row = holes[2*h+1];
    
    if ((col%a<m && row%a<n && keepa) ||
	(col%a>=m && row%a>=n && keepd)) {
      holes[2*g] = col;
      holes[2*g+1] = row;
      g++;
    }
  }
  return g;
}

static int
pl_color_latin_tile(SP_integer x,
		    SP_integer y,
		    SP_integer a,
		    int  nh,
		    SP_integer *holes)
{
    SP_integer m = x%a;
    SP_integer n = y%a;
    int g, h;
    
    for (g=0, h=0; h<nh; h++) {
      SP_integer col = holes[2*h];
      SP_integer row = holes[2*h+1];
      
      if ((col%a<m && row%a<n) ||
	  (col%a>=m && row%a>=n && m+n==a)) {
	holes[2*g] = col;
	holes[2*g+1] = row;
	g++;
      }
    }
    return g;
}

/* pseudocode name: PalletLoadingColorLatin */
static int
pl_color_latin(SP_integer x,
	       SP_integer y,
	       SP_integer a,
	       SP_integer b,
	       SP_integer pslack,
	       int  nh,
	       SP_integer *holes)
{
  SP_integer a1, b1;

  for (a1=1; a1<=a; a1++)
    if (a%a1==0)
      for (b1=1; b1<=b; b1++)
	if (b%b1==0) {
	  SP_integer a2 = a1 > b1 ? a1 : b1;
	  SP_integer b2 = a1 < b1 ? a1 : b1;
	  SP_integer m = x%a2;
	  SP_integer n = y%a2;

	  if (b2==1 && pslack<a2 && (x*y)%a2>0)
	    nh = pl_color_latin_bar(x,y,a2,nh,holes);
	  else if (pslack==m*n && m+n<=a2) {
	    nh = pl_color_latin_tile(x,y,a2,nh,holes);
	  }
	}
  return nh;
}

/* pseudocode name: PalletLoadingColor, where kdims=2 */
static SP_BOOL
pl_color(Wam wam,
	       int  nb_item,
	       SP_integer x,
	       SP_integer y,
	       SP_integer a,
	       SP_integer b,
	       SP_integer *islin,
	       struct geost_data *pdata)

{
  int nh, j;
  SP_integer *holes, col, row, pslack;
  
  if (a*b*nb_item<x*y) {

    pslack = x*y - nb_item*a*b;
    if (x*y >= 1<<20 ||		/* ad-hoc space limit */
        pslack==0 || pslack>=a*b)
      return FALSE;
    nh = 0;
    holes = Malloc(2*x*y,SP_integer);
    for (col=0; col<x; col++) {
      for (row=0; row<y; row++) {
	holes[2*nh] = col;
	holes[2*nh+1] = row;
	nh++;
      }
    }
    nh = pl_color_latin(x,y,a,b,pslack,nh,holes);
    if (b>1) {
      nh = pl_reduce_holes1(x,y,b,(int)pslack,nh,holes);
      nh = pl_reduce_holes2(wam, x,y,(int)pslack,nh,holes,islin);
    }
    if (nh<pslack) {
      Free(holes);
      return TRUE;
    } else if (pdata) {
      pdata->pl.pslack = pslack;
      pdata->pl.nholes = nh;
      pdata->pl.holes = Malloc(2*nh,SP_integer);
      for (j=0; j<nh; j++) {
	pdata->pl.holes[2*j] = holes[2*j];
	pdata->pl.holes[2*j+1] = holes[2*j+1];
      }
      Free(holes);
      return FALSE;
    } else {
      Free(holes);
      return FALSE;
    }
  }
  return FALSE;
}

static SP_integer
pl_loss(SP_integer x,
	SP_integer *islin)
{
  SP_integer y=x;

  while (!islin[y]) y--;

  return x-y;
}


/* pseudocode name: PalletIsFeasible, kdims==2 */
static SP_BOOL
pl_is_feasible(Wam wam,
		     int nitem,
		     SP_integer x,
		     SP_integer y,
		     SP_integer a,
		     SP_integer b,
		     SP_integer *islin)
{
  /* [MC] first of all, clip x and y down to linear combinations of a and b */

  x -= pl_loss(x,islin);
  y -= pl_loss(y,islin);

  {
    SP_integer slack = x*y - nitem*a*b;
    SP_integer xa = x % a;
    SP_integer xb = x % b;
    SP_integer ya = y % a;
    SP_integer yb = y % b;

    if (slack<0) {
      return FALSE;
    } else if (slack==0 && ((xa && ya) || (xb && yb))) { /* NO SLACK: De Bruijn's theorem */
      return FALSE;
    } else {
      SP_integer area_ratio = (x*y)/(a*b);
      SP_integer max_prod = (x/b)*(y/b);
      SP_integer na, nb;

      if (b>1) {
	SP_integer maxh = (x/a)*(y/b); /* max #horizontal pieces */
	SP_integer maxv = (x/b)*(y/a); /* max #vertical   pieces */
	SP_integer minh = nitem-maxv;	/* min #horizontal pieces */
	SP_integer minv = nitem-maxh;	/* min #vertical   pieces */
	SP_integer hrloss=0, vrloss=0, hcloss=0, vcloss=0;
	
	if (maxh<minh)
	  return FALSE;
	if (minh>0) {
	  hrloss = ((minh-1)/(x/a) + 1)*b*pl_loss(x-a,islin);
	  hcloss = ((minh-1)/(y/b) + 1)*a*pl_loss(y-b,islin);
	}
	if (minv>0) {
	  vrloss = ((minv-1)/(x/b) + 1)*a*pl_loss(x-b,islin);
	  vcloss = ((minv-1)/(y/a) + 1)*b*pl_loss(y-a,islin);
	}
	if (slack <
	    (hrloss > vrloss ? hrloss : vrloss) + (hcloss > vcloss ? hcloss : vcloss))
	  return FALSE;
      }
      /* check area ratio, max product, and Barnes bounds */
      if (xa+ya <= a)
	na = (x*y - xa*ya)/a;
      else
	na = (x*y - (a-xa)*(a-ya))/a;
      if (xb+yb <= b)
	nb = (x*y - xb*yb)/b;
      else
	nb = (x*y - (b-xb)*(b-yb))/b;
      if (nitem > area_ratio ||
	  nitem > max_prod ||
	  nitem > na/b ||
	  nitem > nb/a)
	return FALSE;
      if (pl_color(wam, nitem,x,y,a,b,islin,NULL))
	return FALSE;
    }
  }
  return TRUE;
}

/* pseudocode name: PalletConvexHullNotFeasible, kdims==2 */
static SP_BOOL
pl_convex_hull_feasible(Wam wam, struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int s = pdata->pl.sbox;
  SP_integer *islin = pdata->pl.islin;
  SP_integer a = S_SIZE_MULTISET(s,1);
  SP_integer b = S_SIZE_MULTISET(s,0);
  SP_integer size[2], x, y;
  int  i, dim, overlap = 0, nbitem;

  size[0] = FR_END(/*bounding box*/1,0) - FR_ORIGIN(/*bounding box*/1,0) + 1;
  size[1] = FR_END(/*bounding box*/1,1) - FR_ORIGIN(/*bounding box*/1,1) + 1;
  x = size[0] > size[1] ? size[0] : size[1];
  y = size[0] < size[1] ? size[0] : size[1];
  for (dim=0; dim<2; dim++) {
    struct profile *prof = fd_empty_profile(), *prof0;
    SP_integer cur, gap, delta, loss = 0;

    nbitem = 0;
    for (i=0; i<pdata->nobjects; i++) {
      int io = (int)TARGET(i);
      if (i<pdata->ntargets && !object_is_ground(pdata,io)) {
	nbitem++;
      } else {
	Dvar dvs = O_SID_DVAR(io);
	Dvar dv = O_ORIG_DVAR(io,0);
	SP_integer sid = dvar_min_l(dvs);
	int d, s1 = get_first_sbox(sid);
	SP_integer dmin[2], dmax[2];

	for (d=0; d<2; d++) {
	  dmin[d] = dvar_min_l(dv+d);
	  dmax[d] = dmin[d] + S_SIZE(s1,d)-1;
	  dmin[d] = dmin[d] > FR_ORIGIN(1,d) ? dmin[d] : FR_ORIGIN(1,d);
	  dmax[d] = dmax[d] < FR_END(1,d) ? dmax[d] : FR_END(1,d);
	}
	if (dmin[0]<=dmax[0] && dmin[1]<=dmax[1]) {
	  SP_integer beg = dmin[dim];
	  SP_integer end = dmax[dim]+1;
	  SP_integer h = dmax[dim^1] - dmin[dim^1] + 1;
	  
	  if (h*(end-beg) == a*b)
	    nbitem++;
	  else
	    overlap = TRUE;
	  if (b>1)
	    prof = fd_profile_update(wam,prof,beg,end,h);
	}
      }
    }
    if (b>1) {
      prof0 = prof;
      cur = FR_ORIGIN(/*bounding box*/1,dim);
      while (cur < FR_END(/*bounding box*/1,dim)+1) {
	if (prof==NULL) {
	  gap = size[dim^1];
	  delta = FR_END(/*bounding box*/1,dim)+1 - cur;
	  while (!islin[gap])
	    gap--;
	  loss += delta*(size[dim^1] - gap);
	} else if (cur < prof->begin) {
	  gap = size[dim^1];
	  delta = prof->begin - cur;
	  while (!islin[gap])
	    gap--;
	  loss += delta*(size[dim^1] - gap);
	} else {
	  gap = size[dim^1] - prof->erg;
	  delta = prof->end - cur;
	  while (!islin[gap])
	    gap--;
	  loss += delta*(size[dim^1] - prof->erg - gap);
	  prof = prof->next;
	}
	cur += delta;
      }
      fd_profile_dispose(wam, prof0);
      if (loss > x*y - nbitem*a*b)
	return FALSE;
    }
  }
  if (!overlap && !pl_is_feasible(wam, nbitem,x,y,a,b,islin))
    return FALSE;
  return TRUE;
}


/* pseudocode name: PalletLoadingWake, kdims==2 */
static SP_BOOL
pl_wake(Wam wam, struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int inf = 0;
  int sup = pdata->pl.nholes-1;

  if (sup>=0) {
    int heldx = (int)pdata->pl.holes[2*sup];
    int heldy = (int)pdata->pl.holes[2*sup+1];
    int currentx = (int)pdata->pl.holes[0];
    int currenty = (int)pdata->pl.holes[1];
    
    /* count number of active holes by partitioning the holes array */
    while (inf<=sup) {
      SP_BOOL covered = FALSE;
      int i;
    
      for (i=0; i<pdata->ntargets && !covered; i++) {
	int io = (int)TARGET(i);
	if (object_is_ground(pdata,io)) {
	  Dvar dvs = O_SID_DVAR(io);
	  Dvar dv = O_ORIG_DVAR(io,0);
	  SP_integer sid = dvar_min_l(dvs);
	  int  s = get_first_sbox(sid);
	  SP_integer x = dvar_min_l(dv+0) - pdata->pl.bborig[0];
	  SP_integer y = dvar_min_l(dv+1) - pdata->pl.bborig[1];
	  if (x <= currentx && currentx < x+S_SIZE(s,0) &&
	      y <= currenty && currenty < y+S_SIZE(s,1))
	    covered = TRUE;
	}
      }
      if (!covered) {
	pdata->pl.holes[2*inf] = currentx;
	pdata->pl.holes[2*inf+1] = currenty;
	inf++;
	if (inf>=sup) {
	  currentx = heldx;
	  currenty = heldy;
	} else {
	  currentx = (int)pdata->pl.holes[2*inf];
	  currenty = (int)pdata->pl.holes[2*inf+1];
	}
      } else {
	pdata->pl.holes[2*sup] = currentx;
	pdata->pl.holes[2*sup+1] = currenty;
	sup--;
	if (inf>=sup) {
	  currentx = heldx;
	  currenty = heldy;
	} else {
	  currentx = (int)pdata->pl.holes[2*sup];
	  currenty = (int)pdata->pl.holes[2*sup+1];
	}
      }
    }
    pdata->pl.nholes = inf;
  }

  if (pdata->pl.nholes < pdata->pl.pslack ||
      !pl_convex_hull_feasible(wam, pdata))
    return FALSE;
  return TRUE;
}

/* pseudocode name: PalletLoadingPost */
static SP_BOOL
pl_post(Wam wam,
	      struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int s, nb_item = pdata->nobjects;
  SP_integer available[256];
  SP_integer item_vol, placement_vol;
  SP_integer x, y, a, b;
  
  if (!pl_recognize(wam, pdata,available,&item_vol,&placement_vol)) {
    pdata->opt &= ~OPT_PALLET_LOADING;
    return TRUE;
  }
  if (pl_check_debruijn(pdata,item_vol,nb_item,placement_vol,available)) {
    return FALSE;
  } else if (kdims!=2) {
    return TRUE;
  } else if (pl_check_bounds(pdata,item_vol,nb_item,placement_vol,available)) {
    return FALSE;
  }
  s = pdata->pl.sbox;
  x = available[1];
  y = available[0];
  a = S_SIZE_MULTISET(s,1);
  b = S_SIZE_MULTISET(s,0);
  if (pl_color(wam, nb_item,x,y,a,b,pdata->pl.islin,pdata)) {
    return FALSE;
  } else {
    return TRUE;
  }
}
#endif /* PALLET_LOADING */



#if LONGEST_HOLE
static SP_integer
get_longest_hole(Wam wam,
		       struct geost_data *pdata,
		       int dim,
		       SP_integer sigma,
		       SP_integer epsilon,
		       int closed)
{
  SP_integer *lh_table = pdata->lh_tables[dim];
  SP_integer *lch_table = pdata->lch_tables[dim];
  SP_integer ix = epsilon-1;
  (void)wam;
  SP_ASSERT(sigma <= pdata->slack);
  
  if (epsilon==0 || !lh_table)
    return CLPFD_MAXINT2;
  if (pdata->opt & OPT_LONGEST_HOLE_ALL)
    ix += sigma * pdata->maxeps[dim];
  return closed ? lch_table[ix] : lh_table[ix];
}

static SP_integer
get_longest_hole_max(Wam wam,
			   struct geost_data *pdata,
			   int dim,
			   SP_integer sigma,
			   SP_integer epsilon)
{
  SP_integer *lh_table_max = pdata->lh_tables_max[dim];
  SP_integer ix = epsilon-1; 
  (void)wam;

  SP_ASSERT(sigma <= pdata->slack);
  
  if (epsilon==0 || !lh_table_max)
    return CLPFD_MAXINT2;
  if (pdata->opt & OPT_LONGEST_HOLE_ALL)
    ix += sigma * pdata->maxeps[dim];
  return lh_table_max[ix];
}
#endif

static int
bb_bounding_box_lst_ect(Wam wam,
			      struct geost_data *pdata,
			      int object)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  SP_integer lst1[256], ect1[256], lst2[256], ect2[256];
  DVITER it;
  int j, rc = -1;

  for (j=0; j<kdims; j++) {
    lst1[j] = -CLPFD_MAXINT;
    ect1[j] =  CLPFD_MAXINT;
  }

  dviter_init(&it, O_SID_DVAR(object));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    int s;

    for (j=0; j<kdims; j++) {
      lst2[j] =  CLPFD_MAXINT;
      ect2[j] = -CLPFD_MAXINT;
    }
    for (s=sbox0; S_SID(s)==sid; s++) {
      for (j=0; j<kdims; j++) {
	SP_integer lstsj = dvar_max_l(dv+j) + S_TRANSLATION(s,j);
	SP_integer ectsj = dvar_min_l(dv+j) + S_TRANSLATION(s,j) + S_SIZE(s,j);
	lst2[j] = lstsj < lst2[j] ? lstsj : lst2[j];
	ect2[j] = ectsj > ect2[j] ? ectsj : ect2[j];
      }
    }
    for (j=0; j<kdims; j++) {
      lst1[j] = lst2[j] > lst1[j] ? lst2[j] : lst1[j];
      ect1[j] = ect2[j] < ect1[j] ? ect2[j] : ect1[j];
    }
  }
  for (j=0; j<kdims; j++) {
    if (dvar_fix_interval_l(pdata->lower_dvar+j,-HighInt,lst1[j])<0)
      goto ret;
    if (dvar_fix_interval_l(pdata->upper_dvar+j,ect1[j],HighInt-1)<0)
      goto ret;
  }
  rc = 0;
 ret:
  return rc;
}

static SP_BOOL
point_not_in_domain(struct geost_data *pdata,
		    SP_integer *point,
		    int object,
		    int *region)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int j, k;
  
  for (j=0; j<kdims; j++)
    if (!dvar_contains_value_l(dv+j,point[j])) {
      int r = *region = pdata->fr.top_of_stack;
      for (k=0; k<kdims; k++)
	if (j==k) {
	  DVITER it;
	  SP_integer min, max;
	  SP_integer fr_end =     CLPFD_MAXINT-1;
	  SP_integer fr_origin = -fr_end;
	  
	  dviter_init(&it, dv+k);
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
	  FR_ORIGIN(r,k) = dvar_min_l(dv+k);
	  FR_END(r,k) = dvar_max_l(dv+k);
	}
      return TRUE;
    }
  return FALSE;
}


static int
visavis_fill(Wam wam,
		   struct geost_data *pdata,
		   int vav,	/* region, bounding box of vav */
		   SP_integer *size,	/* size of vav, also in dimension dim */
		   int dim,	/* vis_a_vis dimension */
		   int curobj,	/* current object */
		   int cursbox,	/* current sbox */
		   int all,	/* TRUE means allow ground objects */
		   SP_integer *min,	/* min. fill */
		   SP_integer *max,	/* max. fill */
		   SP_integer vol)	/* volume of vav */
{
  int kdims = pdata->kdims;
  int i, j, s, n=0;
  SP_integer gap = size[dim];
  int maxtarget = all ? pdata->nobjects : pdata->ntargets;
  SP_integer minfill = 0;
  SP_integer maxfill = 0;
  
  for (i=0; i<maxtarget; i++) {
    int object = (int)TARGET(i);
    if (decomposable(pdata,object)) {
      Dvar dvs = O_SID_DVAR(object);
      Dvar dv  = O_ORIG_DVAR(object,0);
      SP_integer sid = dvar_min_l(dvs);
      int sbox0 = get_first_sbox(sid);
      for (s=sbox0; S_SID(s)==sid; s++) {
	SP_integer slen = S_SIZE(s,dim);
	if (slen<=gap && (object!=curobj || s!=cursbox)) {
	  SP_integer minvol = 1;
	  SP_integer maxvol = 1;
	  SP_integer m;
	  int mnonzero = TRUE;
	  for (j=0; j<kdims; j++) {
	    FR_ORIGIN(vav+1,j) = dvar_min_l(dv+j) + S_TRANSLATION(s,j);
	    FR_END(vav+1,j) = dvar_max_l(dv+j) + S_TRANSLATION(s,j) + S_SIZE(s,j) - 1;
	    m = min_intersection(pdata,vav,vav+1,size,&S_SIZE(s,0),j);
	    if (j==dim && m>0)
	      m = slen;
	    minvol *= m;
	    m = sbox_max_intersection(pdata,dv,s,vav+1,vav,size,j);
	    if (m==0 || (j==dim && m<slen))
	      goto next_s;
	    maxvol *= m;
	  }
	  n += mnonzero;
	  minfill += minvol;
	  if (vol>0) {		/* prevent integer overflow */
	    maxfill += maxvol;
	    vol -= maxvol;
	  }
	}
      next_s: ;
      }
    }
  }
  *min = minfill;
  *max = maxfill;
  return n;
}







static void
compute_minmax_vol_sid(Wam wam,
			     struct geost_data *pdata,
			     int object)
{
  int kdims = pdata->kdims;
  Dvar sdv = O_SID_DVAR(object);
  SP_integer minvol =  CLPFD_MAXINT;
  SP_integer maxvol = -CLPFD_MAXINT;

  if (dvar_is_integer(sdv)) {
    O_MIN_VOL_SID(object) = dvar_min_l(sdv);
    O_MAX_VOL_SID(object) = dvar_min_l(sdv);
  } else {
    SP_integer minsid=0, maxsid=0;
    DVITER it;

    dviter_init(&it, sdv);
    while (!dviter_empty(&it)) {
      SP_integer sid = dviter_next_value_l(&it);
      int sbox0 = get_first_sbox(sid);
      SP_integer svol = SHAPE_VOLUME(sbox0);
      if (svol<minvol) {
	minvol = svol;
	minsid = sid;
      }
      if (svol>maxvol) {
	maxvol = svol;
	maxsid = sid;
      }
    }
    O_MIN_VOL_SID(object) = minsid;
    O_MAX_VOL_SID(object) = maxsid;
  }
}



static SP_BOOL
flagged_in_dependency(struct geost_data *pdata,
		      int object);

/* Let O be flagged iff O_FLAGS(object)&0x1.

   We must resweep an object if any of the following holds:
   - object is flagged
   - (pdata->opt & (OPT_PALLET_LOADING))
   - NOT (pdata->opt & OPT_OVERLAP) AND object's bounding box intersects BB 0 (BB of all flagged objects)
   - (pdata->opt & OPT_LEX) and a chain member is flagged
   - (pdata->opt & OPT_RULES) and a rule dependency is flagged
 */
static SP_BOOL
must_sweep(Wam wam,
		 struct geost_data *pdata,
		 int object)
{
  if (O_FLAGS(object) & 0x1) {
    return TRUE;
  } else if (pdata->opt & (OPT_PALLET_LOADING)) {
    return TRUE;
  } else if (!(pdata->opt & OPT_OVERLAP) &&
	     fr_intersects_object(wam, pdata,/*bounding box*/0,object)) {
    return TRUE;
  } else if ((pdata->opt & OPT_RULES) && flagged_in_dependency(pdata,object)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

#define SWEEPSETNW(K) (((unsigned int)(K) + 31) >> 5)
#define SWEEPSETWORD(D) ((unsigned int)(D) >> 5)
#define SWEEPSETBIT(D) (1U << ((unsigned int)(D) & 31))

static void
init_sweepset(struct geost_data *pdata)
{
  int n = SWEEPSETNW(pdata->kdims);
  int i;

  for (i=0; i<n; i++) {
    pdata->sweepset.min[i] = ~0U;
    pdata->sweepset.max[i] = ~0U;
  }
}

static SP_BOOL
test_sweepset(struct geost_data *pdata, int d, SP_BOOL min)
{
  int wd = SWEEPSETWORD(d);
  int b = SWEEPSETBIT(d);
  
  if (min)
    return pdata->sweepset.min[wd] & b;
  else
    return pdata->sweepset.max[wd] & b;
}

static void
clear_sweepset(struct geost_data *pdata, int d, SP_BOOL min)
{
  int wd = SWEEPSETWORD(d);
  int b = SWEEPSETBIT(d);
  
  if (min)
    pdata->sweepset.min[wd] &= ~b;
  else
    pdata->sweepset.max[wd] &= ~b;
}

static void
init_up(struct geost_data *pdata,
	Dvar dv,
	int dim)
{
  int kdims = pdata->kdims;
  SP_BOOL atmin = TRUE;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    if (atmin)
      pdata->next[j1] = dvar_max_l(dv+j1)+1;
    else
      pdata->next[j1] = pdata->current[j1]+1;
    if (pdata->current[j1]>dvar_min_l(dv+j1)) /* was != */
      atmin = FALSE;
  }
}

static int
adjust_up(struct geost_data *pdata,
	  Dvar dv,
	  int dim)
{
  int kdims = pdata->kdims;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    pdata->current[j1] = pdata->next[j1];
    pdata->next[j1] = dvar_max_l(dv+j1)+1;
    if (pdata->current[j1] < pdata->next[j1])
      return 0;			/* SUCCESS */
    pdata->current[j1] = dvar_min_l(dv+j1);
  }
  return -1;			/* FAILURE */
}

static SP_BOOL
inside_forbidden(Wam wam,
		 struct geost_data *pdata,
		 SP_integer *p,
		 int object,
		 SP_integer sid,
		 int *cookie)
{
  return (point_not_in_domain(pdata,p,object,cookie) ||
	  point_in_some_fr(wam, pdata,object,sid,p,cookie,inside_forbidden_cont));
}

/* returns -1 for failure, 0 for no change, >0 for pruned */
static int
prune_min(Wam wam,
	  struct geost_data *pdata,
	  int object,
	  SP_integer sid,
	  int dim,
	  int monomorphic)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int region;
  int rc = 0;
  int j;

  for (j=0; j<kdims; j++)
    pdata->current[j] = dvar_min_l(dv+j);
  init_up(pdata,dv,dim);
  while (rc>=0 &&
	 inside_forbidden(wam, pdata,pdata->current,object,sid,&region)) {
    for (j=0; j<kdims; j++)
      if (pdata->next[j] > FR_END(region,j)+1)
	pdata->next[j] = FR_END(region,j)+1;
    rc = adjust_up(pdata, dv, dim);
  }
  if (rc>=0) {
    rc = dvar_fix_min_l(dv+dim,pdata->current[dim]);
    if (rc>0 && monomorphic) {
      pdata->pruneddim[dim] = 1;
      O_FLAGS(object) |= 0x3;	/* pruned, queued */
    }
    for (j=0; j<kdims; j++) {
      if (pdata->current[j] == dvar_min_l(dv+j))
	clear_sweepset(pdata,j,TRUE/*min*/);
      if (pdata->current[j] == dvar_max_l(dv+j))
	clear_sweepset(pdata,j,FALSE/*max*/);
     }
  }
  return rc;
}

static void
init_down(struct geost_data *pdata,
	  Dvar dv,
	  int dim)
{
  int kdims = pdata->kdims;
  SP_BOOL atmax = TRUE;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    if (atmax)
      pdata->next[j1] = dvar_min_l(dv+j1)-1;
    else
      pdata->next[j1] = pdata->current[j1]-1;
    if (pdata->current[j1]<dvar_max_l(dv+j1)) /* was != */
      atmax = FALSE;
  }
}

static int
adjust_down(struct geost_data *pdata,
	    Dvar dv,
	    int dim)
{
  int kdims = pdata->kdims;
  int j, j1;
  
  for (j=kdims-1; j>=0; j--) {
    j1 = (j+dim) % kdims;
    pdata->current[j1] = pdata->next[j1];
    pdata->next[j1] = dvar_min_l(dv+j1)-1;
    if (pdata->current[j1] > pdata->next[j1])
      return 0;			/* SUCCESS */
    pdata->current[j1] = dvar_max_l(dv+j1);
  }
  return -1;			/* FAILURE */
}

/* returns -1 for failure, 0 for no change EVEN IF NEW WITNESS, >0 for pruned */
static int
prune_max(Wam wam,
	  struct geost_data *pdata,
	  int object,
	  SP_integer sid,
	  int dim,
	  int monomorphic)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int region;
  int rc = 0;
  int j;

  for (j=0; j<kdims; j++)
    pdata->current[j] = dvar_max_l(dv+j);
  init_down(pdata,dv,dim);
  while (rc>=0 &&
	 inside_forbidden(wam, pdata,pdata->current,object,sid,&region)) {
    for (j=0; j<kdims; j++)
      if (pdata->next[j] < FR_ORIGIN(region,j)-1)
	pdata->next[j] = FR_ORIGIN(region,j)-1;
    rc = adjust_down(pdata, dv, dim);
  }
  if (rc>=0) {
    rc = dvar_fix_max_l(dv+dim,pdata->current[dim]);
    if (rc>0 && monomorphic) {
      pdata->pruneddim[dim] = 1;
      O_FLAGS(object) |= 0x3;	/* pruned, queued */
    }
    for (j=0; j<kdims; j++) {
      if (pdata->current[j] == dvar_min_l(dv+j))
	clear_sweepset(pdata,j,TRUE/*min*/);
      if (pdata->current[j] == dvar_max_l(dv+j))
	clear_sweepset(pdata,j,FALSE/*max*/);
    }
  }
  return rc;
}



static int
prune_fixall(Wam wam,
	     struct geost_data *pdata,
	     int object,
	     SP_integer sid,
	     int fixix)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int region, j;
  SP_BOOL atmin = TRUE;

  for (j=kdims-1; j>=0; j--) {
    int signj = (int)pdata->fixorder[fixix+j];
    int dim = signj<0 ? -signj-2 : signj-2;
    Dvar dvdim = dv+dim;
    SP_integer min_if_up = signj<0 ? dvar_min_l(dvdim) : dvar_max_l(dvdim);
    SP_integer max_if_up = signj<0 ? dvar_max_l(dvdim) : dvar_min_l(dvdim);
    SP_integer one_if_up = signj<0 ? 1 : -1;
    
    pdata->current[dim] =
      min_if_up;
    if (atmin)
      pdata->next[dim] = max_if_up + one_if_up;
    else
      pdata->next[dim] = pdata->current[dim] + one_if_up;
    if (pdata->current[dim]!=min_if_up)
      atmin = FALSE;
  }
  while (inside_forbidden(wam, pdata,pdata->current,object,sid,&region)) {
    for (j=kdims-1; j>=0; j--) {
      int signj = (int)pdata->fixorder[fixix+j];
      int dim = signj<0 ? -signj-2 : signj-2;
      if (signj<0) {
	if (pdata->next[dim] > FR_END(region,dim)+1)
	  pdata->next[dim] = FR_END(region,dim)+1;
      } else {
	if (pdata->next[dim] < FR_ORIGIN(region,dim)-1)
	  pdata->next[dim] = FR_ORIGIN(region,dim)-1;
      }
    }
    for (j=kdims-1; j>=0; j--) {
      int signj = (int)pdata->fixorder[fixix+j];
      int dim = signj<0 ? -signj-2 : signj-2;
      SP_integer min_if_up = signj<0 ? dvar_min_l(dv+dim) : dvar_max_l(dv+dim);
      SP_integer max_if_up = signj<0 ? dvar_max_l(dv+dim) : dvar_min_l(dv+dim);
      SP_integer one_if_up = signj<0 ? 1 : -1;
      pdata->current[dim] = pdata->next[dim];
      pdata->next[dim] = max_if_up + one_if_up;
      if (signj<0 ? (pdata->current[dim] < pdata->next[dim]) : 
          (pdata->current[dim] > pdata->next[dim]))
	goto next;
      pdata->current[dim] = min_if_up;
    }
    return -1;
  next: ;
  }
  return 0;
}

static int
filter_object_for_shape(Wam wam,
			struct geost_data *pdata,
			int object,
			SP_integer sid,
			int monomorphic)
{
  int kdims = pdata->kdims;
  int dim, change=0;
  Dvar dv = O_ORIG_DVAR(object,0);

#if EXPLICIT_FBOXES
  pdata->fr.end_absolute = pdata->fr.end_relative;
  (void)absolute_fr(wam, pdata,object,sid,&pdata->fr.end_absolute,absolute_cont);
  pdata->fr.top_of_stack = pdata->fr.end_absolute;
  pdata->fr.round_robin = pdata->fr.end_absolute;
  switch (pre_sweep(wam, pdata,object,monomorphic)) {
  case -1:
    return -1;
  case 0:
    break;
  default:
    change = 1;
  }
#endif
  init_sweepset(pdata);
  for (dim=0; dim<kdims-1; dim++)
    if (!dvar_is_integer(dv+dim))
      break;
  for (; dim<kdims; dim++) {
    if (test_sweepset(pdata,dim,TRUE/*min*/)) {
      switch (prune_min(wam,pdata,object,sid,dim,monomorphic)) {
      case -1:
	return -1;
      case 0:
	break;
      default:
	change = 1;
      }
    }
    if (test_sweepset(pdata,dim,FALSE/*max*/)) {
      switch (prune_max(wam,pdata,object,sid,dim,monomorphic)) {
      case -1:
	return -1;
      case 0:
	break;
      default:
	change = 1;
      }
    }
  }
  return change;
}


static void
object_to_virtual(struct geost_data *pdata,
		  int object,
		  SP_integer sid)
{
  int kdims = pdata->kdims;
  int virtual = pdata->nobjects;
  int dim;
  O_PROC(virtual) = O_PROC(object);
  O_FLAGS(virtual) = O_FLAGS(object);
  O_OID(virtual) = O_OID(object);
  *O_SID_DVAR(virtual) = *O_SID_DVAR(object);
  dvar_fix_value_l(O_SID_DVAR(virtual), sid);
  for (dim=0; dim<kdims; dim++) {
    *O_ORIG_DVAR(virtual,dim) = *O_ORIG_DVAR(object,dim);
  }
}


static int
virtual_to_object(struct geost_data *pdata,
		  int object)
{
  int kdims = pdata->kdims;
  int rc = 0;
  int dim;
  for (dim=0; dim<kdims; dim++) {
    int sc = dvar_fix_interval_l(O_ORIG_DVAR(object,dim),
				 pdata->virtual_min[dim],
				 pdata->virtual_max[dim]);
    if (sc < 0) {
      return sc;
    } else if (sc > 0) {
      rc = 1;
      pdata->pruneddim[dim] = 1;
      O_FLAGS(object) |= 0x3;	/* pruned, queued */
    }
  }
  return rc;
}

#if CUMULATIVE_METHOD
/* return one of 0(succeed), 0x1(propagate), 0x2(fail) */
static int
geost_propagate_method(Wam wam,
			     struct task *t)
{
  struct geost_data *pdata = fd.gdata;
  int d = pdata->curdim;
  int kdims = pdata->kdims;
  int object = t->id;
  SP_integer offset = S_TRANSLATION(t->part,d);
  SP_integer size = S_SIZE(t->part,d);
  
  switch (dvar_fix_interval_l(O_ORIG_DVAR(object,d),t->est-offset,t->lctmax-offset-size)) {
  case -1:
    return 0x2;
  case 0:
    return 0x0;
  default:
    t->est = dvar_min_l(O_ORIG_DVAR(object,d)) + offset; /* necessary when holes in domain */
    t->lct = dvar_max_l(O_ORIG_DVAR(object,d)) + offset + size;	/* necessary when holes in domain */
    t->lctmax = t->lct;
    O_FLAGS(object) |= 0x3;
    return 0x1;
  }
}

static void
geost_refresh_method(Wam wam, struct task *t)
{
  struct geost_data *pdata = fd.gdata;
  int d = pdata->curdim;
  int kdims = pdata->kdims;
  (void)dvar_set(O_ORIG_DVAR(t->id,d));
}

static TAGGED
geost_set_method(Wam wam, struct task *t)
{
  struct geost_data *pdata = fd.gdata;
  int d = pdata->curdim;
  int kdims = pdata->kdims;
  SP_integer offset = S_TRANSLATION(t->part,d);
  TAGGED set = dvar_set(O_ORIG_DVAR(t->id,d));

  if (offset!=0)
    set = fd_lsh(wam,set,MakeSmall(offset));

  return set;
}

static int
geost_prune_interval_method(Wam wam,
			    struct task *t,
			    SP_integer min, SP_integer max)
{
  struct geost_data *pdata = fd.gdata;
  int d = pdata->curdim;
  int kdims = pdata->kdims;
  int object = t->id;
  SP_integer offset = S_TRANSLATION(t->part,d);
  
  switch (dvar_prune_interval_l(O_ORIG_DVAR(object,d),min-offset,max-offset)) {
  case -1:
    return 0x2;
  case 0:
    return 0x0;
  default:
    O_FLAGS(object) |= 0x3;
    return 0x1;
  }
}

static int
geost_prune_set_method(Wam wam, struct task *t, TAGGED set)
{
  struct geost_data *pdata = fd.gdata;
  int d = pdata->curdim;
  int kdims = pdata->kdims;
  int object = t->id;
  SP_integer offset = S_TRANSLATION(t->part,d);
  SP_integer size = S_SIZE(t->part,d);
  Dvar dvar = O_ORIG_DVAR(object,d);
  int rc;
  
  if (offset!=0)
    set = fd_lsh(wam,set,MakeSmall(-offset));
  switch (rc=dvar_prune_set(dvar,set)) {
  case -1:
    return 0x2;
  case 0:
    return 0x0;
  default:
    if (rc & (DV_PRUNED_MIN|DV_PRUNED_MIN)) {
      t->est = dvar_min_l(dvar) + offset;
      t->lct = dvar_max_l(dvar) + offset + size;
      t->lctmax = t->lct;
    }
    O_FLAGS(object) |= 0x3;
    return 0x1;
  }
}

static int
geost_is_interval_method(Wam wam,
			       struct task *t)
{
  struct geost_data *pdata = fd.gdata;
  int d = pdata->curdim;
  int kdims = pdata->kdims;
  
  return t->est + t->dur == t->lctmax || dvar_is_interval(O_ORIG_DVAR(t->id,d));
}

static SP_integer
geost_lh_method(Wam wam,
		      SP_integer sigma,
		      SP_integer epsilon,
		      int closed)
{
  struct geost_data *pdata = fd.gdata;
  if (pdata->opt & OPT_LONGEST_HOLE) {
    int curdim = pdata->curdim;
    return get_longest_hole(wam, pdata,curdim,sigma,epsilon,closed);
  } else {
    return CLPFD_MAXINT2;
  }
}

static SP_integer
geost_lh_dummy(Wam wam,
		     SP_integer sigma,
		     SP_integer epsilon,
		     int closed)
{
  (void)fd;
  (void)sigma;
  (void)epsilon;
  (void)closed;
  return CLPFD_MAXINT2;
}


#define DP_HASHADD(T) (hash_tmp[k]+=(T), k=(k<2 ? k+1 : 0))
#define DP_HASHMASK ((SP_integer)-1)
#define DP_HASHSIZE 4

static int
geost_dp_hash_method(Wam wam,
			   int msg,
			   SP_integer *support,
			   int n,
			   SP_integer lslack,
			   SP_integer rslack,
			   SP_integer sslack)
{
  struct sw_on_key_node *keyval;
  struct geost_data *pdata = fd.gdata;
  int curdim = pdata->curdim;
  SP_uinteger hash_tmp[3] = {0,0,0};
  struct dp *node;
  unsigned short *data;
  int i, k=0;

  DP_HASHADD(lslack);
  DP_HASHADD(rslack);
  DP_HASHADD(sslack);
  for (i=0; i<n; i++)
    DP_HASHADD(support[i]>>8);
  hash_tmp[1] ^= hash_tmp[2];
  hash_tmp[0] ^= hash_tmp[1];
  hash_tmp[0] = (((hash_tmp[2] << 7) + hash_tmp[1]) << 7) + hash_tmp[0];
  hash_tmp[0] &= DP_HASHMASK;
    
  switch (msg) {
  case 1:			/* gethash */
    node = incore_gethash(pdata->dp_hash[curdim],hash_tmp[0]<<1)->value.any; /* 4.3.2: <<1 for ht expansion precond */
    while (node) {
      data = (unsigned short *)(node+1);
      if (node->lslack!=lslack || node->rslack!=rslack || node->sslack!=sslack)
	goto next;
      for (i=0; i<n; i++)
	if (support[i]>>8 != data[i]>>8)
	  goto next;
      for (i=0; i<n; i++)
	support[i] = data[i];
      return 1;
    next:
      node = node->next;
    }
    break;
  case 2:			/* puthash */
    keyval = dyn_puthash(&pdata->dp_hash[curdim],hash_tmp[0]<<1); /* 4.3.2: <<1 for ht expansion precond */
    node = (struct dp *)sp_checkalloc(sizeof(*node)+n*sizeof(unsigned short),
				      FALSE);
    node->next = keyval->value.any;
    node->n = n;
    node->lslack = lslack;
    node->rslack = rslack;
    node->sslack = sslack;
    data = (unsigned short *)(node+1);
    for (i=0; i<n; i++)
      data[i] = (unsigned short)support[i];
    keyval->value.any = node;
    break;
  }
  return 0;
}

struct geost_event {
  SP_integer key;
  int object;
  int sbox;
};				/* followed by n unsigned chars */

static int 
cmp_geost_event(Wam wam,
		struct geost_event *p1,
		struct geost_event *p2)
{
  int cmp = CMP(p1->key,p2->key);
  (void)wam;
  if (cmp==0)
    cmp = CMP(p1->object,p2->object);
  if (cmp==0)
    cmp = CMP(p1->sbox,p2->sbox);
  return cmp;
}

#define QType struct geost_event
#define QCmp  cmp_geost_event
#define QSort qsort_geost_event
#include "qsort.ic"

static int
cumulable(Wam wam,
		struct geost_data *pdata,
		int object,
		int bb,
		int dim)
{
  int kdims = pdata->kdims;
  Dvar dvs = O_SID_DVAR(object);
  Dvar dv = O_ORIG_DVAR(object,0);
  
  if (dvar_is_integer(dvs)) {	/* case 1: single shape, possibly multiple sboxes */
    SP_integer sid = dvar_min_l(dvs);	/* TODO: clip to bb */
    int n=0, s, sbox = get_first_sbox(sid);
    for (s=sbox; S_SID(s)==sid; s++)
      if (sbox_intersects_region(pdata,dv,s,bb))
	pdata->cumulative_sbox[n++] = s;
    return n;
  } else {			/* case 2: polymorphic, require single sbox for each shape */
    SP_integer minheight = CLPFD_MAXINT;
    DVITER it;
    int s = pdata->nsboxes_posted + object; /* build a virtual sbox */
    int j;

    for (j=0; j<kdims; j++) {
      S_TRANSLATION(s,j) = 0;
      S_SIZE(s,j) = CLPFD_MAXINT;
    }
    dviter_init(&it, dvs);
    while (!dviter_empty(&it)) {
      SP_integer sid = dviter_next_value_l(&it);
      int sbox = get_first_sbox(sid);
      SP_integer thisheight = S_VOLUME(sbox)/S_SIZE(sbox,dim);

      if (S_SID(sbox)==S_SID(sbox+1)) /* multiple sboxes */
	return 0;
      minheight = minheight < thisheight ? minheight : thisheight;
      for (j=0; j<kdims; j++) {
	SP_integer begin1 = S_TRANSLATION(s,j);
	SP_integer begin2 = S_TRANSLATION(sbox,j);
	SP_integer end1 = begin1 + S_SIZE(s,j);
	SP_integer end2 = begin2 + S_SIZE(sbox,j);

	begin1 = begin1 > begin2 ? begin1 : begin2;
	end1 = end1 < end2 ? end1 : end2;
	if (begin1>=end1)
	  return 0;
	S_TRANSLATION(s,j) = begin1;
	S_SIZE(s,j) = end1 - begin1;
      }
    }
    S_VOLUME(s) = minheight*S_SIZE(s,dim);
    pdata->cumulative_sbox[0] = s;
    return 1;
  }
}

static int
disjunctive_set(Wam wam,
		struct geost_data *pdata,
		struct geost_event **eventp,
		int n,
		int dim)
{
  int kdims = pdata->kdims;
  struct dvar limitvar;
  int i, rc=0;

  for (i=0; i<n; i++) {
    int object = eventp[i]->object;
    int sbox   = eventp[i]->sbox;
    struct task *t = pdata->task+i;
    t->est = dvar_min_l(O_ORIG_DVAR(object,dim)) + S_TRANSLATION(sbox,dim);
    t->lct = dvar_max_l(O_ORIG_DVAR(object,dim)) + S_TRANSLATION(sbox,dim) + S_SIZE(sbox,dim);
    t->lctmax = t->lct;
    t->dur = S_SIZE(sbox,dim);
    t->res = 1;
    t->enable = 2;
    t->diffid = i;
    t->id = object;
    t->part = sbox;
    t->propagate = geost_propagate_method;
    t->refresh = geost_refresh_method;
    t->set = geost_set_method;
    t->prune_interval = geost_prune_interval_method;
    t->prune_set = geost_prune_set_method;
    t->is_interval = geost_is_interval_method;
  }
  dvar_init_temp(&limitvar,TaggedOne,TaggedOne);
  pdata->curdim = dim;
  /* TODO: sboxes within one object have fixed distances!
     0x2 - use task intervals and "slack filtering" methods
     0x4 - use dynamic programming methods
     0x8 -all heights and durations are fixed
  */
  rc |= fd_discrete_filtering(wam, pdata->task, n, n,
			   0x2,	/* like cumulative(..., [global(true)]) */
			   /* +0x8,  all heights and durations fixed - 
                              disable slack methods for limit=1 */
			   NULL,
			   &limitvar,
			   pdata->lower_dvar+dim,
			   pdata->upper_dvar+dim,
			   NULL, 0,
			   NULL, /* pdata->lh_tables[dim] is INVALID */
			   NULL,
			   pdata->size_mem);
  return rc;
}

static int
disjunctive_sets(Wam wam,
		 struct geost_data *pdata,
		 int nbtask,
		 int dim)
{
  int kdims = pdata->kdims;
  struct geost_event *event = Malloc(2*nbtask, struct geost_event);
  struct geost_event **eventp = Malloc(nbtask, struct geost_event *);
  int i, j, ev, lastkey=0, rc=0, cim=dim^1, top=0;

  for (i=0, ev=0; i<pdata->nobjects; i++) { /* for all objects intersecting BB 1 */
    int object = i;
    Dvar dv = O_ORIG_DVAR(object,0);
    int n = cumulable(wam, pdata,object,/*bounding box*/1,dim);

    if (n>0) {
      for (j=0; j<n; j++) {
	int s = (int)pdata->cumulative_sbox[j];
	SP_integer lst = dvar_max_l(dv+cim) + S_TRANSLATION(s,cim);
	SP_integer ect = dvar_min_l(dv+cim) + S_TRANSLATION(s,cim) + S_SIZE(s,cim);
	if (lst < ect) {
	  event[ev].key = (lst*2)+1;
	  event[ev].object = object;
	  event[ev].sbox = s;
	  event[ev+1].key = (ect*2);
	  event[ev+1].object = object;
	  event[ev+1].sbox = s;
	  ev += 2;      
	}
      }
    }
  }
  qsort_geost_event(wam, event,ev);
  for (i=0; i<ev && rc<2; i++) {
    struct geost_event *ev = event+i;
    SP_integer key = ev->key;

    if (lastkey && !(key & 1) && top>1)
      rc |= disjunctive_set(wam, pdata,eventp,top,dim);

    if (key & 1) {		/* start event */
      eventp[top++] = ev;
    } else {			/* end event */
      int j, k;
      for (j=0, k=0; j<top; j++) {
	struct geost_event *ev1 = eventp[j];
	if (ev->object!=ev1->object || ev->sbox!=ev1->sbox)
	  eventp[k++] = ev1;
      }
      top = k;
    }
    lastkey = (int)key;
  }
  Free(eventp);
  Free(event);
  return rc;
}

/* clipping to bounding box */
static int
check_cumulative(Wam wam, struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int nbtask=0, nbdiffs, rc=0, object, d;
  int dp_hash_valid=1;
  SP_integer vol=1;

  for (object=0; object<pdata->nobjects; object++) {
    int n = cumulable(wam, pdata,object,/*bounding box*/1,0);
    
    if (n>0) {
      int i;
      for (i=0; i<n; i++) {
	int s = (int)pdata->cumulative_sbox[i];
	if (s>=pdata->nsboxes_posted ||
	    nbtask >= pdata->check_cumulative_set_count ||
	    pdata->check_cumulative_set[nbtask]!=s)
	  dp_hash_valid = 0;
	pdata->check_cumulative_set[nbtask++] = s;
      }
    }
  }
  if (nbtask != pdata->check_cumulative_set_count)
    dp_hash_valid = 0;
  pdata->check_cumulative_set_count = nbtask;
  for (d=0; d<kdims; d++) {
    vol *= FR_END(/*bounding box*/1,d) - FR_ORIGIN(/*bounding box*/1,d) + 1;
  }
  if (!dp_hash_valid) {
    dispose_dp_hash(wam, pdata);
    for (d=0; d<kdims; d++)
      pdata->dp_hash[d] = new_switch_on_key(DP_HASHSIZE,NULL);
  }
  if (nbtask<2)
    goto ret;
  if (pdata->opt & OPT_DISJUNCTIVE) {
    for (d=0; d<2 && rc<2; d++) {
      if (pdata->prunedsid || pdata->pruneddim[d])
	rc |= disjunctive_sets(wam, pdata,nbtask,d);
    }
  }
  for (d=0; (pdata->opt & OPT_CUMULATIVE) && d<kdims && rc<2; d++) {
    if (pdata->prunedsid || pdata->pruneddim[d]) {
      struct diff_constraint *dc = NULL;
      struct dvar *diffvar = NULL;
      struct dvar limitvar;
      struct dvar gezvar;
      SP_integer limit = vol/(FR_END(/*bounding box*/1,d) - FR_ORIGIN(/*bounding box*/1,d) + 1);
      SP_integer total_volume=0;
      lh_method *lh_method = geost_lh_method;
      
      nbtask = 0;
      for (object=0; object<pdata->nobjects; object++) { /* for all objects intersecting BB 1 */
	int isground = object_is_ground(pdata,object);
	int n = cumulable(wam, pdata,object,/*bounding box*/1,d);
	
	if (n>0) {
	  Dvar dv = O_ORIG_DVAR(object,0);
	  int i;
	  for (i=0; i<n; i++) {
	    int s = (int)pdata->cumulative_sbox[i];
	    struct task *t = pdata->task+nbtask;
	    
	    if (!isground) {
	      t->est = dvar_min_l(dv+d) + S_TRANSLATION(s,d);
	      t->lct = dvar_max_l(dv+d) + S_TRANSLATION(s,d) + S_SIZE(s,d);
	      t->dur = S_SIZE(s,d);
	      t->res = S_VOLUME(s)/t->dur;
	    } else {
	      int j;
	      t->res = 1;
	      for (j=0; j<kdims; j++) {
		SP_integer start = dvar_min_l(dv+j) + S_TRANSLATION(s,j);
		SP_integer completion = start + S_SIZE(s,j);
	      
		start = start >= FR_ORIGIN(/*bounding box*/1,j) ? start : 
		        FR_ORIGIN(/*bounding box*/1,j);
		completion = completion <= FR_END(/*bounding box*/1,j)+1 ? completion : 
		             FR_END(/*bounding box*/1,j)+1;
		if (j==d) {
		  t->est = start;
		  t->lct = completion;
		  t->dur = completion-start;
		} else {
		  t->res *= completion-start;
		}
	      }
	      if (t->dur*t->res < S_VOLUME(s))
		lh_method = geost_lh_dummy;
	    }
	    t->lctmax = t->lct;
	    t->enable = 2;
	    t->diffid = nbtask;
	    t->id = object;
	    t->part = s;
	    t->propagate = geost_propagate_method;
	    t->refresh = geost_refresh_method;
	    t->set = geost_set_method;
	    t->prune_interval = geost_prune_interval_method;
	    t->prune_set = geost_prune_set_method;
	    t->is_interval = geost_is_interval_method;
	    total_volume += t->res * t->dur;
	    nbtask++;
	  }
	}
      }
      SP_ASSERT(vol-total_volume <= pdata->slack);
      dvar_init_temp(&limitvar,MakeSmall(limit),MakeSmall(limit));
      dvar_init_temp(&gezvar,TaggedZero,MakeSmall(HighInt-1)); /* NOT Sup -- assumed tagged integer */
      pdata->curdim = d;
      nbdiffs = 0;
      /* TODO: sboxes within one object have fixed distances!
	 0x2 - use task intervals and "slack filtering" methods
	 0x4 - use dynamic programming methods
	 0x8 - all heights and durations are fixed
      */
      rc |= fd_discrete_filtering(wam, pdata->task, nbtask, nbtask,
			       0x2+ /* like cumulative(..., [global(true)]) */
			       (pdata->opt&OPT_DP ? 0x4 : 0)+ /* dynamic programming? */
			       0x8, /* all heights and durations fixed */
			       diffvar,
			       &limitvar,
			       pdata->lower_dvar+d,
			       pdata->upper_dvar+d,
			       dc,
			       nbdiffs,
			       lh_method,
			       geost_dp_hash_method,
			       pdata->size_mem);
    }
  }
 ret:
  return rc;
}
#endif

/* Sboxes may overlap. */
/* bb == -1 implies total size of object, anywhere. */
/* bb >= 0 implies object is ground, we want the volume of its intersection with bb */
static SP_integer
intersection_volume(Wam wam,
			  struct geost_data *pdata,
			  int bb,
			  int object,
			  SP_integer sid)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int sbox0 = get_first_sbox(sid);
  SP_integer volume = 0;
  int s, j;
  SP_integer *p;
  
  p = pdata->shape.boundaries;
  for (j=0; j<kdims; j++) {
    SP_integer *p0, *p1, *p2;
    p0 = p;
    for (s=sbox0; S_SID(s)==sid; s++) {
      if (bb == -1) {
	p[0] = S_TRANSLATION(s,j);
	p[1] = p[0] + S_SIZE(s,j);
	p += 2;
      } else {
	SP_integer orig = dvar_min_l(dv+j) + S_TRANSLATION(s,j);
	SP_integer end  = orig + S_SIZE(s,j) - 1;
	if (end>=FR_ORIGIN(bb,j) && FR_END(bb,j)>=orig) {
	  p[0] = orig;
	  if (p[0] < FR_ORIGIN(bb,j))
	    p[0] = FR_ORIGIN(bb,j);
	  p[1] = end+1;
	  if (p[1] > FR_END(bb,j)+1)
	    p[1] = FR_END(bb,j)+1;
	  p += 2;
	}
      }
    }
    if (p==p0)
      goto ret;
    fd_qsort_asc_long(wam, p0, (int)(p-p0));
    for (p1=p2=p0+1; p1<p; p1++)
      if (p1[0]!=p1[-1])
	*p2++ = p1[0];
    pdata->shape.min[j] = p0-pdata->shape.boundaries;
    pdata->shape.max[j] = p2-1-pdata->shape.boundaries;
    pdata->shape.cur[j] = p0-pdata->shape.boundaries;
  }

  /* For each combination from the kdim partitions:
     Let c = its smallest_lex corner.
     If c is in some shape,
     then add to volume the size of this partition.
   */
 combine:
  for (s=sbox0; S_SID(s)==sid; s++) {
    SP_integer term = 1;
    for (j=0; j<kdims; j++) {
      SP_integer point = pdata->shape.boundaries[pdata->shape.cur[j]];
      SP_integer orig = S_TRANSLATION(s,j);
      SP_integer end = orig + S_SIZE(s,j) - 1;
      if (bb >= 0)
	point -= dvar_min_l(dv+j);
      if (point < orig || point > end)
	goto next_s;
    }
    for (j=0; j<kdims; j++) {
      SP_integer fac = pdata->shape.boundaries[pdata->shape.cur[j]+1] - 
	         pdata->shape.boundaries[pdata->shape.cur[j]];
      if (term < MAXSPINT/fac)
	term *= fac;
      else
	pdata->overflow = 1;
    }
    if (volume < MAXSPINT-term)
      volume += term;
    else
      pdata->overflow = 1;
    break;
  next_s: ;
  }
  for (j=kdims-1; j>=0; j--) {
    pdata->shape.cur[j]++;
    if (pdata->shape.cur[j]<pdata->shape.max[j])
      goto combine;
    pdata->shape.cur[j] = pdata->shape.min[j];
  }
 ret:
  return volume;
}

static SP_integer
min_intersection_volume(Wam wam,
			      struct geost_data *pdata,
			      int bb,
			      int object)
{
  int kdims = pdata->kdims;
  SP_integer min_volume = CLPFD_MAXINT;
  DVITER it;
  Dvar dvs = O_SID_DVAR(object);
  
  dviter_init(&it, dvs);
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    SP_integer volume = intersection_volume(wam, pdata,bb,object,sid);
    if (min_volume > volume)
      min_volume = volume;
  }
  return min_volume;
}

static SP_BOOL
filter_volume(Wam wam, struct geost_data *pdata)
{
  SP_BOOL change = TRUE;
  int i, kdims = pdata->kdims;
  SP_integer F, E;

  if (!(pdata->opt & OPT_OVERLAP) &&
      dvar_fix_max_l(pdata->volume_dvar,pdata->target_avail+pdata->rest_volume)<0)
    return FALSE;

  F =   dvar_max_l(pdata->volume_dvar) - pdata->rest_volume;
  E = - dvar_min_l(pdata->volume_dvar) + pdata->rest_volume;

  for (i=0; i<pdata->ntargets; i++) {
    int object = (int)TARGET(i);
    F -= SHAPE_VOLUME(get_first_sbox(O_MIN_VOL_SID(object)));
    E += SHAPE_VOLUME(get_first_sbox(O_MAX_VOL_SID(object)));
  }

  while (change) {
    change = FALSE;
    if (E<0 || F<0)
      return FALSE;
    if (!dvar_is_integer(pdata->volume_dvar)) {
      SP_integer minvol = dvar_min_l(pdata->volume_dvar);
      SP_integer maxvol = dvar_max_l(pdata->volume_dvar);
      
      if (maxvol-minvol > F) {
	change = TRUE;
	dvar_fix_min_l(pdata->volume_dvar,maxvol-F); /* can't fail? */
	E += minvol;
	E -= dvar_min_l(pdata->volume_dvar);
      } else if (maxvol-minvol > E) {
	change = TRUE;
	dvar_fix_max_l(pdata->volume_dvar,minvol+E); /* can't fail? */
	F -= maxvol;
	F += dvar_max_l(pdata->volume_dvar);
      }
    }
    for (i=0; i<pdata->ntargets; i++) {
      SP_BOOL pruned = FALSE;
      int object = (int)TARGET(i);
      Dvar dvs = O_SID_DVAR(object);
      if (!dvar_is_integer(dvs)) {
	SP_integer minvol = SHAPE_VOLUME(get_first_sbox(O_MIN_VOL_SID(object)));
	SP_integer maxvol = SHAPE_VOLUME(get_first_sbox(O_MAX_VOL_SID(object)));
	
	if (maxvol-minvol > F) {
	  pruned = TRUE;
	  dvar_prune_value_l(dvs,O_MAX_VOL_SID(object));
	  compute_minmax_vol_sid(wam, pdata,object);
	  E -= maxvol;
	  E += SHAPE_VOLUME(get_first_sbox(O_MAX_VOL_SID(object)));
	} else if (maxvol-minvol > E) {
	  pruned = TRUE;
	  dvar_prune_value_l(dvs,O_MIN_VOL_SID(object));
	  compute_minmax_vol_sid(wam, pdata,object);
	  F += minvol;
	  F -= SHAPE_VOLUME(get_first_sbox(O_MIN_VOL_SID(object)));
	}
	if (pruned) {
	  change = TRUE;
	  O_FLAGS(object) |= 0x3;
	  pdata->prunedsid = 1;
	}
      }
    }
  }
  return TRUE;
}


static SP_BOOL
fix_object_for_shape(Wam wam,
		     struct geost_data *pdata,
		     int object,
		     SP_integer sid,
		     int fixix
		     )
{
  int kdims = pdata->kdims;
  int j, rc = FALSE;
  SP_integer *witness;

#if EXPLICIT_FBOXES
  pdata->fr.end_absolute = pdata->fr.end_relative;
  (void)absolute_fr(wam, pdata,object,sid,&pdata->fr.end_absolute,absolute_cont);
  pdata->fr.top_of_stack = pdata->fr.end_absolute;
  pdata->fr.round_robin = pdata->fr.end_absolute;
#endif
  if (prune_fixall(wam, pdata,object,sid,fixix+1)>=0) {
    witness = pdata->current;
    for (j=0; j<kdims; j++) {
      if (dvar_fix_value_l(O_ORIG_DVAR(object,j),witness[j])<0)
	goto ret;
    }
    rc = TRUE;
  }
 ret:
  return rc;
}

static SP_BOOL
fixall_geost(Wam wam, struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int fixix = 0;
  int i;
#if EXPLICIT_FBOXES
  if (!(pdata->opt & OPT_OVERLAP))
    all_relative_fr(wam, pdata,1);
#endif
  for (i=0; i<pdata->ntargets; i++) {
    int object = (int)TARGET(i);
    Dvar sid_dv = O_SID_DVAR(object);
    SP_integer sid;

    O_FLAGS(object) |= 0x3;	/* possibly pruned */
    if (pdata->fixorder[fixix]<0)
      sid = dvar_min_l(sid_dv);
    else
      sid = dvar_max_l(sid_dv);
    dvar_fix_value_l(sid_dv,sid);
				/* [MC] 20131216 -- was forgotten */
    if (pdata->opt & OPT_VOLUME) /* o.sid was pruned */
      compute_minmax_vol_sid(wam, pdata,object);
    if (!fix_object_for_shape(wam, pdata,object,sid,fixix))
      return FALSE;
#if EXPLICIT_FBOXES
    if (!(pdata->opt & OPT_OVERLAP))
      relative_fr(wam, pdata,object,&pdata->fr.end_relative,relative_cont);
    pdata->fr.end_absolute = pdata->fr.end_relative;
    pdata->fr.top_of_stack = pdata->fr.end_relative;
#endif
    fixix += kdims+1;
    if (fixix>=pdata->norder)
      fixix = 0;
  }
  if ((pdata->opt & OPT_VOLUME) && !filter_volume(wam, pdata))
    return FALSE;
  return TRUE;
}


static int
filter_object(Wam wam,
	      struct geost_data *pdata,
	      int object)
{
  int kdims = pdata->kdims;
  int rc = 0;
  int dim;
  Dvar sid_dv = O_SID_DVAR(object);
    
  if (dvar_is_integer(sid_dv)) {
    SP_integer sid = dvar_min_l(sid_dv);
    switch (filter_object_for_shape(wam, pdata,object,sid,1)) {
    case -1:
      rc |= 2;
    case 0:
      break;
    default:
      rc |= 1;
    }
  } else {
    DVITER it;
    int virtual = pdata->nobjects;
    TAGGED sid_set = dvar_set(sid_dv);
    for (dim=0; dim<kdims; dim++) {
      pdata->virtual_min[dim] = dvar_max_l(O_ORIG_DVAR(object,dim));
      pdata->virtual_max[dim] = dvar_min_l(O_ORIG_DVAR(object,dim));
    }
    dviter_init(&it, sid_dv);
    while (!dviter_empty(&it)) {
      SP_integer sid = dviter_next_value_l(&it);
      object_to_virtual(pdata,object,sid);
      if (filter_object_for_shape(wam, pdata,virtual,sid,0) < 0) {
	sid_set = fd_delete(wam,sid_set,MakeSmall(sid));
	rc |= 1;
      } else {
	for (dim=0; dim<kdims; dim++) {
	  if (pdata->virtual_min[dim] > dvar_min_l(O_ORIG_DVAR(virtual,dim)))
	    pdata->virtual_min[dim] = dvar_min_l(O_ORIG_DVAR(virtual,dim));
	  if (pdata->virtual_max[dim] < dvar_max_l(O_ORIG_DVAR(virtual,dim)))
	    pdata->virtual_max[dim] = dvar_max_l(O_ORIG_DVAR(virtual,dim));
	}
      }
    }
    switch (dvar_fix_set(sid_dv,sid_set)) {
    case -1:
      rc |= 2;
    case 0:
      break;
    default:
      rc |= 1;
      O_FLAGS(object) |= 0x3;	/* pruned, queued */ 
      if (pdata->opt & OPT_VOLUME) /* o.sid was pruned */
	compute_minmax_vol_sid(wam, pdata,object);
    }
    if (rc<2)
      switch (virtual_to_object(pdata,object)) {
      case -1:
	rc |= 2;
      case 0:
	break;
      default:
	rc |= 1;
      }
  }
  return rc;
}

/* precond: OPT_OVERLAP implies OPT_LEX|OPT_RULES|OPT_PALLET_LOADING.
   We will be called in a fixpoint loop.
 */
static int
filter_geost(Wam wam,
	     struct geost_data *pdata,
	     int ntodo)
{
  int i, rc = 0;

#if EXPLICIT_FBOXES
  if (!(pdata->opt & (OPT_OVERLAP)))
    all_relative_fr(wam, pdata,0);
#endif
  for (i=0; i<ntodo && rc<2; i++) {
    int object = (int)TARGET(i);

    rc |= filter_object(wam, pdata,object);
  }
  return rc;
}

static void
shape_volume(Wam wam,
		   struct geost_data *pdata,
		   int ix)
{
  int kdims = pdata->kdims;
  SP_integer sid = S_SID(ix);
  int sbox0 = get_first_sbox(sid);
  SP_integer ivol, tvol = 0;
  int s, j;

  ivol = intersection_volume(wam, pdata,-1,-1,sid);
  for (s=sbox0; S_SID(s)==sid; s++) {
    SP_integer svol = 1;
    for (j=0; j<kdims; j++)
      svol *= S_SIZE(s,j);
    S_VOLUME(s) = svol;
    tvol += svol;
  }
  SP_ASSERT(pdata->overflow || ivol==tvol);
  SHAPE_VOLUME(ix) = ivol;
}

#if HOLE_METHOD

/* Sort by decreasing area. */

static int
cmp_decr_area(Wam wam,
	      SP_integer *t1,
	      SP_integer *t2)
{
  struct geost_data *pdata = fd.gdata;
  int kdims = pdata->kdims;
  Dvar dvs1 = O_SID_DVAR(*t1);
  SP_integer sid1 = dvar_min_l(dvs1);
  int sbox1 = get_first_sbox(sid1);
  SP_integer nvol1 = -SHAPE_VOLUME(sbox1);
  Dvar dvs2 = O_SID_DVAR(*t2);
  SP_integer sid2 = dvar_min_l(dvs2);
  int sbox2 = get_first_sbox(sid2);
  SP_integer nvol2 = -SHAPE_VOLUME(sbox2);

  return CMP(nvol1,nvol2);
}

#define QType SP_integer
#define QCmp  cmp_decr_area
#define QSort qsort_decr_area
#include "qsort.ic"

#if LONGEST_HOLE

/* Compute:
   lh_tables[0][h-1] : max. length of a hole of height h
   lh_tables[1][l-1] : max. height of a hole of length l
 */
static SP_BOOL
lh_create(Wam wam, struct geost_data *pdata, SP_integer sigma)
{
  int kdims = pdata->kdims;
  int i, d, s, nbtask=0;
  struct lhs *lhs;
  TAGGED *top;
  SP_BOOL rc = TRUE;
  SP_integer nrows = (pdata->opt & OPT_LONGEST_HOLE_ALL) ? sigma+1 : 1;
  SP_integer sigma0 = (pdata->opt & OPT_LONGEST_HOLE_ALL) ? 0 : sigma;
  SP_integer bbvolume = 1;
  SP_integer row;

  SP_ASSERT(fd.profile_pool==NULL);
  NumstackAlloc(0,top);

  for (d=0; d<kdims; d++) {
    SP_integer bblength = FR_END(/*bounding box*/1,d) - FR_ORIGIN(/*bounding box*/1,d) + 1;
    bbvolume *= bblength;
  }
  for (d=0; d<kdims; d++) {
    SP_integer bblength = FR_END(/*bounding box*/1,d) - FR_ORIGIN(/*bounding box*/1,d) + 1;
    SP_integer bbheight = bbvolume/bblength;
    SP_integer tabsize = nrows*bbheight;
    pdata->maxeps[d] = bbheight;
    pdata->lch_tables[d] = Malloc(tabsize, SP_integer);
    pdata->lh_tables[d] = Malloc(tabsize, SP_integer);
    pdata->lh_tables_max[d] = Malloc(tabsize, SP_integer);
    if (d==0)
      for (i=0; i<pdata->ntargets; i++) { /* for all objects */
	int object = (int)TARGET(i);
	if (decomposable(pdata,object)) {
	  SP_integer sid = dvar_min_l(O_SID_DVAR(object));
	  int sbox0 = get_first_sbox(sid);
	  for (s=sbox0; S_SID(s)==sid; s++) {
	    nbtask++;
	  }
	}
      }
  }
  lhs = Malloc(nbtask, struct lhs);
  for (d=0; d<kdims && rc; d++) {
    SP_integer bblength = FR_END(/*bounding box*/1,d) - FR_ORIGIN(/*bounding box*/1,d) + 1;
    SP_integer bbheight = pdata->maxeps[d];
    
    nbtask = 0;
    for (i=0; i<pdata->ntargets; i++) { /* for all objects */
      int object = (int)TARGET(i);
      if (decomposable(pdata,object)) {
	SP_integer sid = dvar_min_l(O_SID_DVAR(object));
	int sbox0 = get_first_sbox(sid);
	for (s=sbox0; S_SID(s)==sid; s++) {
	  lhs[nbtask].l = S_SIZE(s,d);
	  lhs[nbtask].h = S_VOLUME(s)/lhs[nbtask].l;
	  nbtask++;
	}
      }
    }
    for (row=0; row<nrows && rc; row++) {
      SP_integer offset = row*bbheight;
      SP_integer max = 0;

      fd_lh_init(wam,
		 sigma0,
		 (int)row,
		 (int)nrows,
		 bblength,
		 bbheight,
		 nbtask,
		 lhs,
		 pdata->lh_tables[d],
		 pdata->lch_tables[d],
		 (int)pdata->maxbacks,
		 2);
      if (row+1==nrows) {
	if (pdata->lch_tables[d][offset+bbheight-1]<bblength)
	  rc = FALSE;		/* 20080819 rule 2 */
	for (i=0; i<nbtask && rc; i++) {
	  SP_integer l = lhs[i].l;
	  SP_integer h = lhs[i].h;
	  if (h<bbheight && pdata->lh_tables[d][offset+bbheight-h-1]<l)
	    rc = FALSE;		/* 20080819 rule 3 */
	}
      }
      for (i=0; i<bbheight; i++) {
	max = (pdata->lh_tables[d][offset+i] > max 
	       ? pdata->lh_tables[d][offset+i] : max);
	pdata->lh_tables_max[d][offset+i] = max;
      }
    }
  }
  
  Free(lhs);
  numstack_trim(w,top);
  fd_init_profile(wam);		/* profile pool was empty before */
  return rc;
}
#endif

static int
init_avoid_holes_border(Wam wam, struct geost_data *pdata, int leftp)
{
  int kdims = pdata->kdims;
  int d, i, s, rc=0;
  
  for (d=0; d<kdims; d++) {
    SP_integer size = FR_END(/*bounding box*/1,d) - FR_ORIGIN(/*bounding box*/1,d) + 1;
    SP_integer lsmall, orig;
    int osmall=0, ssmall=0, count=0;
  saturate:
    lsmall = size+1;
    count = 0;
    for (i=0; i<pdata->nobjects; i++) {
      int object = (int)TARGET(i);
      Dvar dvs = O_SID_DVAR(object);
      SP_integer sid = dvar_min_l(dvs);
      int sbox0 = get_first_sbox(sid);
      Dvar dv  = O_ORIG_DVAR(object,0);
      
      for (s=sbox0; S_SID(s)==sid; s++) {
	orig = leftp ? FR_ORIGIN(/*bounding box*/1,d) - S_TRANSLATION(s,d) :
	               FR_END(/*bounding box*/1,d) + 1 - S_SIZE(s,d) - S_TRANSLATION(s,d);
	if (dvar_contains_value_l(dv+d,orig)) {
	  SP_integer len = S_SIZE(s,d);
	  if (len<lsmall) {
	    lsmall = len;
	    osmall = object;
	    ssmall = s;
	    count = 1;
	  } else if (len==lsmall) {
	    count++;
	  }
	}
      }
    }
    if (count==1 && lsmall<size) {
      SP_integer hsum = 0;
      SP_integer hsmall = S_VOLUME(ssmall)/S_SIZE(ssmall,d);
      for (i=0; i<pdata->nobjects; i++) {
	int object = (int)TARGET(i);
	Dvar dvs = O_SID_DVAR(object);
	SP_integer sid = dvar_min_l(dvs);
	int sbox0 = get_first_sbox(sid);
	Dvar dv  = O_ORIG_DVAR(object,0);
	int s;
	  
	for (s=sbox0; S_SID(s)==sid; s++) {
	  if (object!=osmall || s!=ssmall) {
	    orig = leftp ? FR_ORIGIN(/*bounding box*/1,d) - S_TRANSLATION(s,d) + lsmall :
	      FR_END(/*bounding box*/1,d) + 1 - lsmall - S_SIZE(s,d) - S_TRANSLATION(s,d);
	    if (dvar_contains_value_l(dv+d,orig)) {
	      SP_integer h = S_VOLUME(s)/S_SIZE(s,d);
	      if (h<=hsmall) {
		hsum += h;
		if (hsum>=hsmall)
		  goto cont;
	      }
	    }
	  }
	}
      }
      orig = leftp ? FR_ORIGIN(/*bounding box*/1,d) - S_TRANSLATION(ssmall,d) :
	             FR_END(/*bounding box*/1,d) + 1 - S_SIZE(ssmall,d) - S_TRANSLATION(ssmall,d);
      rc |= dvar_prune_value_l(O_ORIG_DVAR(osmall,d),orig);
      if (rc<0)
	return rc;
      goto saturate;
    }
  cont: ;
  }
  return rc;
}


static SP_integer
get_minloss(Wam wam,
		  struct geost_data *pdata,
		  SP_integer len,
		  SP_integer low,
		  SP_integer up,
		  SP_integer eps,
		  SP_integer sigma,
		  int closed,
		  int dim)
{
  SP_integer lhlow = get_longest_hole(wam, pdata,dim,low,eps,closed);
  SP_integer lhup =  get_longest_hole(wam, pdata,dim,up,eps,closed);
  if (lhup<len) {
    return sigma+1;
  } else if (lhlow<len) {
    while (low<up) { /* compute low = smallest loss for this interval 
			that does not violate longest hole table */
      SP_integer mid = (low+up)>>1;
      if (get_longest_hole(wam, pdata,dim,mid,eps,closed)<len) {
	low = mid+1;
      } else {
	up = mid;
      }
    }
  }
  return low;
}

static SP_integer
get_max_penetration(Wam wam,
			  struct geost_data *pdata,
			  SP_integer size_interval,
			  SP_integer first_val,
			  SP_integer last_val,
			  SP_integer len,
			  SP_integer hmax,
			  int object,
			  int sbox,
			  int dim)
{
  /* SP_ASSERT(size_interval==last_val-first_val+1); */
  if (len > size_interval)
    return 0;
#if LONGEST_HOLE
  if (pdata->opt & OPT_LONGEST_HOLE) {
    int kdims = pdata->kdims;
    SP_integer trans = S_TRANSLATION(sbox,dim);
    SP_integer bblength = FR_END(/*bounding box*/1,dim) - 
                    FR_ORIGIN(/*bounding box*/1,dim) + 1;
    SP_integer bbheight = FR_END(/*bounding box*/1,dim^1) - 
                    FR_ORIGIN(/*bounding box*/1,dim^1) + 1;
    SP_integer sigma_size_interval = pdata->slack - get_minloss(wam, pdata,bblength-size_interval,0,
							  pdata->slack,bbheight,
							  pdata->slack,TRUE,dim);
    SP_integer val, limit=last_val-len+1, hbest=0;
    Dvar dv = O_ORIG_DVAR(object,0);
    DVITER it;

    if (len < size_interval) {
      SP_integer hmax1 = get_longest_hole_max(wam, pdata,dim^1,sigma_size_interval,size_interval-len);
      hmax = hmax < hmax1 ? hmax : hmax1;
    }
    dviter_init(&it, dv+dim);
    dviter_skip_l(&it, first_val-trans-1);
    while (!dviter_empty(&it)) {
      val = dviter_next_value_l(&it)+trans;
      if (val>limit || hbest>=hmax) {
	break;
      } else {
	SP_integer e1 = val - first_val;
	SP_integer e2 = size_interval - len - e1;
	SP_integer h1 = e1==0 ? hmax : get_longest_hole(wam, pdata,dim^1,sigma_size_interval,e1,FALSE);
	SP_integer h2 = e2==0 ? hmax : get_longest_hole(wam, pdata,dim^1,sigma_size_interval,e2,FALSE);
	if (h1>h2)
	  h1 = h2;
	if (hbest<h1)
	  hbest = h1;
      }
    }
    return hbest < hmax ? hbest : hmax;
  }
#else
  (void)pdata;
  (void)first_val;
  (void)last_val;
  (void)object;
  (void)sbox;
  (void)dim;
#endif
  return hmax;
}


static int
compute_max_surf_in_gap_before_task(Wam wam,
				    struct geost_data *pdata,
				    SP_integer first_val,
				    SP_integer last_val,
				    int dim,
				    int object,
				    int s,
				    SP_integer e,
				    SP_integer val1,
				    SP_integer val2,
				    SP_integer surf_in_gap)
{
  int kdims = pdata->kdims;
  int cim = dim^1;
  SP_integer heights = S_SIZE(s,cim);
  SP_integer max_surf_in_gap = pdata->slack;
  SP_integer max_surf_disj1 = 0;
  SP_integer max_surf_disj2 = 0;
  int rc = 0;
  int i, s1;

  for (i=0; i<pdata->nobjects; i++) {
    int object1 = (int)pdata->byarea[i];
    Dvar dvs = O_SID_DVAR(object1);
    SP_integer sid = dvar_min_l(dvs);
    int sbox0 = get_first_sbox(sid);
      
    for (s1=sbox0; S_SID(s1)==sid; s1++) {
      if (object1!=object || s1!=s) {
	SP_integer lens1 = S_SIZE(s1,dim);
	SP_integer heights1 = S_SIZE(s1,cim);
	if (lens1<=e) {
	  SP_integer h, surf;
	  if (first_val<=last_val) {
	    h = get_max_penetration(wam, pdata,e,first_val,last_val,lens1,
				    heights1 < heights ? heights1 : heights,
				    object1,s1,dim);
	  } else {
	    h = heights1;
#if LONGEST_HOLE
	    if (pdata->opt & OPT_LONGEST_HOLE) {
	      SP_integer bblength = FR_END(/*bounding box*/1,dim) - 
		              FR_ORIGIN(/*bounding box*/1,dim) + 1;
	      SP_integer bbheight = FR_END(/*bounding box*/1,dim^1) - 
		              FR_ORIGIN(/*bounding box*/1,dim^1) + 1;
	      SP_integer sigma_e = pdata->slack -
		             get_minloss(wam, pdata,bblength-e,0,
					 pdata->slack,bbheight,
					 pdata->slack,TRUE,dim);
	      SP_integer hmax = lens1==e ? heights : 
		          get_longest_hole_max(wam, pdata,cim,sigma_e,e-lens1);
	      h = heights1<hmax ? heights1 : hmax;
	    }
#endif
	  }
	  surf = lens1*h;
	  if (lens1>e/2 && h!=heights1) {
	    if (surf>max_surf_disj1) {
	      max_surf_disj2 = max_surf_disj1;
	      max_surf_disj1 = surf;
	    } else if (surf>max_surf_disj2) {
	      max_surf_disj2 = surf;
	    }
	  } else {
	    max_surf_in_gap += surf;
	  }
	  if (max_surf_in_gap+max_surf_disj1+max_surf_disj2 >= surf_in_gap)
	    goto ret;
	}
      }
    }
  }
  if (max_surf_in_gap+max_surf_disj1+max_surf_disj2 < surf_in_gap) {
    Dvar dv  = O_ORIG_DVAR(object,0);
    rc |= dvar_prune_value_l(dv+dim,val1);
    rc |= dvar_prune_value_l(dv+dim,val2);
  }
 ret:
  return rc;
}

static int
compute_min_surf_used_on_top_of_task(Wam wam,
				     struct geost_data *pdata,
				     SP_integer first_val1,
				     SP_integer last_val1,
				     SP_integer first_val2,
				     SP_integer last_val2,
#if PARCONFLICT_METHOD
				     SP_integer *sizes,
				     SP_integer *earliest,
				     SP_integer *latest,
				     int sizrowlen,
#endif
				     int dim,
				     int object,
				     int s,
				     SP_integer e1,
				     SP_integer e2,
				     SP_integer e3,
				     SP_integer val1,
				     SP_integer val2,
				     SP_integer surf_on_top)
{
  int kdims = pdata->kdims;
  int cim = dim^1;
  SP_integer heights = S_SIZE(s,cim);
  Dvar dv  = O_ORIG_DVAR(object,0);
  SP_integer min_surf_on_top = 0;
  SP_integer s0=0, s1=0, s2=0, s3=0, s4=0;
  SP_integer maxe1e2 = e1 > e2 ? e1 : e2;
  int rc = 0;
  int i, sh1;
#if PARCONFLICT_METHOD
  int ix=0;
#else
  (void)e3;
#endif

  for (i=0; i<pdata->nobjects; i++) {
    int object1 = (int)pdata->byarea[i];
    Dvar dvs = O_SID_DVAR(object1);
    SP_integer sid = dvar_min_l(dvs);
    int sbox0 = get_first_sbox(sid);
    
    for (sh1=sbox0; S_SID(sh1)==sid; sh1++) {
      if (object1!=object || sh1!=s) {
	SP_integer lens1 = S_SIZE(sh1,dim);
	SP_integer heights1 = S_SIZE(sh1,cim);
	SP_integer h=0, surf;
	if (lens1>maxe1e2) {
	  min_surf_on_top += lens1*heights1;
	} else {
	  if (first_val1<=last_val1) {
	    SP_integer h1 = get_max_penetration(wam, pdata,e1,first_val1,last_val1,lens1,
					  heights1 < heights ? heights1 : heights,
					  object1,sh1,dim);
	    SP_integer h2 = get_max_penetration(wam, pdata,e2,first_val2,last_val2,lens1,
					  heights1 < heights ? heights1 : heights,
					  object1,sh1,dim);
	    h = h1 > h2 ? h1 : h2;
	  } else {
	    h = heights1;
#if LONGEST_HOLE
	    if (pdata->opt & OPT_LONGEST_HOLE) {

	      SP_integer bblength = FR_END(/*bounding box*/1,dim) -
		              FR_ORIGIN(/*bounding box*/1,dim) + 1;
	      SP_integer bbheight = FR_END(/*bounding box*/1,dim^1) - 
		              FR_ORIGIN(/*bounding box*/1,dim^1) + 1;
	      SP_integer sigma_e = pdata->slack-get_minloss(wam, pdata,bblength-maxe1e2,0,
						      pdata->slack,bbheight,
						      pdata->slack,TRUE,dim);
	      SP_integer hmax = (lens1==e1 || lens1==e2) ? heights : 
		          get_longest_hole_max(wam, pdata,cim,sigma_e,maxe1e2-lens1);
	      h = heights1<hmax ? heights1 : hmax;
	    }
#endif
	  }
	  surf = lens1*h;
	  if (lens1>maxe1e2/2 && h!=heights1) {
	    s0 += lens1*heights1;
	    if (surf>s1) {
	      s4=s3; s3=s2; s2=s1; s1=surf;
	    } else if (surf>s2) {
	      s4=s3; s3=s2; s2=surf;
	    } else if (surf>s3) {
	      s4=s3; s3=surf;
	    } else if (surf>s4) {
	      s4=surf;
	    }
	  } else {
	    min_surf_on_top += lens1*(heights1-h);
	  }
	}
	if (min_surf_on_top+s0-s1-s2-s3-s4 > surf_on_top) {
	  /* fprintf(stderr, "VISAVIS_INIT middle: %ld not in obj %d dim %d\n", val1, object, dim); */
	  rc |= dvar_prune_value_l(dv+dim,val1);
	  /* fprintf(stderr, "VISAVIS_INIT middle: %ld not in obj %d dim %d\n", val2, object, dim); */
	  rc |= dvar_prune_value_l(dv+dim,val2);
	  goto ret;
	}
#if PARCONFLICT_METHOD
	if (pdata->opt & OPT_PARCONFLICT) {
	  if (heights1>h) {
	    AREF2(sizes,0,ix,sizrowlen) = lens1;
	    AREF2(sizes,1,ix,sizrowlen) = heights1-h;
	    AREF2(earliest,0,ix,sizrowlen) = 1;
	    AREF2(earliest,1,ix,sizrowlen) = 1;
	    AREF2(latest,0,ix,sizrowlen) = FR_END(/*bounding box*/1,dim) - 
	                                   FR_ORIGIN(/*bounding box*/1,dim) + 1;
	    AREF2(latest,1,ix,sizrowlen) = e3;
	    ix++;
	  }
	}
#endif
      }
    }
  }
#if PARCONFLICT_METHOD
  if (pdata->opt & OPT_PARCONFLICT) {
    if (parallel_conflict(wam,2,ix,TRUE,sizes,earliest,latest,sizrowlen,
			  &pdata->size_mem[1])) {
      rc |= dvar_prune_value_l(dv+dim,val1);
      rc |= dvar_prune_value_l(dv+dim,val2);
    }
  }
#endif
 ret:
  return rc;
}

static int
init_avoid_holes_middle_mats(Wam wam,
			     struct geost_data *pdata,
			     int dim,
			     int object,
			     int s,
			     SP_integer val1,
			     SP_integer val2)
{
  int kdims = pdata->kdims;
  Dvar dv = O_ORIG_DVAR(object,0);
  int i, s1, last;
  SP_integer height = S_VOLUME(s)/S_SIZE(s,dim);
  SP_integer heights = height-pdata->slack;
  int rc = 0;

  if (heights<=0)
    return rc;
  for (last=0; last<2 && rc>=0; last++) {
    SP_integer val = last ? val2 : val1;
    if (dvar_contains_value_l(dv+dim,val)) {
      SP_integer suml=0, sumr=0, summ=0, borrowl, borrowr;
      SP_integer start = val + S_TRANSLATION(s,dim);
      SP_integer end = start + S_SIZE(s,dim);

      for (i=0; i<pdata->nobjects; i++) {
	int object1 = (int)pdata->byarea[i];
	Dvar dvs = O_SID_DVAR(object1);
	Dvar dv1 = O_ORIG_DVAR(object1,0);
	SP_integer sid = dvar_min_l(dvs);
	int sbox0 = get_first_sbox(sid);

	for (s1=sbox0; S_SID(s1)==sid; s1++) {
	  if (object!=object1 || s!=s1) {
	    SP_integer height1 = S_VOLUME(s1)/S_SIZE(s1,dim);

	    switch (2*dvar_contains_value_l(dv1+dim,start-S_SIZE(s1,dim)-S_TRANSLATION(s1,dim)) +
		      dvar_contains_value_l(dv1+dim,end-S_TRANSLATION(s1,dim))) {
	    case 1:
	      sumr += height1;
	      break;
	    case 2:
	      suml += height1;
	      break;
	    case 3:
	      summ += height1;
	      break;
	    }
	    borrowl = heights < suml ? 0 : heights-suml;
	    borrowr = heights < sumr ? 0 : heights-sumr;
	    if (borrowl+borrowr<=summ && suml+sumr+summ+pdata->slack>=2*height)
	      goto nextval;
	  }
	}
      }
      rc |= dvar_prune_value_l(dv+dim,val);
    }
  nextval: ;
  }
  return rc;
}

static int
init_avoid_holes_middle_cheap(Wam wam,
			      struct geost_data *pdata,
#if PARCONFLICT_METHOD
			      SP_integer *sizes,
			      SP_integer *earliest,
			      SP_integer *latest,
			      int sizrowlen,
#endif
			      int dim,
			      int object,
			      int s,
			      SP_integer e1,
			      SP_integer e2,
			      SP_integer e3,
			      SP_integer val1,
			      SP_integer val2,
			      SP_integer surf_in_gap1,
			      SP_integer surf_in_gap2,
			      SP_integer surf_on_top)
{
  int kdims = pdata->kdims;
  int rc = 0;
  Dvar dv = O_ORIG_DVAR(object,0);
  
  if (dvar_contains_value_l(dv+dim,val1) || dvar_contains_value_l(dv+dim,val2)) {
    rc |= compute_max_surf_in_gap_before_task(wam, pdata,0,-1,dim,object,s,e1,val1,val2,
					      surf_in_gap1);
    if (rc<0)
      goto ret;
  }
  if (dvar_contains_value_l(dv+dim,val1) && e1!=e2) {
    rc |= compute_max_surf_in_gap_before_task(wam, pdata,0,-1,dim,object,s,e2,val1,val2,
					      surf_in_gap2);
    if (rc<0)
      goto ret;
  }
  if (dvar_contains_value_l(dv+dim,val1)) {
#if PARCONFLICT_METHOD
    rc |= compute_min_surf_used_on_top_of_task(wam, pdata,0,-1,0,-1,
					       sizes, earliest, latest, sizrowlen,
					       dim,object,s,e1,e2,e3,val1,val2,
					       surf_on_top);
#else
    rc |= compute_min_surf_used_on_top_of_task(wam, pdata,0,-1,0,-1,
					       dim,object,s,e1,e2,e3,val1,val2,
					       surf_on_top);
#endif
    if (rc<0)
      goto ret;
  }
 ret:
  return rc;
}

static int
init_avoid_holes_middle_heavy(Wam wam,
			      struct geost_data *pdata,
#if PARCONFLICT_METHOD
			      SP_integer *sizes,
			      SP_integer *earliest,
			      SP_integer *latest,
			      int sizrowlen,
#endif
			      int dim,
			      int object,
			      int s,
			      SP_integer e1,
			      SP_integer e2,
			      SP_integer e3,
			      SP_integer val,
			      SP_integer surf_in_gap1,
			      SP_integer surf_in_gap2,
			      SP_integer surf_on_top)
{
  int kdims = pdata->kdims;
  int rc = 0;
  Dvar dv = O_ORIG_DVAR(object,0);
  
  if (dvar_contains_value_l(dv+dim,val)) {
    SP_integer first_val = FR_ORIGIN(/*bounding box*/1,dim);
    SP_integer last_val  = first_val + e1 - 1;
    rc |= compute_max_surf_in_gap_before_task(wam, pdata,first_val,last_val,dim,object,
					      s,e1,val,val,surf_in_gap1);
    if (rc<0)
      goto ret;
  }
  if (dvar_contains_value_l(dv+dim,val)) {
    SP_integer first_val = FR_ORIGIN(/*bounding box*/1,dim) + e1 + S_SIZE(s,dim);
    SP_integer last_val  = first_val + e2 - 1;
    rc |= compute_max_surf_in_gap_before_task(wam, pdata,first_val,last_val,dim,object,
					      s,e2,val,val,surf_in_gap2);
    if (rc<0)
      goto ret;
  }
  if (dvar_contains_value_l(dv+dim,val)) {
    SP_integer first_val1 = FR_ORIGIN(/*bounding box*/1,dim);
    SP_integer last_val1  = first_val1 + e1 - 1;
    SP_integer first_val2 = FR_ORIGIN(/*bounding box*/1,dim) + e1 + S_SIZE(s,dim);
    SP_integer last_val2  = first_val2 + e2 - 1;
#if PARCONFLICT_METHOD
    rc |= compute_min_surf_used_on_top_of_task(wam, pdata,first_val1,last_val1,
					       first_val2,last_val2,
					       sizes,earliest,latest,sizrowlen,
					       dim,object,s,e1,e2,e3,val,val,surf_on_top);
#else
    rc |= compute_min_surf_used_on_top_of_task(wam, pdata,first_val1,last_val1,
					       first_val2,last_val2,
					       dim,object,s,e1,e2,e3,val,val,surf_on_top);
#endif
    if (rc<0)
      goto ret;
  }
 ret:
  return rc;
}

static int
init_avoid_holes_middle(Wam wam, struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int dim, i, s;
  int rc=0;
#if PARCONFLICT_METHOD
  int sizrowlen = pdata->nobjects*pdata->max_nsboxes;
  SP_integer *sizes = PermAlloc(6*sizrowlen,SP_integer,&pdata->size_mem[0]);
  SP_integer *earliest = sizes + 2*sizrowlen;
  SP_integer *latest = sizes + 4*sizrowlen;
#endif
  
  for (i=0; i<pdata->nobjects; i++)
    pdata->byarea[i] = i;
  
  qsort_decr_area(wam, pdata->byarea,i);
  for (dim=0; dim<2; dim++) {
    int cim = dim^1;
    SP_integer size = FR_END(/*bounding box*/1,dim) - FR_ORIGIN(/*bounding box*/1,dim) + 1;
    
    for (i=0; i<pdata->nobjects; i++) {
      int object = (int)pdata->byarea[i];
      Dvar dvs = O_SID_DVAR(object);
      SP_integer sid = dvar_min_l(dvs);
      int sbox0 = get_first_sbox(sid);
      for (s=sbox0; S_SID(s)==sid; s++) {
	SP_integer lens = S_SIZE(s,dim);
	SP_integer heights = S_SIZE(s,cim);
	SP_integer e3 = FR_END(/*bounding box*/1,cim) - FR_ORIGIN(/*bounding box*/1,cim) + 1 - heights;
	SP_integer surf_on_top = e3*size;
	SP_integer e1max = (size-lens)/2;
	SP_integer e1;
	for (e1=1; e1<=e1max; e1++) {
	  SP_integer e2 = size-e1-lens;
	  SP_integer surf_in_gap1 = e1*heights;
	  SP_integer surf_in_gap2 = e2*heights;
	  SP_integer val1 = FR_ORIGIN(/*bounding box*/1,dim) + e1 - S_TRANSLATION(s,dim);
	  SP_integer val2 = val1 - e1 + e2;
	  rc |= init_avoid_holes_middle_mats(wam, pdata,dim,object,s,val1,val2);
	  if (rc<0)
	    goto ret;
#if PARCONFLICT_METHOD
	  rc |= init_avoid_holes_middle_cheap(wam, pdata,
					      sizes, earliest, latest, sizrowlen,
					      dim,object,s,e1,e2,e3,val1,val2,
					      surf_in_gap1,surf_in_gap2,surf_on_top);
	  if (rc<0)
	    goto ret;
	  rc |= init_avoid_holes_middle_heavy(wam, pdata,
					      sizes, earliest, latest, sizrowlen,
					      dim,object,s,e1,e2,e3,val1,
					      surf_in_gap1,surf_in_gap2,surf_on_top);
	  if (rc<0)
	    goto ret;
	  rc |= init_avoid_holes_middle_heavy(wam, pdata,
					      sizes, earliest, latest, sizrowlen,
					      dim,object,s,e2,e1,e3,val2,
					      surf_in_gap2,surf_in_gap1,surf_on_top);
	  if (rc<0)
	    goto ret;
#else
	  rc |= init_avoid_holes_middle_cheap(wam, pdata,
					      dim,object,s,e1,e2,e3,val1,val2,
					      surf_in_gap1,surf_in_gap2,surf_on_top);
	  if (rc<0)
	    goto ret;
	  rc |= init_avoid_holes_middle_heavy(wam, pdata,
					      dim,object,s,e1,e2,e3,val1,
					      surf_in_gap1,surf_in_gap2,surf_on_top);
	  if (rc<0)
	    goto ret;
	  rc |= init_avoid_holes_middle_heavy(wam, pdata,
					      dim,object,s,e2,e1,e3,val2,
					      surf_in_gap2,surf_in_gap1,surf_on_top);
	  if (rc<0)
	    goto ret;
#endif
	}
      }
    }
  }
 ret:
#if PARCONFLICT_METHOD
  PermFree(sizes,&pdata->size_mem[0]);
#endif
  return rc;
}

/* precondition: region 1 is the bounding box of all objects
 * precondition: pdata->slack>=0 is its slack */
static int
init_avoid_holes(Wam wam, struct geost_data *pdata)
{
  int kdims = pdata->kdims;
  int i, d, rc=0;
  int ndecomposable = 0;
  
  for (i=0; i<pdata->nobjects; i++) {
    int object = (int)TARGET(i);
    Dvar dvs = O_SID_DVAR(object);
    SP_integer sid = dvar_min_l(dvs);
    int sbox0 = get_first_sbox(sid);
    if (decomposable(pdata,object)) {
      Dvar dv  = O_ORIG_DVAR(object,0);
      int s;
      ndecomposable++;
      
      for (s=sbox0; S_SID(s)==sid; s++) {
	for (d=0; d<kdims; d++) {
	  SP_integer surface = S_VOLUME(s)/S_SIZE(s,d);
	  SP_integer gap_min = CEILDIV(pdata->slack+1,surface);
	  SP_integer vavsize[256];
	  int r0 = pdata->fr.end_bb;
	  int j;
	  SP_integer gap, orig, delta;
	  DVITER it;
	  
	  for (j=0; j<kdims; j++) {
	    FR_ORIGIN(r0,j) = FR_ORIGIN(/*bounding box*/1,j);
	    FR_END(r0,j) = FR_END(/*bounding box*/1,j);
	    vavsize[j] = S_SIZE(s,j);
	  }
	  delta = FR_ORIGIN(/*bounding box*/1,d) - S_TRANSLATION(s,d);
	  dviter_init(&it, dv+d);
	  dviter_skip_l(&it, gap_min-1+delta);
	  while (!dviter_empty(&it)) {
	    SP_integer vol, min, max;
	    orig = dviter_next_value_l(&it);
	    gap = orig-delta;
	    vol = surface*gap;
	    FR_ORIGIN(r0,d) = FR_ORIGIN(/*bounding box*/1,d);
	    FR_END(r0,d) = FR_ORIGIN(/*bounding box*/1,d) + gap - 1;
	    vavsize[d] = gap;
#if LONGEST_HOLE
	    if (pdata->opt & OPT_LONGEST_HOLE) {
	      if (get_longest_hole(wam, pdata,d^1,pdata->slack,gap,FALSE)<vavsize[d^1])
		goto prune1;
	    }
#endif
	    (void)visavis_fill(wam, pdata,r0,vavsize,d,object,s,FALSE,
			       &min,&max,vol);
	    if (vol<min || vol-max>pdata->slack) {
#if LONGEST_HOLE
	    prune1:
#endif
	      /* fprintf(stderr, "VISAVIS_INIT init1: %ld not in obj %d dim %d\n", orig, object, d); */
	      rc |= dvar_prune_value_l(dv+d,orig);
	      if (rc<0)
		goto ret;
	    }
	  }
	  delta = FR_END(/*bounding box*/1,d) - S_SIZE(s,d) - S_TRANSLATION(s,d) + 1;
	  dviter_init(&it, dv+d);
	  while (!dviter_empty(&it)) {
	    SP_integer vol, min, max;
	    orig = dviter_next_value_l(&it);
	    gap = delta-orig;
	    if (gap<gap_min)
	      break;
	    vol = surface*gap;
	    FR_ORIGIN(r0,d) = FR_END(/*bounding box*/1,d) - gap + 1;
	    FR_END(r0,d) = FR_END(/*bounding box*/1,d);
	    vavsize[d] = gap;
	    orig = delta-gap;
#if LONGEST_HOLE
	    if (pdata->opt & OPT_LONGEST_HOLE) {
	      if (get_longest_hole(wam, pdata,d^1,pdata->slack,gap,FALSE)<vavsize[d^1])
		goto prune2;
	    }
#endif
	    (void)visavis_fill(wam, pdata,r0,vavsize,d,object,s,FALSE,
			       &min,&max,vol);
	    if (vol<min || vol-max>pdata->slack) {
#if LONGEST_HOLE
	    prune2:
#endif
	      /* fprintf(stderr, "VISAVIS_INIT init2: %ld not in obj %d dim %d\n", orig, object, d); */
	      rc |= dvar_prune_value_l(dv+d,orig);
	      if (rc<0)
		goto ret;
	    }
	  }
	}
      }
    }
  }
  if (ndecomposable==pdata->nobjects) {
    if (pdata->slack==0) {
      rc |= init_avoid_holes_border(wam, pdata,TRUE);
      rc |= init_avoid_holes_border(wam, pdata,FALSE);
      if (rc<0)
	goto ret;
    }
    if (pdata->kdims==2) {
      rc |= init_avoid_holes_middle(wam, pdata);
      if (rc<0)
	goto ret;
    }
  }
 ret:
  return rc;
}
#endif


static void
init_object(Wam wam,
	    struct geost_data *pdata,
	    int object,
	    int init_bounds)
{
  int kdims = pdata->kdims;
  Dvar dv = O_SID_DVAR(object);
  int j;
  (void)wam;
  
  dvar_refresh(dv);
  dv = O_ORIG_DVAR(object,0);
  for (j=0; j<kdims; j++) {
    dvar_refresh(dv+j);
    if (init_bounds) {
      if (pdata->dvar_min[j] > dvar_min_l(dv+j))
	pdata->dvar_min[j] = dvar_min_l(dv+j);
      if (pdata->dvar_max[j] < dvar_max_l(dv+j))
	pdata->dvar_max[j] = dvar_max_l(dv+j);
    }
  }
}

static int 
geost_init(Wam wam, struct geost_data *pdata, int entry)
{
  SP_integer total_demand = 0;
  SP_integer decomposable_demand = 0;
  int i, kdims = pdata->kdims;

  /* Compute in region 0 the bounding box of all recently pruned objects, TEMPORARY */
  /* Compute in region 1 the bounding box of all objects not known to be ground,
     PERSISTENT during this iteration of the "loop". */
  pdata->fr.end_bb = FIRST_BB;
  bb_init(pdata,/*bounding box*/0); /* bounding box of recently queued objects */
  bb_init(pdata,/*bounding box*/1); /* bounding box of TARGET objects */
  for (i=0; i<pdata->ntargets; i++) {
    int object = (int)TARGET(i);
    bb_add_object(wam, pdata,/*bounding box*/1,object);
    if (entry && O_FLAGS(object) & 0x1) { /* queued? */
      bb_add_object(wam, pdata,/*bounding box*/0,object);
    }      
  }
  pdata->target_avail = 0;
  for (i=0; i<pdata->nobjects && !pdata->overflow; i++) {
    int object = (int)TARGET(i);
    SP_integer demand;
    if (fr_intersects_object(wam, pdata,/*bounding box*/1,object)) {
      if (i<pdata->ntargets) {
	demand = min_intersection_volume(wam, pdata,/*NO bounding box*/-1,object);
      } else {
	demand = min_intersection_volume(wam, pdata,/*bounding box*/1,object);
	pdata->target_avail -= demand;
      }
      total_demand += demand;
      if (decomposable(pdata,object))
	decomposable_demand += demand;
    }
  }
  pdata->exact_slack = 1;
  for (i=0; i<kdims; i++) {
    SP_integer fac = FR_END(/*bounding box*/1,i) - FR_ORIGIN(/*bounding box*/1,i) + 1;
    if (fac<=0)
      return FALSE;
    if (pdata->exact_slack < MAXSPINT/fac)
      pdata->exact_slack *= fac;
    else
      pdata->overflow = 1;
  }
  if (pdata->overflow) {
    pdata->opt &= ~(OPT_DISJUNCTIVE|
		    OPT_CUMULATIVE|
		    OPT_POLYMORPHISM|
		    OPT_PARCONFLICT|
		    OPT_VISAVIS_INIT|
		    OPT_VISAVIS|
		    OPT_CORNERS|
		    OPT_TASK_INTERVALS|
		    OPT_DP|
		    OPT_LONGEST_HOLE|
		    OPT_LONGEST_HOLE_ALL|
		    OPT_VISAVIS_FLOATING|
		    OPT_PALLET_LOADING);
  } else {			/* count volumes */
    pdata->target_avail += pdata->exact_slack;
    pdata->slack = pdata->exact_slack - decomposable_demand;
    pdata->exact_slack -= total_demand;
    if (!(pdata->opt&(OPT_OVERLAP)) && pdata->exact_slack<0)
      return FALSE;
  }
  if (pdata->tick++ == 0) {	/* ONCE only */
    if (pdata->opt & (OPT_OVERLAP))
      pdata->opt &= ~(OPT_DISJUNCTIVE|
		      OPT_CUMULATIVE|
		      OPT_POLYMORPHISM|
		      OPT_PARCONFLICT|
		      OPT_VISAVIS_INIT|
		      OPT_VISAVIS|
		      OPT_CORNERS|
		      OPT_TASK_INTERVALS|
		      OPT_DP|
		      OPT_LONGEST_HOLE|
		      OPT_LONGEST_HOLE_ALL|
		      OPT_VISAVIS_FLOATING|
		      OPT_PALLET_LOADING|
		      OPT_VOLUME);
    if (kdims!=2)
      pdata->opt &= ~(OPT_LONGEST_HOLE|
		      OPT_LONGEST_HOLE_ALL|
		      OPT_DISJUNCTIVE|
		      OPT_CORNERS);
#if LONGEST_HOLE
    if (pdata->slack!=pdata->exact_slack) /* ANY polymorphism renders longest hole tables invalid */
      pdata->opt &= ~(OPT_LONGEST_HOLE|OPT_LONGEST_HOLE_ALL);
    if (pdata->opt & OPT_LONGEST_HOLE) {
      if (!lh_create(wam, pdata,pdata->slack))
	return FALSE;
    } else {
      pdata->opt &= ~OPT_VISAVIS_FLOATING;
    }
#endif
#if HOLE_METHOD
    if (pdata->opt & OPT_VISAVIS_INIT) {
      if (init_avoid_holes(wam, pdata)<0) /* seems to reach fixpoint in one run */
	return FALSE;
    }
#endif
#if PALLET_LOADING
    if ((pdata->opt & OPT_PALLET_LOADING) && !pl_post(wam, pdata))
      return FALSE;
#endif
  }
#if PARCONFLICT_METHOD
  if (pdata->opt & OPT_PARCONFLICT) {
    if (!check_wrt_parallel_conflict(wam, pdata,pdata->tick==1))
      return FALSE;
  }
#endif
  return TRUE;
}

static DAEMON_RC SPCDECL 
geost_daemon(Wam wam,
	     void *vdata,
	     SP_globref attr_ref,
	     TAGGED *global)
{
  struct geost_data *pdata = (struct geost_data *)vdata;
  int kdims = pdata->kdims;
  TAGGED tstate, tmp;
  int incremental, ar, object, i, j;
  SP_integer state_stamp;
  DAEMON_RC rc = DAEMON_FIX;

  object = (int)((attr_ref - pdata->refbase)/REFS_PER_OBJECT);
  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  incremental = (pdata->stamp==state_stamp);
  if (!incremental) {
    pdata->check_cumulative_set_count = 0; /* prevent runaway memory use */
    pdata->stamp = state_stamp;
    DerefArg(tmp,tstate,5);	/* CTagToArg(X(0),5): NTargets */
    pdata->ntargets = GetSmall_int(tmp);
    for (i=0; i<pdata->ntargets; i++) {
      int obj = (int)TARGET(i);
      Dvar dv = O_ORIG_DVAR(obj,0);
      for (j=0; j<kdims; j++) {
	dvar_refresh(dv+j);
      }
      O_FLAGS(obj) &= ~0x3;	/* not queued, not pruned */
      if (pdata->opt & OPT_VOLUME) {
	Dvar dv = O_SID_DVAR(obj);
	dvar_refresh(dv);
	compute_minmax_vol_sid(wam, pdata,obj);
      }
    }
    pdata->prunedsid = 0;
    for (j=0; j<kdims; j++) {
      pdata->pruneddim[j] = 0;
    }
  }
  if (object<pdata->nobjects) {
    int d = (((int)(attr_ref - pdata->refbase) % REFS_PER_OBJECT)>>1);
    if (d==0) {			/* o.sid was pruned */
      pdata->prunedsid = 1;
      if (incremental && (pdata->opt & OPT_VOLUME)) {
	Dvar dv = O_SID_DVAR(object);
	dvar_refresh(dv);
	compute_minmax_vol_sid(wam, pdata,object);
      }
    } else {			/* o.x was pruned */
      for (j=0; j<kdims; j++) {
	if (j==d-1)
	  pdata->pruneddim[j] = 1;
      }
    }
    O_FLAGS(object) |= 0x1;	/* queued */
  } else {			/* touched a bound or the fixall flag */
    for (i=0; i<pdata->ntargets; i++) {
      O_FLAGS(TARGET(i)) |= 0x1; /* queued */
    }
  }
  tmp = CTagToArg(*global,3);
  if (!(RefMutable(tmp) & IStep(1))) {	/* STATUS: not enqueued */
    SP_BOOL buried;
    (void)fd_daemon_copy_state(wam,global,&buried);
    pdata->stamp++;
    rc = DAEMON_NOFIX;
  }
  return rc;
}

/*
   '$fd_geost'(+State0, -State, -Actions) :-
   State0 = State = f(1: Kdims,
		      2: NObjects,
		      3: Nsboxes,
		      4: MaxNSboxes,
		      5: NTargets,
		      6: Objects,
		      7: Sboxs,
		      8: FixAllFlagPair,
		      9: LBUBPair,
		      10: VolumePair,
		      11: Options,
		      12: FixOrder,
		      13: _Chains,
		      14: Maxbacks,
		      15: Procs,
		      16: NHoles,
		      17: RestVolume,
		      18: Handle,
		      19: Stamp
		      )
   Object = object(int Oid,int Sid,dvar Origin[])
   Sbox = sbox(int Sid,int Translation[],int Size[])
 */
void SPCDECL
prolog_fd_geost(Wam wam,
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  struct geost_data *pdata;
  int kdims, norder, change, i, j, rc;
  int ent = -1;			/* disentailed unless otherwise */
  int ntargets_at_entry;
  SP_BOOL committed;
  TAGGED handle, tmp;
  
/*    X(0) = RefTerm(State0); */
  (void)State0;
  fd_init_profile(wam);
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct geost_data,handle);
    kdims = pdata->kdims;
#if PALLET_LOADING
    DerefArg(tmp,X(0),16);	/* NHoles */
    pdata->pl.nholes = GetSmall_int(tmp);
#endif
    if (pdata->opt & OPT_VOLUME) {
      DerefArg(tmp,X(0),17);	/* RestVolume */
      pdata->rest_volume = GetSmall(tmp);
    }
  } else {			/* build persistent state */
    int nobjects, nsboxes_posted, nsboxes_allocated, max_nsboxes;
    SP_integer total_size;
    char *ptr;
    SP_globref ref;
    
    DerefArg(tmp,X(0),1);	/* KDims */
    kdims = GetSmall_int(tmp);
    DerefArg(tmp,X(0),2);	/* NObjects */
    nobjects = GetSmall_int(tmp);
    DerefArg(tmp,X(0),3);	/* Nsboxes */
    nsboxes_posted = GetSmall_int(tmp);
    nsboxes_allocated = nsboxes_posted+(nobjects > 1 ? nobjects : 1);
    if (nsboxes_allocated < 2*kdims)
      nsboxes_allocated = 2*kdims;
    DerefArg(tmp,X(0),4);	/* MaxNSboxes */
    max_nsboxes = GetSmall_int(tmp);
    DerefArg(tmp,X(0),12);	/* get Fixorder */
    norder = fd_list_length(tmp);
    total_size =
      norder*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      nobjects*sizeof(SP_integer) +
      nobjects*sizeof(SP_integer) +
      nobjects*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer *) +
      kdims*sizeof(SP_integer *) +
      kdims*sizeof(SP_integer *) +
      kdims*sizeof(struct sw_on_key *) +
      (nobjects*max_nsboxes+1)*sizeof(SP_integer) +
      max_nsboxes*sizeof(SP_integer) +
      (kdims+1)*(nobjects+2)*sizeof(struct dvar) +
      1*sizeof(struct dvar) +
      1*sizeof(struct dvar) +
      kdims*sizeof(struct dvar) +
      kdims*sizeof(struct dvar) +
      (nobjects+2)*sizeof(struct geost_proc *) +
      (nobjects+2)*sizeof(SP_integer) +
      (nobjects+2)*sizeof(SP_integer) +
      (nobjects+2)*sizeof(SP_integer) +
      (nobjects+2)*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      (nsboxes_allocated*kdims)*sizeof(SP_integer) +
      (nsboxes_allocated*kdims)*sizeof(SP_integer) +
      (nsboxes_allocated*kdims)*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      nsboxes_allocated*sizeof(SP_integer) +
      nsboxes_allocated*kdims*2*sizeof(SP_integer) +
#if PALLET_LOADING
      kdims*sizeof(SP_integer) +
      kdims*sizeof(SP_integer) +
#endif
      SWEEPSETNW(kdims)*sizeof(unsigned int) +
      SWEEPSETNW(kdims)*sizeof(unsigned int);
    fd.gdata = pdata = Palloc(struct geost_data, total_size, handle); /* GC, clobbers tmp */
    ptr = (char *)(pdata+1);
    pdata->fixorder = (SP_integer *)ptr;
    ptr += norder*sizeof(SP_integer);
    pdata->current_point = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->current = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->next = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->dvar_min = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->dvar_max = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->virtual_min = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->virtual_max = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->pruneddim = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->target = (SP_integer *)ptr;
    ptr += nobjects*sizeof(SP_integer);
    pdata->byclass = (SP_integer *)ptr;
    ptr += nobjects*sizeof(SP_integer);
    pdata->byarea = (SP_integer *)ptr;
    ptr += nobjects*sizeof(SP_integer);
    pdata->maxeps = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->lch_tables = (SP_integer **)ptr;
    ptr += kdims*sizeof(SP_integer *);
    pdata->lh_tables = (SP_integer **)ptr;
    ptr += kdims*sizeof(SP_integer *);
    pdata->lh_tables_max = (SP_integer **)ptr;
    ptr += kdims*sizeof(SP_integer *);
    pdata->dp_hash = (struct sw_on_key **)ptr;
    ptr += kdims*sizeof(struct sw_on_key *);
    pdata->check_cumulative_set = (SP_integer *)ptr;
    ptr += (nobjects*max_nsboxes+1)*sizeof(SP_integer);
    pdata->cumulative_sbox = (SP_integer *)ptr;
    ptr += max_nsboxes*sizeof(SP_integer);
    pdata->dvar = (Dvar)ptr;
    ptr += (kdims+1)*(nobjects+2)*sizeof(struct dvar);
    pdata->fix_dvar = (Dvar)ptr;
    ptr += 1*sizeof(struct dvar);
    pdata->volume_dvar = (Dvar)ptr;
    ptr += 1*sizeof(struct dvar);
    pdata->lower_dvar = (Dvar)ptr;
    ptr += kdims*sizeof(struct dvar);
    pdata->upper_dvar = (Dvar)ptr;
    ptr += kdims*sizeof(struct dvar);
    pdata->object.proc = (struct geost_proc **)ptr;
    ptr += (nobjects+2)*sizeof(struct geost_proc *);
    pdata->object.flags = (SP_integer *)ptr;
    ptr += (nobjects+2)*sizeof(SP_integer);
    pdata->object.oid = (SP_integer *)ptr;
    ptr += (nobjects+2)*sizeof(SP_integer);
    pdata->object.min_vol_sid = (SP_integer *)ptr;
    ptr += (nobjects+2)*sizeof(SP_integer);
    pdata->object.max_vol_sid = (SP_integer *)ptr;
    ptr += (nobjects+2)*sizeof(SP_integer);
    pdata->shape.sid = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.translation = (SP_integer *)ptr;
    ptr += (nsboxes_allocated*kdims)*sizeof(SP_integer);
    pdata->shape.size = (SP_integer *)ptr;
    ptr += (nsboxes_allocated*kdims)*sizeof(SP_integer);
    pdata->shape.size_multiset = (SP_integer *)ptr;
    ptr += (nsboxes_allocated*kdims)*sizeof(SP_integer);
    pdata->shape.max_size = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->shape.max_trans_size = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->shape.min_trans = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->shape.min = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.max = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.cur = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.useful = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.volume = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.svolume = (SP_integer *)ptr;
    ptr += nsboxes_allocated*sizeof(SP_integer);
    pdata->shape.boundaries = (SP_integer *)ptr;
    ptr += nsboxes_allocated*kdims*2*sizeof(SP_integer);
#if PALLET_LOADING
    pdata->pl.bborig = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->pl.bbend = (SP_integer *)ptr;
    ptr += kdims*sizeof(SP_integer);
    pdata->pl.pslack = -1;
    pdata->pl.nholes = 0;
    pdata->pl.holes = NULL;
    pdata->pl.islin = NULL;
#endif
    pdata->sweepset.min = (unsigned int *)ptr;
    ptr += SWEEPSETNW(kdims)*sizeof(unsigned int);
    pdata->sweepset.max = (unsigned int *)ptr;
    ptr += SWEEPSETNW(kdims)*sizeof(unsigned int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    pdata->prunedsid = 1;
    for (j=0; j<kdims; j++) {
      pdata->lch_tables[j] = NULL;
      pdata->lh_tables[j] = NULL;
      pdata->lh_tables_max[j] = NULL;
      pdata->pruneddim[j] = 1;
    }
    for (i=nobjects+1; i>=0; i--) {
      O_PROC(i) = NULL;
      O_FLAGS(i) = 0x3;		/* queued, pruned: force saving witness */
    }
    pdata->destructor = geost_destructor;
    pdata->daemon = geost_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = nobjects*REFS_PER_OBJECT + 4 + 4*kdims;
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    pdata->stamp = 0;
    pdata->tick = 0;
    pdata->kdims = kdims;
    pdata->nobjects = nobjects;
    pdata->ntargets = nobjects;
    pdata->nsboxes_posted = nsboxes_posted;
    pdata->nsboxes_allocated = nsboxes_allocated;
    pdata->max_nsboxes = max_nsboxes;
    pdata->check_cumulative_set_count = 0;
    pdata->overflow = 0;
    DerefArg(tmp,X(0),6);	/* get Objects */
    ref = pdata->refbase;
    for (i=0; i<nobjects; i++) {
      TAGGED t1, t2, t3;
      DerefCar(t1,tmp);
      DerefCdr(tmp,tmp);
      DerefArg(t2,t1,1);	/* get obj.oid */
      O_OID(i) = GetSmall(t2);
      DerefArg(t2,t1,2);	/* get obj.sid */
      fd_get_var_and_attr(t2,ref);
      ref += 2;
      DerefArg(t2,t1,3);	/* get obj.origin */
      for (j=0; j<kdims; j++) {
	DerefCar(t3,t2);
	DerefCdr(t2,t2);
	fd_get_var_and_attr(t3,ref);
	ref += 2;
      }
    }
    DerefArg(tmp,X(0),7);	/* get Sboxes */
    for (i=0; i<nsboxes_posted; i++) {
      TAGGED t1, t2, t3;
      DerefCar(t1,tmp);
      DerefCdr(tmp,tmp);
      DerefArg(t2,t1,1);	/* get shape.sid */
      S_SID(i) = GetSmall(t2);
      DerefArg(t2,t1,2);	/* get shape.translation */
      for (j=0; j<kdims; j++) {
	DerefCar(t3,t2);
	DerefCdr(t2,t2);
	S_TRANSLATION(i,j) = GetSmall(t3);
      }
      DerefArg(t2,t1,3);	/* get shape.size */
      for (j=0; j<kdims; j++) {
	SP_integer len;
	DerefCar(t3,t2);
	DerefCdr(t2,t2);
	S_SIZE(i,j) = len = GetSmall(t3);
	S_SIZE_MULTISET(i,j) = len;
      }
      fd_qsort_asc_long(wam, &S_SIZE_MULTISET(i,0),kdims);
    }
    for (j=0; j<kdims; j++) {
      SP_integer max_size = -CLPFD_MAXINT;
      SP_integer max_trans_size = -CLPFD_MAXINT;
      SP_integer min_trans = CLPFD_MAXINT;
      for (i=0; i<nsboxes_posted; i++) {
	if (max_size < S_SIZE(i,j))
	  max_size = S_SIZE(i,j);
	if (max_trans_size < S_TRANSLATION(i,j) + S_SIZE(i,j))
	  max_trans_size = S_TRANSLATION(i,j) + S_SIZE(i,j);
	if (min_trans > S_TRANSLATION(i,j))
	  min_trans = S_TRANSLATION(i,j);
      }
      S_MAX_SIZE(j) = max_size;
      S_MAX_TRANS_SIZE(j) = max_trans_size;
      S_MIN_TRANS(j) = min_trans;
      pdata->dp_hash[j] = NULL;
    }
    for (j=0; j<3; j++) {
      pdata->size_mem[j].size = 1024;
      pdata->size_mem[j].mem = SP_malloc(1024);
    }
    S_SID(nsboxes_posted) = CLPFD_MAXINT; /* for loop termination */
    init_first_sbox(wam, pdata);
    shape_volume(wam, pdata,0);
    for (i=1; i<nsboxes_posted; i++)
      if (S_SID(i-1)!=S_SID(i))
	shape_volume(wam, pdata,i);
    for (i=0; i<nobjects; i++) {
      TARGET(i) = i;
    }
    DerefArg(tmp,X(0),11);	/* get Options bitmask */
    pdata->opt = GetSmall_int(tmp);
    
    DerefArg(tmp,X(0),8);	/* get FixAll flag */
    ref = FIX_ATTR;
    fd_get_var_and_attr(tmp,ref);
    dvar_init(pdata->fix_dvar, ref, ref+1);
    dvar_attach_daemon(wam, pdata->fix_dvar, pdata, X(1), fd.functor_val);

    if (pdata->opt & OPT_VOLUME) {
      pdata->rest_volume = 0;
      DerefArg(tmp,X(0),10);	/* get Volume dvar */
      ref = VOLUME_ATTR;
      fd_get_var_and_attr(tmp,ref);
      dvar_init(pdata->volume_dvar, ref, ref+1);
      dvar_attach_daemon(wam, pdata->volume_dvar, pdata, X(1), fd.functor_minmax);
    }
    
    DerefArg(tmp,X(0),9);	/* get lower bound vector */
    DerefArg(tmp,tmp,1);
    ref = LOWER_ATTR;
    for (i=0; i<kdims; i++) {
      TAGGED t1;
      DerefCar(t1,tmp);
      DerefCdr(tmp,tmp);
      fd_get_var_and_attr(t1,ref);
      ref += 2;
    }
    ref = LOWER_ATTR;
    for (i=0; i<kdims; i++) {
      dvar_init(pdata->lower_dvar+i, ref, ref+1);
      dvar_attach_daemon(wam, pdata->lower_dvar+i, pdata, X(1), fd.functor_min); /* [MC] 4.2.3: can GC */
      ref += 2;
    }
    
    DerefArg(tmp,X(0),9);	/* get upper bound vector */
    DerefArg(tmp,tmp,2);
    ref = UPPER_ATTR;
    for (i=0; i<kdims; i++) {
      TAGGED t1;
      DerefCar(t1,tmp);
      DerefCdr(tmp,tmp);
      fd_get_var_and_attr(t1,ref);
      ref += 2;
    }
    ref = UPPER_ATTR;
    for (i=0; i<kdims; i++) {
      dvar_init(pdata->upper_dvar+i, ref, ref+1);
      dvar_attach_daemon(wam, pdata->upper_dvar+i, pdata, X(1), fd.functor_max); /* [MC] 4.2.3: can GC */
      ref += 2;
    }

    DerefArg(tmp,X(0),14);	/* get Maxbacks */
    pdata->maxbacks = GetSmall(tmp);
    
    DerefArg(tmp,X(0),12);	/* get Fixorder */
    pdata->norder = norder;
    for (i=0; i<norder; i++) {
      TAGGED t1;
      DerefCar(t1,tmp);
      DerefCdr(tmp,tmp);
      pdata->fixorder[i] = GetSmall(t1);
    }
    for (i=0; i<nobjects; i++) {
      SP_globref ref = O_SID_ATTR(i);
      Dvar dv = O_SID_DVAR(i);
      dvar_init(dv, ref, ref+1);
      dvar_attach_daemon(wam, dv, pdata, X(1), functor_dom1);
      dv = O_ORIG_DVAR(i,0);
      for (j=0; j<kdims; j++) {
	ref = O_ORIG_ATTR(i,j);
	dvar_init(dv+j, ref, ref+1);
	dvar_attach_daemon(wam, dv+j, pdata, X(1), fd.functor_minmax);
      }
      if (pdata->opt & OPT_VOLUME)
	compute_minmax_vol_sid(wam, pdata,i);
    }
    
    DerefArg(tmp,X(0),15);	/* get Procs */
    if (pdata->opt & OPT_RULES)
      geost_alloc_procs(wam, pdata, tmp);
  }

  /* RESUME */
  dvar_export_start(wam);
  {
    int nregions = pdata->nobjects;
    int ntasks = pdata->nobjects*pdata->max_nsboxes;
    size_t total_size;
    char *ptr;

    if (nregions<FIRST_BB+2)
      nregions = FIRST_BB+2; /* regions 0 and 1 are used by the while loop below */
    pdata->nregions = nregions;
    total_size =
      nregions*sizeof(SP_integer)+
      nregions*kdims*sizeof(SP_integer)+
      nregions*kdims*sizeof(SP_integer)+
      nregions*2*sizeof(SP_integer);

    ptr = SP_malloc(total_size);
    pdata->fr.obj = (SP_integer *)ptr;
    ptr += nregions*sizeof(SP_integer);
    pdata->fr.origin = (SP_integer *)ptr;
    ptr += nregions*kdims*sizeof(SP_integer);
    pdata->fr.end = (SP_integer *)ptr;
    ptr += nregions*kdims*sizeof(SP_integer);
    pdata->fr.heap = (SP_integer *)ptr;
    ptr += nregions*2*sizeof(SP_integer);
    SP_ASSERT(ptr == (char *)(pdata->fr.obj)+total_size);
    pdata->task = (struct task *)SP_malloc(ntasks*sizeof(struct task));
  }

  for (j=0; j<kdims; j++) {
    pdata->dvar_min[j] = CLPFD_MAXINT;
    pdata->dvar_max[j] = -CLPFD_MAXINT;
  }
  for (i=0; i<pdata->ntargets; i++)
    init_object(wam,pdata,(int)TARGET(i),1);
  dvar_refresh(pdata->fix_dvar);
  if (pdata->opt & OPT_VOLUME) {
    dvar_refresh(pdata->volume_dvar);
    dvar_fix_max_l(pdata->volume_dvar,HighInt-1); /* no 'sup' please */
  }
  for (j=0; j<kdims; j++) {
    dvar_refresh(pdata->lower_dvar+j);
    dvar_fix_interval_l(pdata->lower_dvar+j,-HighInt,HighInt-1); /* no 'inf'/'sup' please */
  }
  for (j=0; j<kdims; j++) {
    dvar_refresh(pdata->upper_dvar+j);
    dvar_fix_interval_l(pdata->upper_dvar+j,-HighInt,HighInt-1); /* no 'inf'/'sup' please */
  }
 loop:
   if (!geost_init(wam, pdata,1))
     goto ret;

#if EXPLICIT_FBOXES
   pdata->fr.end_relative = pdata->fr.end_bb;
   pdata->fr.end_absolute = pdata->fr.end_bb;
#endif
   pdata->fr.top_of_stack = pdata->fr.end_bb;

#if PALLET_LOADING
   if ((pdata->opt & OPT_PALLET_LOADING) && kdims==2 && !pl_wake(wam, pdata))
     goto ret;
#endif

   if ((pdata->opt & OPT_VOLUME) && !filter_volume(wam, pdata))
     goto ret;

   if (dvar_min_l(pdata->fix_dvar)==1) { /* FixAll mode */
     rc = fixall_geost(wam, pdata)? 0 : 3;
   } else {
     int inf = 0;
     int sup = pdata->ntargets-1;
     int held = (int)TARGET(sup); /* sup is the hole */
     int current = (int)TARGET(inf);

     while (inf<=sup) {
       if (must_sweep(wam, pdata,current)) {
	 TARGET(inf) = current;
	 inf++;
	 current = (inf>=sup ? held : (int)TARGET(inf));
       } else {
	 TARGET(sup) = current;
	 sup--;
	 current = (inf>=sup ? held : (int)TARGET(sup));
       }
     }
     rc = filter_geost(wam, pdata,inf);
   }
   switch (rc) {
   case 0x3:
   case 0x2:
     goto ret;
   case 0x1:
     goto loop;
   }
   for (i=0; i<pdata->ntargets; i++) {
     int object = (int)TARGET(i);
     O_FLAGS(object) &= ~0x1;	/* not queued */
   }
   if (dvar_min_l(pdata->fix_dvar)==0) { /* FixAll mode */
#if CUMULATIVE_METHOD
     if (pdata->opt & (OPT_DISJUNCTIVE|OPT_CUMULATIVE)) {
       switch (check_cumulative(wam, pdata)) {
       case 0x3:
       case 0x2:
	 goto ret;
       case 0x1:
	 goto loop;
       }
     }
#endif
   }
   /* BB TODO: ensure bounding box is tight, make this more incremental */
   bb_init(pdata,/*bounding box*/1);
   for (i=0; i<pdata->nobjects; i++)
     bb_add_object(wam, pdata,/*bounding box*/1,(int)TARGET(i));
   change = 0;
   for (j=0; j<kdims; j++) {
     if (dvar_fix_interval_l(pdata->lower_dvar+j,FR_ORIGIN(/*bounding box*/1,j),FR_END(/*bounding box*/1,j)+1)<0)
       goto ret;
     if (dvar_fix_interval_l(pdata->upper_dvar+j,FR_ORIGIN(/*bounding box*/1,j),FR_END(/*bounding box*/1,j)+1)<0)
       goto ret;
     if (dvar_min_l(pdata->lower_dvar+j)>FR_ORIGIN(/*bounding box*/1,j) ||
	 dvar_max_l(pdata->upper_dvar+j)<FR_END(/*bounding box*/1,j)+1)
       change = 1;
   }
   if (change) {
     for (i=0; i<pdata->ntargets; i++) {
       int object = (int)TARGET(i);
       O_FLAGS(object) |= 0x1;	/* maybe affected by tighter bound */
     }
         goto loop;
  }
  /* BB TODO: make this more incremental */
  for (i=0; i<pdata->ntargets; i++) {
    if (bb_bounding_box_lst_ect(wam, pdata,(int)TARGET(i)) < 0)
      goto ret;
  }
  pdata->prunedsid = 0;
  for (j=0; j<kdims; j++) {
    pdata->pruneddim[j] = 0;
  }

  ntargets_at_entry = pdata->ntargets;
  {
    int inf = 0;
    int sup = pdata->ntargets-1;
    int held = (int)TARGET(sup); /* sup is the hole */
    int current = (int)TARGET(inf);
    
    while (inf<=sup) {
      Dvar dv = O_SID_DVAR(current);
      SP_BOOL ground = dvar_is_integer(dv);
      for (j=0; j<kdims && ground; j++) {
	ground &= dvar_is_integer(dv+j+1);
      }
      if (!ground) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : (int)TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : (int)TARGET(sup));
      }
    }
    pdata->ntargets = inf;
  }
  if (pdata->ntargets>0) {
    if (!geost_init(wam, pdata,0))
      goto ret;
    if (pdata->opt & OPT_VOLUME) {
      for (i=pdata->ntargets; i<ntargets_at_entry; i++) {
	int object = (int)TARGET(i);
	pdata->rest_volume += SHAPE_VOLUME(get_first_sbox(O_MIN_VOL_SID(object)));
      }
      if (!(pdata->opt & OPT_OVERLAP)) {
	int j, k;
	for (j=0; j<kdims; j++) {
	  SP_integer minvol = 0;
	  SP_integer product = 1;

	  bb_init(pdata,/*bounding box*/2); /* bounding box of nonground */
	  for (i=0; i<pdata->ntargets; i++) {
	    int object = (int)TARGET(i);
	    if (!dvar_is_integer(O_SID_DVAR(object)) || 
		!dvar_is_integer(O_ORIG_DVAR(object,j))) {
	      bb_add_object(wam, pdata,/*bounding box*/2,object);
	      minvol += SHAPE_VOLUME(get_first_sbox(O_MIN_VOL_SID(object)));
	    }
	  }
	  if (minvol > 0) {
	    for (i=0; i<pdata->nobjects; i++) {
	      int object = (int)TARGET(i);
	      if (i >= pdata->ntargets ||
		  (dvar_is_integer(O_SID_DVAR(object)) && 
		   dvar_is_integer(O_ORIG_DVAR(object,j)))) {
		minvol += min_intersection_volume(wam, pdata,/*bounding box*/2,object);
	      }
	    }
	    for (k=0; k<kdims; k++)
	      product *= FR_END(/*bounding box*/2,k) - FR_ORIGIN(/*bounding box*/2,k) + 1;
	    if (minvol > product)
	      goto ret;
	  }
	}
      }
    }
  }
  for (i=0; i<ntargets_at_entry; i++) {
    int object = (int)TARGET(i);
    if (O_FLAGS(object) & 0x2) { /* pruned? */
      Dvar dv = O_SID_DVAR(object);
      dvar_pruning_done( dv);
      for (j=0; j<kdims; j++)
	dvar_pruning_done( dv+j+1);
    }
  }
  if (pdata->opt & OPT_VOLUME)
    dvar_pruning_done( pdata->volume_dvar);
  for (j=0; j<kdims; j++) {
    dvar_pruning_done( pdata->lower_dvar+j);
    dvar_pruning_done( pdata->upper_dvar+j);
  }
  for (i=0; i<ntargets_at_entry; i++) {
    int current = (int)TARGET(i);
    if (O_FLAGS(current) & 0x2) { /* pruned? */
      Dvar dv = O_SID_DVAR(current);
      dvar_export(dv);
      for (j=0; j<kdims; j++)
	dvar_export(dv+j+1);
    }
    O_FLAGS(current) &= ~0x3;	/* not queued, not pruned */
  }
  if (pdata->opt & OPT_VOLUME)
    dvar_export(pdata->volume_dvar);
  for (j=0; j<kdims; j++) {
    dvar_export(pdata->lower_dvar+j);
    dvar_export(pdata->upper_dvar+j);
  }
  
  CTagToArg(X(0),5) = MakeSmall(pdata->ntargets); /* NTargets */
  
#if PALLET_LOADING
  if (pdata->opt & OPT_PALLET_LOADING) {
    CTagToArg(X(0),16) = MakeSmall(pdata->pl.nholes); /* NHoles */
  }
#endif
  if (pdata->opt & OPT_VOLUME) {
    CTagToArg(X(0),17) = MakeSmall(pdata->rest_volume); /* RestVolume */
  }  
  ent = (pdata->ntargets==0);
 ret:
  SP_free(pdata->fr.obj);
  SP_free(pdata->task);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

#if DBG
void
dump_region(struct geost_data *pdata,
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

void 
dump_objects(struct geost_data *pdata)
{
  int i, j;
  int kdims = pdata->kdims;

  for (i=0; i<pdata->nobjects; i++) {
    Dvar dv = O_SID_DVAR(i);

    printf("*** object %d.sid\n\n", i);
    dvar_dump(dv);
    printf("\n");
    for (j=0; j<kdims; j++) {
      printf("*** object %d.x[%d]\n\n", i,j);
      dvar_dump(dv+j+1);
      printf("\n");
    }
  }
}
#endif

/*** rule interpretation stuff ***/

static SP_BOOL
flagged_in_dependency(struct geost_data *pdata,
		      int object)
{
  struct geost_proc *geost_proc = O_PROC(object);
  while (geost_proc) {
    int j;
    for (j=0; j<geost_proc->ndep; j++)
      if (O_FLAGS(geost_proc->dep[j])&0x1)
	return TRUE;
    geost_proc = geost_proc->next;
  }
  return FALSE;
}

static void
geost_alloc_procs(Wam wam,
			struct geost_data *pdata,
			TAGGED list)
{
  while (TagIsLST(list)) {
    TAGGED tproc, tmp, tcode;
    struct geost_proc *geost_proc;
    SP_integer extra;
    int i, dest, obj, nreg, nstack, ncode, ndep;

    DerefCar(tproc,list);
    DerefCdr(list,list);
    DerefArg(tmp,tproc,1);
    obj = GetSmall_int(tmp);
    DerefArg(tmp,tproc,2);
    dest = GetSmall_int(tmp);
    DerefArg(tmp,tproc,3);
    nreg = GetSmall_int(tmp);
    DerefArg(tmp,tproc,4);
    nstack = GetSmall_int(tmp);
    DerefArg(tmp,tproc,5);
    ncode = GetSmall_int(tmp);
    DerefArg(tmp,tproc,6);
    ndep = GetSmall_int(tmp);
    extra = ncode*sizeof(SP_integer) + ndep*sizeof(SP_integer);
    geost_proc = (struct geost_proc *)SP_malloc(sizeof(*geost_proc) + extra);
    geost_proc->next = O_PROC(obj);
    geost_proc->self = obj;
    geost_proc->dest = dest;
    geost_proc->nreg = nreg;
    geost_proc->nstack = nstack;
    geost_proc->ncode = ncode;
    geost_proc->code = (SP_integer *)(geost_proc+1);
    O_PROC(obj) = geost_proc;
    DerefArg(tcode,tproc,7);
    for (i=0; TagIsLST(tcode); i++) {
      DerefCar(tmp,tcode);
      DerefCdr(tcode,tcode);
      geost_proc->code[i] = GetSmall(tmp);
    }
    SP_ASSERT(i==ncode);
    geost_proc->ndep = ndep;
    geost_proc->dep = geost_proc->code + ncode;
    DerefArg(tcode,tproc,8);
    for (i=0; TagIsLST(tcode); i++) {
      DerefCar(tmp,tcode);
      DerefCdr(tcode,tcode);
      geost_proc->dep[i] = GetSmall(tmp);
    }
    SP_ASSERT(i==ndep);
  }
}

static void
geost_free_procs(Wam wam,
		       struct geost_data *pdata)
{
  int obj;
  for (obj=0; obj<pdata->nobjects; obj++) {
    struct geost_proc *geost_proc = O_PROC(obj);
    while (geost_proc) {
      struct geost_proc *next = geost_proc->next;
      SP_free(geost_proc);
      geost_proc = next;
    }
  }
}

static SP_BOOL
point_breaks_rule(Wam wam,
			struct geost_data *pdata,
			int object,
			SP_integer sid,
			SP_integer *point,
			int *region)
{
  struct geost_proc *geost_proc = O_PROC(object);
  while (geost_proc) {
    if (emulate_rule(wam, pdata,object,sid,point,region,geost_proc)) {
      return TRUE;
    }
    geost_proc = geost_proc->next;
  }
  return FALSE;
}

static SP_integer
get_sbox_minl(Wam wam,
		    struct geost_data *pdata,
		    int obj,
		    int n,
		    int dim)
{
  int kdims = pdata->kdims;
  SP_integer minl = CLPFD_MAXINT;
  DVITER it;

  dviter_init(&it, O_SID_DVAR(obj));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    SP_integer cand = S_SIZE(sbox0+n,dim);
    minl = minl < cand ? minl : cand;
  }
  return minl;
}

static SP_integer
get_sbox_maxl(Wam wam,
		    struct geost_data *pdata,
		    int obj,
		    int n,
		    int dim)
{
  int kdims = pdata->kdims;
  SP_integer maxl = -CLPFD_MAXINT;
  DVITER it;

  dviter_init(&it, O_SID_DVAR(obj));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    SP_integer cand = S_SIZE(sbox0+n,dim);
    maxl = maxl > cand ? maxl : cand;
  }
  return maxl;
}

static SP_integer
get_sbox_mint(Wam wam,
		    struct geost_data *pdata,
		    int obj,
		    int n,
		    int dim)
{
  int kdims = pdata->kdims;
  SP_integer mint = CLPFD_MAXINT;
  DVITER it;

  dviter_init(&it, O_SID_DVAR(obj));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    SP_integer cand = S_TRANSLATION(sbox0+n,dim);
    mint = mint < cand ? mint : cand;
  }
  return mint;
}

static SP_integer
get_sbox_maxt(Wam wam,
		    struct geost_data *pdata,
		    int obj,
		    int n,
		    int dim)
{
  int kdims = pdata->kdims;
  SP_integer maxt = -CLPFD_MAXINT;
  DVITER it;

  dviter_init(&it, O_SID_DVAR(obj));
  while (!dviter_empty(&it)) {
    SP_integer sid = dviter_next_value_l(&it);
    int sbox0 = get_first_sbox(sid);
    SP_integer cand = S_TRANSLATION(sbox0+n,dim);
    maxt = maxt > cand ? maxt : cand;
  }
  return maxt;
}


static SP_BOOL
emulate_rule(Wam wam,
		   struct geost_data *pdata,
		   int object,
		   SP_integer sid,
		   SP_integer *point,
		   int *region,
		   struct geost_proc *geost_proc)
{
  int kdims = pdata->kdims;
  struct dvar *dv = O_ORIG_DVAR(object,0);
  int sbox0 = get_first_sbox(sid);
  int R = pdata->fr.top_of_stack; /* R reg */
  SP_BOOL rc = FALSE;
  SP_integer *reg, *bpc, *br, i1, i2;
  int B = 0;			/* B reg */
  int PC = 0;			/* PC reg */
  int op, dest, t1, t2, t3, j;

  reg = (SP_integer *)SP_malloc((geost_proc->nreg + 2*geost_proc->nstack)*sizeof(SP_integer));
  bpc = reg+geost_proc->nreg;
  br = bpc+geost_proc->nstack;
  reg[0] = -CLPFD_MAXINT;
  reg[1] =  CLPFD_MAXINT;

  while (PC<geost_proc->ncode) {
    op = (int)geost_proc->code[PC++];
    dest = (int)geost_proc->code[PC++];
    switch (op) {
    case 0:			/* int(I) */
      reg[dest] = geost_proc->code[PC++];
      break;
    case 1:			/* xmin(Oix,J) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      reg[dest] = dvar_min_l(O_ORIG_DVAR(t1,t2));
      break;
    case 2:			/* xmax(Oix,J) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      reg[dest] = dvar_max_l(O_ORIG_DVAR(t1,t2));
      break;
    case 3:			/* xget(Oix,J) */
      t1 = (int)geost_proc->code[PC++];
      reg[dest] = point[t1];
      break;
    case 4:			/* lmin(Oix,J,D) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      t3 = (int)geost_proc->code[PC++];
      reg[dest] = get_sbox_minl(wam, pdata,t1,t2,t3);
      break;
    case 5:			/* lmax(Oix,J,D) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      t3 = (int)geost_proc->code[PC++];
      reg[dest] = get_sbox_maxl(wam, pdata,t1,t2,t3);
      break;
    case 6:			/* lget(J,D) */
      t2 = (int)geost_proc->code[PC++];
      t3 = (int)geost_proc->code[PC++];
      reg[dest] = S_SIZE(sbox0+t2,t3);
      break;
    case 7:			/* tmin(Oix,J,D) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      t3 = (int)geost_proc->code[PC++];
      reg[dest] = get_sbox_mint(wam, pdata,t1,t2,t3);
      break;
    case 8:			/* tmax(Oix,J,D) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      t3 = (int)geost_proc->code[PC++];
      reg[dest] = get_sbox_maxt(wam, pdata,t1,t2,t3);
      break;
    case 9:			/* tget(J,D) */
      t2 = (int)geost_proc->code[PC++];
      t3 = (int)geost_proc->code[PC++];
      reg[dest] = S_TRANSLATION(sbox0+t2,t3);
      break;
    case 10:			/* plus(R1,R2) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      reg[dest] = reg[t1] + reg[t2];
      break;
    case 11:			/* minus(R1,R2) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      reg[dest] = reg[t1] - reg[t2];
      break;
    case 12:			/* mul(R1,I2) */
      t1 = (int)geost_proc->code[PC++];
      i2 = geost_proc->code[PC++];
      reg[dest] = reg[t1] * i2;
      break;
    case 13:			/* floordiv(R1,I2) */
      t1 = (int)geost_proc->code[PC++];
      i2 = geost_proc->code[PC++];
      reg[dest] = reg[t1] / i2;
      break;
    case 14:			/* ceildiv(R1,I2) */
      t1 = (int)geost_proc->code[PC++];
      i2 = geost_proc->code[PC++];
      reg[dest] = (reg[t1]-1) / i2 + 1;
      break;
    case 15:			/* obox(L) */
      for (j=0; j<kdims; j++) {
	t1 = (int)geost_proc->code[PC++];
	t2 = (int)geost_proc->code[PC++];
	i1 = reg[t1];
	i2 = reg[t2];
	if (i1>point[j] || i2<point[j])
	  goto storempty;
	if (i1 < dvar_min_l(dv+j))
	  i1 = dvar_min_l(dv+j);
	if (i2 > dvar_max_l(dv+j))
	  i2 = dvar_max_l(dv+j);
	FR_ORIGIN(R,j) = i1;
	FR_END(R,j) = i2;
      }
      reg[dest] = R++;		/* invariant: new FR contains point */
      if (R >= pdata->nregions)
	expand_regions(wam, pdata);
      break;
    case 16:			/* ltz(R) */
      t1 = (int)geost_proc->code[PC++];
      if (reg[t1]<0)
	goto storinfty;
      else
	goto storempty;
    case 17:			/* lez(R) */
      t1 = (int)geost_proc->code[PC++];
      if (reg[t1]<=0)
	goto storinfty;
      else
	goto storempty;
    case 18:			/* intersection(R1,R2) */
      t1 = (int)geost_proc->code[PC++];
      t2 = (int)geost_proc->code[PC++];
      t1 = (int)reg[t1]; /* invariant: !ground implies FR's both contain point */
      t2 = (int)reg[t2];
      for (j=0; j<kdims; j++) {
	SP_integer o1 = FR_ORIGIN(t1,j);
	SP_integer o2 = FR_ORIGIN(t2,j);
	SP_integer e1 = FR_END(t1,j);
	SP_integer e2 = FR_END(t2,j);
	o1 = o1 > o2 ? o1 : o2;
	e1 = e1 < e2 ? e1 : e2;
	FR_ORIGIN(R,j) = o1;
	FR_END(R,j)    = e1;
      }
      reg[dest] = R++;		/* invariant: new FR contains point */
      if (R >= pdata->nregions)
	expand_regions(wam, pdata);
      break;
    case 19:			/* try(L) */
      bpc[B] = dest;
      br[B++] = R;
      break;
    case 20:			/* trust(L) */
      t1 = (int)geost_proc->code[dest+1]; /* DEST field of a JOIN insn */
      t2 = (int)geost_proc->code[dest+2];	/* R1 field of a JOIN insn */
      reg[t1] = reg[t2];
      PC = dest+4;
      B--;			/* NB! commit to this branch */
      break;
    case 21:			/* join(R1,R2) */
      PC++;			/* IGNORE R1 */
      t2 = (int)geost_proc->code[PC++];
      reg[dest] = reg[t2];
      break;
    default:
      SP_ASSERT(op>=0 && op<=21);
    storempty:
      if (B==0)			/* backtrack */
	goto ret;
      PC = (int)bpc[--B];
      R = (int)br[B];
      break;
    storinfty:
      for (j=0; j<kdims; j++) {
	FR_ORIGIN(R,j) = dvar_min_l(dv+j);
	FR_END(R,j) = dvar_max_l(dv+j);
      }
      reg[dest] = R++;		/* invariant: new FR contains point */
      if (R+1 >= pdata->nregions)
	expand_regions(wam, pdata);
      break;
    }
  }
  R = (int)reg[geost_proc->dest];
  *region = R;
  rc = TRUE;
 ret:
  SP_free(reg);
  return rc;
}
