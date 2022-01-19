/* Copyright(C) 2004, Swedish Institute of Computer Science */

/****************************************************************

 Generic filtering algorithms for unary resources:
 edge finding, detectable precedences, energetic reasoning.

 ****************************************************************/

#include "fd.h"
#include "dvars.h"

#define LONGEST_HOLE 1
/* -1 for bounds only */
#define LONGEST_HOLE_NBACKS 1000

/* essential for squares benchmark */
#define PROFILE_METHOD 1

/* essential for squares benchmark */
#define KNAPSACK_COLUMN_METHOD 1

/* cheap and effective */
#define PARALLEL_CONFLICT_PROFILE_METHOD 1

#define CONSECUTIVE_STAIRS_METHOD 1

#define EXTRA_METHOD (PROFILE_METHOD || KNAPSACK_COLUMN_METHOD ||  PARALLEL_CONFLICT_PROFILE_METHOD)


/*! duration of task \a t */
#define dur(t) ((t)->dur)
/*! earliest start time of task \a t (inclusive) */
#define est(t) ((t)->est)
/*! earliest completion time of task \a t (exclusive) */
#define ect(t) ((t)->est+(t)->dur)
/*! latest start time of task \a t (inclusive) */
#define lst(t) ((t)->lct-(t)->dur)
/*! latest completion time of task \a t (exclusive) */
#define lct(t) ((t)->lct)
/*! latest completion time of task \a t (exclusive) */
#define lctmax(t) ((t)->lctmax)
/*! resource consumption of task \a t */
#define res(t) ((t)->res)
#define enable(t) ((t)->enable)
#define must_precede(ti,tj) (ect(tj) > lst(ti))

#define TERMIN (-1)

#define SYNCRC(t)				\
{						\
  if (lct(t) > lctmax(t))			\
    lct(t) = lctmax(t);				\
  if (ect(t)>lct(t) && enable(t)==2)		\
    rc |= 0x2;					\
  else						\
    rc |= (t)->propagate(wam,t);		\
}						\

#define SYNCLB(t,ti,bound)			\
      if (pdata->lb[ti] < (bound)) {		\
	pdata->lb[ti] = (bound);		\
        if (lst(t) < (bound) && enable(t)==2)	\
	  return FALSE;				\
      }
#define SYNCUB(t,ti,bound)			\
      if (pdata->ub[ti] > (bound)) {		\
	pdata->ub[ti] = (bound);		\
        if (ect(t) > (bound) && enable(t)==2)	\
	  return FALSE;				\
      }


struct event {
  SP_integer key;
  union {
    struct task *task;
    SP_integer delta;
  } value;
};

struct comppart {
  SP_integer start;
  SP_integer height;
};

struct unary_data {
  int nbtasks;
  int flags;
  SP_integer slack;
  SP_integer resslack;
  SP_integer limit;
  struct task *tasks;
  struct event *event;
  struct event **eventp;
  struct comppart *cp;
  struct comppart *cp2;
  SP_integer *minloss;
  SP_integer *maxloss;
  SP_integer *id_map;
  /* theta-lambda trees */
  struct theta_lambda_node *tree;
  struct task **est_rank;
  struct task **lct_rank;
  struct task **aux_rank;
  SP_integer *tt_map;
  SP_integer *lb;
  SP_integer *ub;
  SP_integer *degree;
  SP_integer *edge;
  SP_integer *pred;
  SP_integer *succ;
  /* task intervals */
  int nbobltasks;
  int nbti_tasks;
  int nbti;
  SP_integer *est;
  SP_integer *lct;
  struct task **obltasks;
  struct task **ti_tasks;
  struct ti_index *ti_index;
  struct task_interval *task_interval;
  struct profile *profile;
#if PROFILE_METHOD
  struct task **contrib;
#endif
#if KNAPSACK_COLUMN_METHOD
  int nbrelevant;
  struct task **cand;
  SP_integer *support;
  SP_integer *sumres;
  int hashsize;
  SP_integer stamp;
  struct sw_on_key *knaphash;
  struct bucket *cache;
  SP_integer *knapsackl;
  SP_integer *knapsackr;
#endif
};

/* Sorting */

/* for qsorting events */
static int 
cmp_asc_events(Wam wam, struct event **p1, struct event **p2)
{
  struct event *l1 = *p1;
  struct event *l2 = *p2;
  int cmp = CMP(l1->key,l2->key);

  (void)wam;
  if (cmp==0)
    cmp = CMP(l1->value.task,l2->value.task);
  return cmp;
}


#define QType struct event *
#define QCmp  cmp_asc_events
#define QSort qsort_asc_events
#include "qsort.ic"

static void
qsort_asc_event_ptrs(Wam wam,
			   struct unary_data *pdata,
			   int n)
{
  int i;
  struct event *event = pdata->event;
  struct event **eventp = pdata->eventp;
  
  for (i=n-1; i>=0; i--)
    *eventp++ = event++;
  qsort_asc_events(wam, pdata->eventp,n);
}

static int 
cmp_ti_up(Wam wam, struct task **l1, struct task **l2) 
{
  struct task *t1 = *l1;
  struct task *t2 = *l2;
  int cmp = CMP(est(t1),est(t2));
  
  (void)wam;
  if (cmp==0)
    cmp = -CMP(lct(t1),lct(t2));
  if (cmp==0)
    cmp = CMP(lst(t1),lst(t2));
  if (cmp==0) {
    SP_integer area1 = dur(t1)*res(t1);
    SP_integer area2 = dur(t2)*res(t2);
    cmp = -CMP(area1,area2);
  }
  if (cmp==0)
    cmp = CMP(t1,t2);
  return cmp;
}


#define QType struct task *
#define QCmp  cmp_ti_up
#define QSort qsort_ti_up
#include "qsort.ic"

static int 
cmp_ti_down(Wam wam, struct task **l1, struct task **l2)
{
  struct task *t1 = *l1;
  struct task *t2 = *l2;
  int cmp = -CMP(lct(t1),lct(t2));
  
  (void)wam;
  if (cmp==0)
    cmp = CMP(est(t1),est(t2));
  if (cmp==0)
    cmp = -CMP(ect(t1),ect(t2));
  if (cmp==0) {
    SP_integer area1 = dur(t1)*res(t1);
    SP_integer area2 = dur(t2)*res(t2);
    cmp = -CMP(area1,area2);
  }
  if (cmp==0)
    cmp = CMP(t1,t2);
  return cmp;
}

#define QType struct task *
#define QCmp  cmp_ti_down
#define QSort qsort_ti_down
#include "qsort.ic"


/* THETA TREES */

/* ThetaTree index macros */
#define LSON(i)     ((i)<<1)
#define RSON(i)     (((i)<<1) + 1)
#define PARENT(i)   ((i)>>1)
#define ROOT(i)     ((i) == 1)
#define ISNODE(i)   ((i) >= 1)
#define LEAF(i)     (LSON(i) > nbtasks)
#define LBROTHER(i) (!RBROTHER(i))
#define RBROTHER(i) ((i)&1)

#define tstate cplfd_tstate     /* [PM] 4.0 work around AIX name conflict */
enum tstate {
  tstate_empty=0,
  tstate_normal,
  tstate_gray
};

struct theta_node
{
  SP_integer dur;      /* Processing time of activity. 0 if node is empty */
  SP_integer est;      /* Est of activity */
  SP_integer ECT; /* maximalni earliest completition time podstromu */
  SP_integer DUR;   /* celkovy processing time podstromu */
  SP_integer EST;      /* Est_Omega, where ECT_Omega = ECT */
  enum tstate state;
};

struct lambda_node
{
  SP_integer ECT;			/* ECT of subtree, one gray node can be used */
  SP_integer DUR;			/* sum duration in subtree, one gray node can be used */
  int  grayECT;			/* gray node responsible for ECT, or -1 */
  int  grayDUR;			/* gray node responsible for DUR, or -1 */
  int  gray;			/* rank if gray, or -1 */
};

struct theta_lambda_node {
  struct theta_node theta;
  struct lambda_node lambda;
};

/* Sort by increasing est. */

static int cmp_est(Wam wam, struct task **t1, struct task **t2)
{
  SP_integer val1 = est(*t1);
  SP_integer val2 = est(*t2);

  (void)wam;
  return CMP(val1,val2);
}

#define QType struct task *
#define QCmp  cmp_est
#define QSort qsort_est
#include "qsort.ic"

/* Sort by decreasing lct. */

static int cmp_lct(Wam wam, struct task **t1, struct task **t2)
{
  SP_integer val1 = -lct(*t1);
  SP_integer val2 = -lct(*t2);

  (void)wam;
  return CMP(val1,val2);
}

#define QType struct task *
#define QCmp  cmp_lct
#define QSort qsort_lct
#include "qsort.ic"

/* Sort by decreasing res. */

static int cmp_res(Wam wam, struct task **t1, struct task **t2)
{
  SP_integer val1 = -res(*t1);
  SP_integer val2 = -res(*t2);

  (void)wam;
  return CMP(val1,val2);
}

#define QType struct task *
#define QCmp  cmp_res
#define QSort qsort_res
#include "qsort.ic"

static void
init_ranks(Wam wam,
		  struct unary_data *pdata)
{
  int i;
  int nbtasks = pdata->nbtasks;
  
  for (i=nbtasks-1; i>=0; i--) {
    pdata->est_rank[i] = pdata->tasks+i;
    pdata->lct_rank[i] = pdata->tasks+i;
  }
  qsort_est(wam, pdata->est_rank,nbtasks);
  qsort_lct(wam, pdata->lct_rank,nbtasks);
  for (i=nbtasks-1; i>=0; i--) {
    pdata->est_rank[i]->est_rank = i;
    pdata->lct_rank[i]->lct_rank = i;
  }
}

static void 
tn_recompute_leaf(struct theta_node *th)
{
  if (th->state==tstate_normal) {
    th->ECT = th->est + th->dur;
    th->EST = th->est;
    th->DUR = th->dur;
  } else {
    th->ECT = -CLPFD_MAXINT2;
    th->EST = -CLPFD_MAXINT2;
    th->DUR = 0;
  }
}

static void 
tn_recompute_node(struct theta_node *th,
		  struct theta_node *left,
		  struct theta_node *right)
{
  SP_integer ECTleft;
  SP_integer ECTthis;
  SP_integer ECTright;

  if (th->state==tstate_normal) {
    ECTleft = left->ECT + th->dur  + right->DUR;
    ECTthis = th->est + th->dur + right->DUR;
    th->DUR = left->DUR + th->dur + right->DUR;
  } else {
    ECTleft = left->ECT + right->DUR;
    ECTthis = -CLPFD_MAXINT2;
    th->DUR = left->DUR + right->DUR;
  }
  ECTright = right->ECT;

  if (ECTright >= ECTthis && ECTright >= ECTleft) {
    th->ECT = ECTright;
    th->EST = right->EST;
  } else if (ECTthis >= ECTleft) {
    th->ECT = ECTthis;
    th->EST = th->est;
  } else {
    th->ECT = ECTleft;
    th->EST = left->EST;
  }
}

static SP_integer 
compute_max_ect(SP_integer ECT1, SP_integer ECT2, SP_integer ECT3)
{
  if (ECT3 >= ECT2 && ECT3 >= ECT1) {
    return ECT3;
  } else if (ECT2 >= ECT1) {
    return ECT2;
  } else {
    return ECT1;
  }
}

/* max ECT of current Theta if an activity is removed */
static SP_integer 
tt_del_element_common(struct unary_data *pdata,
		      int rank,
		      SP_integer esta,
		      SP_integer dura)
{
  int nbtasks = pdata->nbtasks;
  int node = (int)pdata->tt_map[rank];
  SP_integer ECT = -CLPFD_MAXINT2; /* fixed ECT of current node */

  if (pdata->tree[1].theta.EST > esta || pdata->tree[node].theta.state!=tstate_normal) {
    /* ECT in tree root doesn't use removed activity */
    return pdata->tree[1].theta.ECT;
  }

  if (!LEAF(node)) {
    ECT = compute_max_ect(
      pdata->tree[LSON(node)].theta.ECT + pdata->tree[node].theta.dur - dura + pdata->tree[RSON(node)].theta.DUR,
      pdata->tree[node].theta.dur == dura ? -CLPFD_MAXINT2 :
	(pdata->tree[node].theta.est + pdata->tree[node].theta.dur - dura + pdata->tree[RSON(node)].theta.DUR),
      pdata->tree[RSON(node)].theta.ECT);
  } else {
    ECT = pdata->tree[node].theta.dur == dura ? -CLPFD_MAXINT2 : (pdata->tree[node].theta.est + pdata->tree[node].theta.dur - dura);
  }

  while (!ROOT(node)) {
    int parent = PARENT(node);
    SP_integer pest, pdur;
    if (pdata->tree[parent].theta.state==tstate_normal) {
      pest = pdata->tree[parent].theta.est;
      pdur = pdata->tree[parent].theta.dur;
    } else {
      pest = -CLPFD_MAXINT2;
      pdur = 0;
    }
    if (LBROTHER(node)) {
      ECT = compute_max_ect(
	ECT + pdur + pdata->tree[RSON(parent)].theta.DUR,
	pest + pdur + pdata->tree[RSON(parent)].theta.DUR,
	pdata->tree[RSON(parent)].theta.ECT);
    } else {
      ECT = compute_max_ect(
	pdata->tree[LSON(parent)].theta.ECT + pdur + pdata->tree[RSON(parent)].theta.DUR - dura,
	pest + pdur + pdata->tree[RSON(parent)].theta.DUR - dura,
	ECT);
    }
    node = parent;
  }

  return ECT;
}

static SP_integer 
tt_del_element_ect(struct unary_data *pdata,
		   struct task *a)
{
  return tt_del_element_common(pdata,a->est_rank,est(a),dur(a));
}

static SP_integer 
tt_del_element_lst(struct unary_data *pdata,
		   struct task *a)
{
  return -tt_del_element_common(pdata,a->lct_rank,-lct(a),dur(a));
}


/* Compute the mapping from rank to node so that LSON and RSON work
   as expected.  E.g.
   nbtasks=1 tt_map={1}
   nbtasks=2 tt_map={2,1}
   nbtasks=3 tt_map={2,1,3}
   nbtasks=4 tt_map={4,2,1,3}
   nbtasks=5 tt_map={4,2,5,1,3}
   nbtasks=6 tt_map={4,2,5,1,6,3}
   nbtasks=7 tt_map={4,2,5,1,6,3,7}
*/
static void
tt_init_inorder(int node, SP_integer **mapp, int nbtasks)
{
  if (node <= nbtasks) {
    tt_init_inorder(LSON(node),mapp,nbtasks);
    *((*mapp)++) = node;
    tt_init_inorder(RSON(node),mapp,nbtasks);
  }			
}

static void
tt_init(Wam wam,
	       struct unary_data *pdata)
{
  SP_integer *map = pdata->tt_map;

  tt_init_inorder(1,&map,pdata->nbtasks);
  init_ranks(wam, pdata);
}

/* THETA-LAMBDA-TREES */

static void 
tln_clear(struct theta_lambda_node *th)
{
  th->theta.state = tstate_empty;
  th->theta.dur = 0;
  th->theta.est = -CLPFD_MAXINT2;
  th->theta.DUR = 0;
  th->theta.EST = -CLPFD_MAXINT2;
  th->theta.ECT = -CLPFD_MAXINT2;
  th->lambda.ECT = -CLPFD_MAXINT2;
  th->lambda.DUR = 0;
  th->lambda.grayECT = -1;
  th->lambda.grayDUR = -1;
  th->lambda.gray = -1;
}

static void 
tln_recompute_leaf(struct theta_lambda_node *th)
{
  SP_integer est = th->theta.est;
  SP_integer dur = th->theta.dur;

  tn_recompute_leaf(&th->theta);
  if (th->theta.state==tstate_gray) {
    th->lambda.DUR = dur;
    th->lambda.ECT = est+dur;
    th->lambda.grayECT = th->lambda.gray;
    th->lambda.grayDUR = th->lambda.gray;
  } else {
    th->lambda.DUR = th->theta.DUR;
    th->lambda.ECT = th->theta.ECT;
    th->lambda.grayECT = -1;
    th->lambda.grayDUR = -1;
  }
}

static void 
tln_recompute_node(struct theta_lambda_node *th,
		   struct theta_lambda_node *left,
		   struct theta_lambda_node *right)
{
  SP_integer ECTleft1;
  SP_integer ECTleft2;
  SP_integer ECTthis;
  SP_integer ECT;
  SP_integer thdur, thelam, lamthe, thethethe;
  int  gray1;
  int  gray;
  
  tn_recompute_node(&th->theta, &left->theta, &right->theta);
  if (th->lambda.gray == -1 &&
      left->lambda.grayDUR == -1 && right->lambda.grayDUR == -1 &&
      left->lambda.grayECT == -1 && right->lambda.grayECT == -1) {
    th->lambda.DUR = th->theta.DUR;
    th->lambda.grayDUR = -1;
    th->lambda.ECT = th->theta.ECT;
    th->lambda.grayECT = -1;
    return;
  }
  thdur = th->theta.dur;
  thelam = left->theta.DUR + right->lambda.DUR;
  lamthe = left->lambda.DUR + right->theta.DUR;
  thethethe = left->theta.DUR + thdur + right->theta.DUR;
  switch (th->theta.state) {
  case tstate_empty:
    if (thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    ECTleft1 = left->theta.ECT + right->lambda.DUR;
    gray1 = right->lambda.grayDUR;
    ECTleft2 = left->lambda.ECT + right->theta.DUR;
    ECTthis = -CLPFD_MAXINT2;
    break;
  case tstate_normal:
    if (thelam >= lamthe) {
      th->lambda.DUR = thdur + thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = thdur + lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    ECTleft1 = left->theta.ECT + thdur + right->lambda.DUR;
    gray1 = right->lambda.grayDUR;
    ECTleft2 = left->lambda.ECT + thdur + right->theta.DUR;
    ECTthis = th->theta.est + thdur + right->lambda.DUR;
    break;
  case tstate_gray:
  default:
    if (thelam >= thethethe && thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else if (thethethe >= lamthe) {
      th->lambda.DUR = thethethe;
      th->lambda.grayDUR = th->lambda.gray;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    ECTleft1 = left->theta.ECT + right->lambda.DUR;
    gray1 = right->lambda.grayDUR;
    if (thdur + right->theta.DUR > right->lambda.DUR) {
      ECTleft1 = left->theta.ECT + thdur + right->theta.DUR;
      gray1 = th->lambda.gray;
    }
    ECTleft2 = left->lambda.ECT + right->theta.DUR;
    ECTthis = th->theta.est + thdur + right->theta.DUR;
  }

  ECT = right->lambda.ECT;
  gray = right->lambda.grayECT;
  
  if (ECT < ECTthis) {
    ECT = ECTthis;
    if (th->theta.state==tstate_gray)
      gray = th->lambda.gray;
    else
      gray = right->lambda.grayDUR;
  }
  
  if (ECT < ECTleft2) {
    ECT = ECTleft2;
    gray = left->lambda.grayECT;
  }
  
  if (ECT < ECTleft1) {
    ECT = ECTleft1;
    gray = gray1;
  }
  
  th->lambda.ECT = ECT;
  th->lambda.grayECT = gray;
}

static void 
tln_recompute_gray(struct theta_lambda_node *th,
		   struct theta_lambda_node *left, 
		   struct theta_lambda_node *right,
		   int gray)
{
  SP_integer thdur, thelam, lamthe, thethethe;
  
  if (th->theta.ECT != th->lambda.ECT) {
    tln_recompute_node(th, left, right);
    return;
  }

  th->lambda.grayECT = gray;
  tn_recompute_node(&th->theta, &left->theta, &right->theta);
  thdur = th->theta.dur;
  thelam = left->theta.DUR + right->lambda.DUR;
  lamthe = left->lambda.DUR + right->theta.DUR;
  thethethe = left->theta.DUR + thdur + right->theta.DUR;

  switch (th->theta.state) {
  case tstate_empty:
    if (thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    break;
  case tstate_normal:
    if (thelam >= lamthe) {
      th->lambda.DUR = thdur + thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = thdur + lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    break;
  case tstate_gray:
    if (thelam >= thethethe && thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else if (thethethe >= lamthe) {
      th->lambda.DUR = thethethe;
      th->lambda.grayDUR = th->lambda.gray;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
  }
}

static void 
tlt_bottom_up(struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  int i;
  
  for (i = nbtasks; LEAF(i); i--) {
    tln_recompute_leaf(&pdata->tree[i]);
  }
  for (; ISNODE(i); i--) {
    tln_recompute_node(&pdata->tree[i],
		       &pdata->tree[LSON(i)],
		       &pdata->tree[RSON(i)]);
  }
}

static void 
tlt_gray_activity(struct unary_data *pdata,
		  int rank)
{
  int nbtasks = pdata->nbtasks;
  int node = (int)pdata->tt_map[rank];
  
  pdata->tree[node].theta.state = tstate_gray;
  pdata->tree[node].lambda.gray = rank;
  if (LEAF(node)) {
    tln_recompute_leaf(&pdata->tree[node]);
    node = PARENT(node);
  }
  while (ISNODE(node)) {
    tln_recompute_gray(&pdata->tree[node],
		       &pdata->tree[LSON(node)], 
		       &pdata->tree[RSON(node)],
		       rank);
    node = PARENT(node);
  }
}

static void 
tlt_clear(struct unary_data *pdata)
{
  int nbtheta = 2*pdata->nbtasks;
  int i;
  
  for (i=1; i<nbtheta; i++)
    tln_clear(&pdata->tree[i]);
}

static void
tlt_fix_path(struct unary_data *pdata,
	     int node)
{
  int nbtasks = pdata->nbtasks;
  if (LEAF(node)) {
    tln_recompute_leaf(&pdata->tree[node]);
    node = PARENT(node);
  }
  while (ISNODE(node)) {
    tln_recompute_node(&pdata->tree[node],
		       &pdata->tree[LSON(node)],
		       &pdata->tree[RSON(node)]);
    node = PARENT(node);
  }
}

static void 
tlt_place(struct unary_data *pdata,
	  int rank,
	  SP_integer est,
	  SP_integer dur,
	  enum tstate state)
{
  int node = (int)pdata->tt_map[rank];
  struct theta_lambda_node *th = &pdata->tree[node];
  
  th->theta.state = tstate_normal;
  th->theta.est = est;
  th->theta.dur = dur;
  switch (state) {
  case tstate_normal:
    tlt_fix_path(pdata,node);
    break;
  case tstate_gray:
    tlt_gray_activity(pdata,rank);
  case tstate_empty:
    break;
  }
}

static void 
tlt_remove(struct unary_data *pdata,
	   int rank)
{
  int node = (int)pdata->tt_map[rank];
  struct theta_lambda_node *th = &pdata->tree[node];
  
  tln_clear(th);
  tlt_fix_path(pdata,node);
}

/* max ECT of current Theta+(the activity rank)+(at most one activity of Lambda) */
static SP_integer 
tlt_del_element_common(struct unary_data *pdata,
		       int rank,
		       SP_integer esta,
		       SP_integer dura,
		       int *grayp)
{
  int node = (int)pdata->tt_map[rank];
  SP_integer ECT = pdata->tree[1].lambda.ECT; /* from the root */
  int grayECT = pdata->tree[1].lambda.grayECT; /* from the root */

  if (pdata->tree[1].theta.EST <= esta && pdata->tree[node].theta.state==tstate_normal) {
    /* ECT in tree root uses the removed activity */
    tlt_remove(pdata, rank);
    ECT = pdata->tree[1].lambda.ECT; /* from the root */
    grayECT = pdata->tree[1].lambda.grayECT; /* from the root */
    tlt_place(pdata, rank, esta, dura, tstate_normal);
  }
  *grayp = grayECT;
  return ECT;
}

static SP_integer 
tlt_del_element_ect(struct unary_data *pdata,
		    struct task *a,
		    int *grayp)
{
  return tlt_del_element_common(pdata,a->est_rank,est(a),dur(a),grayp);
}

static SP_integer 
tlt_del_element_lst(struct unary_data *pdata,
		    struct task *a,
		    int *grayp)
{
  return -tlt_del_element_common(pdata,a->lct_rank,-lct(a),dur(a),grayp);
}


/* DETECTABLE PRECEDENCES */

#define CACHE_CLEAR c1=c2=NULL;

#define CACHE_ADD(T) c2=c1; c1=(T);

#define CACHE_MORE_THAN(T) (c2 || (c1 && c1!=(T)))

static SP_BOOL
unary_detect_precedences(Wam wam,
				struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  struct event *event = pdata->event;
  struct task *tasks = pdata->tasks;
  int i, j;
  int nbevent = nbtasks<<1;
  struct task *c1, *c2;

  for (i=0, j=0; i<nbtasks; i++, j+=2) {
    event[j].key = (ect(tasks+i)*2);
    event[j].value.task = (tasks+i);
    event[j+1].key = (lst(tasks+i)*2)+1;
    event[j+1].value.task = (tasks+i);
  }
  qsort_asc_event_ptrs(wam, pdata,nbevent);
  CACHE_CLEAR;
  tlt_clear(pdata);
  for (j=0; j<nbevent; j++) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->value.task;
    
    if (ev->key & 1) { /* lst item */
      if (enable(t)<2) {
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_gray);
      } else {
	CACHE_ADD(t);
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_normal);
      }
    } else {
      int rank;
      
      if (CACHE_MORE_THAN(t)) { /* t is preceded by omega-t */
	int ti = (int)(t-tasks);
	SP_integer lb = tt_del_element_ect(pdata,t);
	SYNCLB(t,ti,lb);
      }

      if (enable(t)==2) {
	while (tlt_del_element_ect(pdata,t,&rank) > lst(t)) {
	  struct task *to = pdata->est_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  CACHE_CLEAR;
  tlt_clear(pdata);
  for (j--; j>=0; j--) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->value.task;
    
    if (!(ev->key & 1)) { /* ect item */
      if (enable(t)<2) {
	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_gray);
      } else {
	CACHE_ADD(t);
	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_normal);
      }
    } else {
      int rank;

      if (CACHE_MORE_THAN(t)) { /* t precedes omega-t */
	int ti = (int)(t-tasks);
	SP_integer ub = tt_del_element_lst(pdata,t);
	SYNCUB(t,ti,ub);
      }

      if (enable(t)==2) {
	while (tlt_del_element_lst(pdata,t,&rank) < ect(t)) {
	  struct task *to = pdata->lct_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  return TRUE;
}


/* NOT-FIRST & NOT-LAST */

static SP_BOOL
unary_not_first_last(Wam wam,
			    struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  struct event *event = pdata->event;
  struct task *tasks = pdata->tasks;
  int i, j;
  int nbevent = nbtasks<<1;
  SP_integer limt=0, limtl=0;
  struct task *c1, *c2;

  CACHE_CLEAR;
  tlt_clear(pdata);
  for (i=0, j=0; i<nbtasks; i++, j+=2) {
    event[j].key = (lct(tasks+i)*2);
    event[j].value.task = (tasks+i);
    event[j+1].key = (lst(tasks+i)*2)+1;
    event[j+1].value.task = (tasks+i);
  }
  qsort_asc_event_ptrs(wam, pdata,nbevent);
  for (j=0; j<nbevent; j++) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->value.task;
    if (ev->key & 1) { /* lst item */
      limtl = lst(t);
      if (enable(t)<2) {
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_gray);
      } else {
	limt = limtl;
	CACHE_ADD(t);
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_normal);
      }
    } else {
      int rank;

      if (CACHE_MORE_THAN(t)) {
	int ti = (int)(t-tasks);
	if (tt_del_element_ect(pdata,t) > lst(t)) /* t is not last among omega */
	  SYNCUB(t,ti,limt);
      }

      if (enable(t)==2 && limtl < ect(t)) {
	while (tlt_del_element_ect(pdata,t,&rank) > lst(t)) {
	  struct task *to = pdata->est_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  CACHE_CLEAR;
  tlt_clear(pdata);
  for (i=0, j=0; i<nbtasks; i++, j+=2) {
    event[j].key = (est(tasks+i)*2)+1;
    event[j].value.task = (tasks+i);
    event[j+1].key = (ect(tasks+i)*2);
    event[j+1].value.task = (tasks+i);
  }
  qsort_asc_event_ptrs(wam, pdata,nbevent);
  while (--j >= 0) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->value.task;
    if (!(ev->key & 1)) { /* lst item */
      limtl = ect(t);
      if (enable(t)<2) {
 	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_gray);
      } else {
 	limt = limtl;
 	CACHE_ADD(t);
 	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_normal);
      }
    } else {
      int rank;
      if (CACHE_MORE_THAN(t)) {
 	int ti = (int)(t-tasks);
 	if (tt_del_element_lst(pdata,t) < ect(t)) /* t is not first among omega */
 	  SYNCLB(t,ti,limt);
      }
      if (enable(t)==2 && limtl > lst(t)) {
 	while (tlt_del_element_lst(pdata,t,&rank) < ect(t)) {
 	  struct task *to = pdata->lct_rank[rank];
 	  tlt_remove(pdata,rank);
 	  enable(to) = 0;
 	}
      }
    }
  }  
  return TRUE;
}


/* FIRST & LAST (EDGE-FINDING) */

static SP_BOOL
unary_first_last(struct unary_data *pdata,
		 SP_integer *lst,
		 SP_integer *ect)
{
  int nbtasks = pdata->nbtasks;
  struct theta_lambda_node *root = &pdata->tree[1];
  int i, di, dj;
  
  tlt_clear(pdata);

  /* Put all activities into theta tree */
  for (i=0; i < nbtasks; i++) {
    struct task *t = &pdata->tasks[i];
    
    if (enable(t)==2)
      tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_empty);
  }

  tlt_bottom_up(pdata);
  *ect = root->theta.ECT;

  /* Form set of obligatory tasks by descending lct. */
  for (di=0, dj=0; di<nbtasks; di++) {
    struct task *t = pdata->lct_rank[di];
    
    if (enable(t)==2)
      pdata->aux_rank[dj++] = t;
  }

  /* In descending order by lct remove activities (ie make them gray) */
  for (di=0; di<dj-1; di++) {
    SP_integer lct;
    struct task *t = pdata->aux_rank[di];

    tlt_gray_activity(pdata,t->est_rank);
    
    lct = lct(pdata->aux_rank[di+1]);
    if (root->theta.ECT > lct)
      return FALSE;

    while (root->lambda.ECT > lct) {
      int rank = root->lambda.grayECT;
      struct task *t = pdata->est_rank[rank];
      int ti = (int)(t - pdata->tasks);
      SP_integer ect = root->theta.ECT;

      SYNCLB(t,ti,ect);
      tlt_remove(pdata,rank);
    }
  }
  
  tlt_clear(pdata);

  /* Put all activities into theta tree */
  for (i=0; i < nbtasks; i++) {
    struct task *t = &pdata->tasks[i];
    
    if (enable(t)==2)
      tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_empty);
  }

  tlt_bottom_up(pdata);
  *lst = -root->theta.ECT;

  /* Form set of obligatory tasks by descending lct. */
  for (di=0, dj=0; di<nbtasks; di++) {
    struct task *t = pdata->est_rank[di];
    
    if (enable(t)==2)
      pdata->aux_rank[dj++] = t;
  }

  /* In ascending order by est remove activities (ie make them gray) */
  for (di=0; di<dj-1; di++) {
    SP_integer est;
    struct task *t = pdata->aux_rank[di];

    tlt_gray_activity(pdata,t->lct_rank);
    
    est = est(pdata->aux_rank[di+1]);
    if (-root->theta.ECT < est)
      return FALSE;

    while (-root->lambda.ECT < est) {
      int rank = root->lambda.grayECT;
      struct task *t = pdata->lct_rank[rank];
      int ti = (int)(t - pdata->tasks);
      SP_integer lst = -root->theta.ECT;

      SYNCUB(t,ti,lst);
      tlt_remove(pdata,rank);
    }
  }
  return TRUE;
}


/* EXPLICIT PRECEDENCES AS SIDE-CONSTRAINTS */

static int
unary_detect_all_precedences(Wam wam, 
				    struct unary_data *pdata,
				    Dvar diffvar,
				    struct diff_constraint *dc,
				    int nbdiffs);


/* ENERGETIC REASONING WITHIN TASK INTERVALS */

struct ti_index {
  SP_integer key;
  struct ti_index *val;		/* subtree for prefix */
  struct ti_index *next;	/* to tree of the next prefix */
};

struct task_interval {
  SP_integer a;
  SP_integer b;
  SP_integer minslack;
  SP_integer maxslack;
};

#define get_ti_index(ROW,COL) (pdata->ti_index+5*(ROW)+(COL))

#define ti_subsumes(TI1,TI2)								\
((TI1)->a >= (TI2)->a &&								\
 (TI1)->b <= (TI2)->b &&								\
 (((TI1)->b-(TI1)->a)-((TI2)->b-(TI2)->a)) * maxlimit >= (TI1)->maxslack-(TI2)->maxslack)	\


/* compute min area of <est,lct,dur> overlapping [lb,ub) */
static SP_integer 
min_overlap(struct task *t, SP_integer lb, SP_integer ub)
{
  SP_integer ltmp1=ect(t)-lb, ltmp2=ub-lst(t);
  SP_integer r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);

  if (r<=0)
    return 0;
  if (r>dur(t))
    r = dur(t);
  if (r>ub-lb)
    r = ub-lb;
  return r * res(t);
}

/* compute max area of <est,lct,dur> overlapping [lb,ub) */
static SP_integer 
max_overlap(struct task *t, SP_integer lb, SP_integer ub)
{
  SP_integer ltmp1=lct(t)-lb, ltmp2=ub-est(t);
  SP_integer r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);

  if (r<=0)
    return 0;
  if (r>dur(t))
    r = dur(t);
  if (r>ub-lb)
    r = ub-lb;
  return r * res(t);
}

static int
must_overlap_at_least(Wam wam,
			    struct task *t,
			    SP_integer lmin,
			    SP_integer inf,
			    SP_integer sup)
{
  int rc=0;
  SP_integer dur = dur(t);
  if (dur<lmin)
    return 0x2;
  if (est(t)<inf+lmin-dur) {
    est(t) = inf+lmin-dur;
    SYNCRC(t);
  }
  if (lctmax(t)>sup-lmin+dur) {
    lctmax(t) = sup-lmin+dur;
    SYNCRC(t);
  }
  return rc;
}

/* We need an O(N log N) algorithm for building a 4-level "indexing
   tree" of tasks.  

   For adjusting ends, the tasks should be sorted by
   [est(up)][lct(down)][lst(up)][area(down)].
   For adjusting starts, the tasks should be sorted by
   [lct(down)][est(up)][ect(down)][area(down)].
*/
static void 
ti_build_index(Wam wam,
		      struct unary_data *pdata, SP_BOOL up)
{
  int i, j;

  for (i=0, j=0; i<pdata->nbobltasks; i++) {
    struct task *t = pdata->obltasks[i];
    if (lct(t)-est(t)>dur(t))
      pdata->ti_tasks[j++] = t;
  }
  pdata->nbti_tasks = j;
  if (up)
    qsort_ti_up(wam, pdata->ti_tasks,pdata->nbti_tasks);
  else
    qsort_ti_down(wam, pdata->ti_tasks,pdata->nbti_tasks);
  for (i=0; i<pdata->nbti_tasks; i++) {
    struct ti_index *tix = get_ti_index(i,0);
    struct task *t = pdata->ti_tasks[i];

    for (j=0; j<5; j++) {
      (tix+j)->val = j<4 ? (tix+j+1) : NULL;
      (tix+j)->next = (j==0 && i<pdata->nbti_tasks-1) ? (tix+5) : NULL;
    }
    if (up) {
      (tix+0)->key = est(t);
      (tix+1)->key = lct(t);
      (tix+2)->key = lst(t);
      (tix+3)->key = dur(t)*res(t);
      (tix+4)->key = (SP_integer)t;
    } else {
      (tix+0)->key = lct(t);
      (tix+1)->key = est(t);
      (tix+2)->key = ect(t);
      (tix+3)->key = dur(t)*res(t);
      (tix+4)->key = (SP_integer)t;
    }
  }
  for (j=0; j<4; j++) {
    for (i=pdata->nbti_tasks-2; i>=0; i--) {
      struct ti_index *tix = get_ti_index(i,j);

      if (tix->next && tix->key==tix->next->key) {
	tix->val->next = tix->next->val;
	tix->next = tix->next->next;
      }
    }
  }
}

static int 
ti_do_one_interval_up(Wam wam,
			    struct unary_data *pdata,
			    SP_integer a,
			    SP_integer b,
			    SP_integer tslack)
{
  int rc = 0;
  struct ti_index *tix1, *tix2, *tix3, *tix4, *tix5;
  
  for (tix1 = pdata->ti_index; tix1 && tix1->key < a; tix1 = tix1->next) {
    for (tix2 = tix1->val; tix2 && tix2->key > a; tix2 = tix2->next) {
      for (tix3 = tix2->val; tix3 && tix3->key < b; tix3 = tix3->next) {
	for (tix4 = tix3->val; tix4 && tix4->key > tslack; tix4 = tix4->next) {
	  for (tix5 = tix4->val; tix5; tix5 = tix5->next) {
	    struct task *t = (struct task *)tix5->key;
	    SP_integer a2 = a;
	    SP_integer b2 = b;
	    SP_integer res = res(t);
	    SP_integer slack = tslack+min_overlap(t,a,b);

	    if (b2 > lct(t))
	      b2 = lct(t);
	    if (a2 < lst(t))
	      a2 = lst(t);
	    if ((b2-a2)*res > slack && (lct(t)-a)*res > slack) {
	      lctmax(t) = a+slack/res;
	      SYNCRC(t);
	      if (rc & 0x2)
		return rc;
	    }
	  }
	}
      }
    }
  }
  return rc;
}

static int 
ti_do_one_interval_down(Wam wam,
			      struct unary_data *pdata,
			      SP_integer a,
			      SP_integer b,
			      SP_integer tslack)
{
  int rc = 0;
  struct ti_index *tix1, *tix2, *tix3, *tix4, *tix5;
  
  for (tix1 = pdata->ti_index; tix1 && tix1->key > b; tix1 = tix1->next) {
    for (tix2 = tix1->val; tix2 && tix2->key < b; tix2 = tix2->next) {
      for (tix3 = tix2->val; tix3 && tix3->key > a; tix3 = tix3->next) {
	for (tix4 = tix3->val; tix4 && tix4->key > tslack; tix4 = tix4->next) {
	  for (tix5 = tix4->val; tix5; tix5 = tix5->next) {
	    struct task *t = (struct task *)tix5->key;
	    SP_integer a2 = a;
	    SP_integer b2 = b;
	    SP_integer res = res(t);
	    SP_integer slack = tslack+min_overlap(t,a,b);

	    if (b2 > ect(t))
	      b2 = ect(t);
	    if (a2 < est(t))
	      a2 = est(t);
	    if ((b2-a2)*res > slack && (b-est(t))*res > slack) {
	      est(t) = b-slack/res;
	      SYNCRC(t);
	      if (rc & 0x2)
		return rc;
	    }
	  }
	}
      }
    }
  }
  return rc;
}

/* An O(N^2) algorithm for computing all task intervals and their slacks. 
   Given fixed t and a, minoverlap(t,a,b) is a piecewise linear function in b
   with at most 3 segments: zero, a slope, and a plateau:

   max(0,min(b-a, dur(t), ect(t)-a, b-lst(t)))
   = 0                          , if b <= max(lst(t),a)
   = res(t)*(b-max(lst(t),a))   , if max(lst(t),a) <= b <= max(lst(t),a)+min(dur(t),ect(t)-a)
   = res(t)*min(dur(t),ect(t)-a), if max(lst(t),a)+min(dur(t),ect(t)-a) =< b

   Similarly, so is maxoverlap(t,a,b):

   max(0,min(b-a, dur(t), lct(t)-a, b-est(t)))
   = 0                          , if b <= max(est(t),a)
   = res(t)*(b-max(est(t),a))   , if max(est(t),a) <= b <= max(est(t),a)+min(dur(t),lct(t)-a)
   = res(t)*min(dur(t),lct(t)-a), if max(est(t),a)+min(dur(t),lct(t)-a) =< b

   Algorithm idea:
   For each distinct a=est(t):
     Build an event queue with events (break ties arbitrarily):
     0x0: event date is a unique b=lct(t) > a.
     0x1: event date is the beginning of a slope for minoverlap
     0x2: event date is the end of a slope for minoverlap
     0x3: event date is the beginning of a slope for maxoverlap
     0x4: event date is the end of a slope for maxoverlap
     Do the sweep.  For each LCT event, record a task interval <a,b,maxslack,minslack>.
*/
static SP_BOOL 
ti_collect_intervals(Wam wam,
			    struct unary_data *pdata,
			    Dvar limitvar,
			    int flags)
{
  int nbobltasks = pdata->nbobltasks;
  struct event *event = pdata->event;
  int i, j, k, n, nbest, nblct, nbevent;

  for (i=0, j=0; i<nbobltasks; i++) {
    struct task *t = pdata->obltasks[i];
    
    pdata->est[j] = est(t);
    pdata->lct[j++] = lct(t);
  }
  fd_qsort_asc_long(wam, pdata->est,nbobltasks);
  fd_qsort_asc_long(wam, pdata->lct,nbobltasks);
  for (i=1, j=1; i<nbobltasks; i++) {
    if (pdata->est[j-1]!=pdata->est[i])
      pdata->est[j++] = pdata->est[i];
  }
  nbest = j;
  for (i=1, j=1; i<nbobltasks; i++) {
    if (pdata->lct[j-1]!=pdata->lct[i])
      pdata->lct[j++] = pdata->lct[i];
  }
  nblct = j;

  n=0;
  for (i=0; i<nbest; i++) {
    SP_integer a = pdata->est[i];
    SP_integer mincoeff = 0;
    SP_integer maxcoeff = 0;
    SP_integer minterm = 0;
    SP_integer maxterm = 0;
    SP_integer maxarea = 0;
    SP_integer maxb = 0;
    for (j=0, k=0; j<nblct; j++) {
      SP_integer b = pdata->lct[j];
      if (a<b && (i>0 || j<nblct-1)) {
	(event+k)->key = maxb = b*8;	/* LCT event */
	(event+k)->value.task = NULL;
	k++;
      }
    }
    for (j=0; j<nbobltasks; j++) {	/* I should sort... */
      struct task *t = pdata->obltasks[j];
      SP_integer area = dur(t)*res(t);

      if (lct(t)>a && lct(t)-est(t)>dur(t) && maxarea<area)
	maxarea = area;
      
      if (a < ect(t)) {		/* minoverlap events */
	SP_integer m = ect(t)-a;
	if (m > dur(t))
	  m = dur(t);
	if (a >= lst(t)) {
	  SP_integer key = ((a+m)*8)+0x2; /* PLATEAU minoverlap event, a >= lst(t) */
	  mincoeff += res(t);	/* Total += res(t)*(b-a) */
	  if (key < maxb) {
	    (event+k)->key = key;
	    (event+k)->value.task = t;
	    k++;
	  }
	} else {
	  SP_integer key = ((lst(t))*8)+0x1; /* SLOPE minoverlap event, a < lst(t) */
	  if (key < maxb) {
	    (event+k)->key = key;
	    (event+k)->value.task = t;
	    k++;
	  }
	  key = ((lst(t)+m)*8)+0x2; /* PLATEAU minoverlap event, a < lst(t) */
	  if (key < maxb) {
	    (event+k)->key = key;
	    (event+k)->value.task = t;
	    k++;
	  }
	}
      }
      if (a < lct(t)) {		/* maxoverlap events */
	SP_integer m = lct(t)-a;
	if (m > dur(t))
	  m = dur(t);
	if (a >= est(t)) {
	  SP_integer key = ((a+m)*8)+0x4; /* PLATEAU maxoverlap event, a >= est(t) */
	  maxcoeff += res(t);	/* Total += res(t)*(b-a) */
	  if (key < maxb) {
	    (event+k)->key = key;
	    (event+k)->value.task = t;
	    k++;
	  }
	} else {
	  SP_integer key = ((est(t))*8)+0x3; /* SLOPE maxoverlap event, a < est(t) */
	  if (key < maxb) {
	    (event+k)->key = key;
	    (event+k)->value.task = t;
	    k++;
	  }
	  key = ((est(t)+m)*8)+0x4; /* PLATEAU maxoverlap event, a < est(t) */
	  if (key < maxb) {
	    (event+k)->key = key;
	    (event+k)->value.task = t;
	    k++;
	  }
	}
      }
    }
    nbevent = k;
    qsort_asc_event_ptrs(wam, pdata,nbevent);
    for (k=0; k<nbevent; k++) {
      struct event *ev = pdata->eventp[k];
      SP_integer key = ev->key;
      struct task *t = ev->value.task;
      SP_integer b = key>>3;
      SP_integer minslack, maxslack;
      SP_integer maxlimit = dvar_max_l(limitvar);
      SP_integer minlimit = dvar_min_l(limitvar);

      switch (key & 0x7) {
      case 0:			/* LCT event */
	maxslack = (maxlimit-mincoeff)*(b-a)-minterm;
	minslack = (minlimit-maxcoeff)*(b-a)-maxterm;
	if (maxslack < 0 || (flags&0x8 && minslack > pdata->slack)) {
	  return FALSE;
	} else if (maxslack < maxarea) {
	  struct task_interval *ti = pdata->task_interval+n;
	  struct task_interval *ti0 = ti-1;
	  ti->a = a;
	  ti->b = b;
	  ti->maxslack = maxslack;
	  ti->minslack = minslack;
	  if (n>0 && ti_subsumes(ti0,ti))
	    ;
	  else if (n>0 && ti_subsumes(ti,ti0))
	    *ti0 = *ti;
	  else
	    n++;
	}
	break;
      case 1: 			/* SLOPE minoverlap event, a < lst(t) */
	mincoeff += res(t);
	minterm += res(t)*(a - lst(t));
	break;
      case 2:			/* PLATEAU minoverlap event, a < lst(t) */
	mincoeff -= res(t);
	minterm += res(t)*(b - a);
	break;
      case 3: 			/* SLOPE maxoverlap event, a < est(t) */
	maxcoeff += res(t);
	maxterm += res(t)*(a - est(t));
	break;
      case 4:			/* PLATEAU maxoverlap event, a < est(t) */
      default:
	maxcoeff -= res(t);
	maxterm += res(t)*(b - a);
	break;
      }
    }
  }
  pdata->nbti = n;
  return TRUE;
}

static int
task_interval_filtering(Wam wam,
			       struct unary_data *pdata,
			       Dvar limitvar,
			       int flags)
{
  int i, j;
  int rc=0;
  int nbtasks = pdata->nbtasks;

  for (i=0, j=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;

    if (enable(t)==2)
      pdata->obltasks[j++] = t;
  }
  if (j<=2)
    return 0;
  pdata->nbobltasks = j;
  if (!ti_collect_intervals(wam, pdata,limitvar,flags))
    return 0x2;
  if (pdata->nbti>0) {
    ti_build_index(wam, pdata,TRUE);
    for (i=0; i<pdata->nbti; i++) {
      struct task_interval *ti = pdata->task_interval+i;
      rc |= ti_do_one_interval_up(wam, pdata, ti->a, ti->b, ti->maxslack);
      if (rc & 0x2)
	return rc;
    }
    ti_build_index(wam, pdata,FALSE);
    for (i=0; i<pdata->nbti; i++) {
      struct task_interval *ti = pdata->task_interval+i;
      rc |= ti_do_one_interval_down(wam, pdata, ti->a, ti->b, ti->maxslack);
      if (rc & 0x2)
	return rc;
    }
    if (flags&0x8) {		/* underfill check */
      for (i=0; i<pdata->nbti; i++) {
	struct task_interval *ti = pdata->task_interval+i;
	for (j=0; j<pdata->nbti_tasks; j++) {
	  struct task *t = pdata->ti_tasks[j];
	  if (lct(t)>ti->a && est(t)<ti->b) {
	    SP_integer delta = ti->minslack + max_overlap(t,ti->a,ti->b) - pdata->slack;
	    if (delta>0) {
	      rc |= must_overlap_at_least(wam, t,(delta-1)/res(t)+1,ti->a,ti->b);
	      if (rc & 0x2)
		return rc;
	    }
	  }
	}
      }
    }
  }
  return rc;
}


/* FILTERING */

static struct unary_data *
unary_alloc(Wam wam,
	    struct task *tasks,
	    int nbtasks,
	    int nbtotal,
	    int flags,
	    struct size_mem *size_mem)
{
  struct unary_data *pdata;
  int nbt2 = nbtasks*nbtasks;
  char *ptr;
  int i;
  SP_integer msize = (5*nbtasks*sizeof(struct event)
		      + 5*nbtasks*sizeof(struct event *)
		      + (4*nbtasks+4)*sizeof(struct comppart)
		      + 2*nbtasks*sizeof(struct theta_lambda_node)
		      + nbtotal*sizeof(SP_integer)
		      + 7*nbtasks*sizeof(SP_integer)
		      + (4*nbtasks+4)*sizeof(SP_integer)
		      + nbt2*sizeof(SP_integer)
		      + 2*nbtasks*sizeof(SP_integer));
  if (flags & 2) {
    msize += 4*nbtasks*sizeof(SP_integer)
          +  5*nbtasks*sizeof(struct ti_index)
          +  nbt2*sizeof(struct task_interval);
  }
  ptr = PermAlloc(sizeof(struct unary_data) + msize,char,size_mem);
  pdata = (struct unary_data *)ptr;
  ptr += sizeof(struct unary_data);
  pdata->tasks = tasks;
  pdata->nbtasks = nbtasks;
  pdata->flags = flags;
  pdata->event = (struct event *)ptr;
  ptr += 5*nbtasks*sizeof(struct event);
  pdata->eventp = (struct event **)ptr;
  ptr += 5*nbtasks*sizeof(struct event *);
  pdata->cp = (struct comppart *)ptr;
  pdata->cp2 = pdata->cp + 2*nbtasks+2;
  ptr += (4*nbtasks+4)*sizeof(struct comppart);
  pdata->minloss = (SP_integer *)ptr;
  ptr += (2*nbtasks+2)*sizeof(SP_integer);
  pdata->maxloss = (SP_integer *)ptr;
  ptr += (2*nbtasks+2)*sizeof(SP_integer);
  pdata->id_map = (SP_integer *)ptr;
  ptr += nbtotal*sizeof(SP_integer);
  pdata->tree = (struct theta_lambda_node *)ptr;
  ptr += 2*nbtasks*sizeof(struct theta_lambda_node);
  pdata->est_rank = (struct task **)ptr;
  ptr += nbtasks*sizeof(struct task *);
  pdata->lct_rank = (struct task **)ptr;
  ptr += nbtasks*sizeof(struct task *);
  pdata->aux_rank = (struct task **)ptr;
  ptr += nbtasks*sizeof(struct task *);
  pdata->tt_map = (SP_integer *)ptr;
  ptr += nbtasks*sizeof(SP_integer);
  pdata->lb = (SP_integer *)ptr;
  ptr += nbtasks*sizeof(SP_integer);
  pdata->ub = (SP_integer *)ptr;
  ptr += nbtasks*sizeof(SP_integer);
  pdata->degree = (SP_integer *)ptr;
  ptr += nbtasks*sizeof(SP_integer);
  pdata->edge = (SP_integer *)ptr;
  ptr += nbt2*sizeof(SP_integer);
  pdata->pred = (SP_integer *)ptr;
  ptr += nbtasks*sizeof(SP_integer);
  pdata->succ = (SP_integer *)ptr;
  ptr += nbtasks*sizeof(SP_integer);
  if (flags&2) {		/* extra for task intervals */
    pdata->est = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->lct = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->obltasks = (struct task **)ptr;
    ptr += nbtasks*sizeof(struct task *);
    pdata->ti_tasks = (struct task **)ptr;
    ptr += nbtasks*sizeof(struct task *);
    pdata->ti_index = (struct ti_index *)ptr;
    ptr += 5*nbtasks*sizeof(struct ti_index);
    pdata->task_interval = (struct task_interval *)ptr;
    ptr += nbt2*sizeof(struct task_interval);
  }
  SP_ASSERT(ptr==(char *)(pdata+1)+msize);
  if (nbtotal>0) {
    for (i=nbtotal-1; i>=0; i--)
      pdata->id_map[i] = -1;
    
    for (i=nbtasks-1; i>=0; i--)
      pdata->id_map[(tasks+i)->diffid] = i;
  }

  return pdata;
}

static int
unary_prune_set(Wam wam,
		struct task *t,
		TAGGED set) 
{
  return t->prune_set(wam,t, set);
}

static int
unary_prune_interval(Wam wam,
		     struct task *t,
		     SP_integer a,
		     SP_integer b)
{
  int rc = 0;
  if (est(t)>=a && est(t)<=b) {
    est(t) = b+1;
    SYNCRC(t);
  } else if (lctmax(t)>=a+t->dur && lctmax(t)<=b+t->dur) {
    lctmax(t) = a+t->dur-1;
    SYNCRC(t);
  } else if (est(t)<a && b+t->dur<lctmax(t)) {
    rc |= t->prune_interval(wam,t, a, b);
  }
  return rc;
}

#if KNAPSACK_COLUMN_METHOD
static int
unary_prune_value(Wam wam,
		  struct task *t,
		  SP_integer v) 
{
  return unary_prune_interval(wam, t,v,v);
}
#endif

static int
must_overlap_less_than(Wam wam,
		       struct task *t,
		       SP_integer lmax,
		       SP_integer inf,
		       SP_integer sup) 
{
  int rc=0;
  
  if (lmax<=dur(t) && lmax<=sup-inf)
    rc |= unary_prune_interval(wam, t, inf+lmax-dur(t), sup-lmax);
  return rc;
}

static int
unary_apply(Wam wam,
	    struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  int i, rc=0;

  for (i=0; i<nbtasks && !(rc & 0x2); i++) {
    struct task *t = pdata->tasks+i;
    SP_integer lb = pdata->lb[i];
    SP_integer ub = pdata->ub[i];

    switch (enable(t)) {
    case 2:			/* normal task */
      if (lb>est(t) || ub<lctmax(t)) {
	est(t) = lb;
	lctmax(t) = ub;
	SYNCRC(t);
      }
      break;
    case 1:			/* still optional */
      if (ub-lb < lctmax(t)-lst(t)) {
	est(t) = lb;
	lctmax(t) = ub;
	SYNCRC(t);
      }
      break;
    case 0:			/* forced inactive */
      rc |= t->propagate(wam, t); /* [MC] 4.1 was missing */
    }      
  }
  return rc;
}

/* latest start time of a task that finishes no later than deadline */
static SP_integer
limited_lst(Wam wam,
	    struct task *t,
	    SP_integer deadline)
{
  if (deadline==-CLPFD_MAXINT2) {
    return deadline;
  } else if (est(t)+dur(t)==lct(t)) {
    return ect(t) <= deadline ? lst(t) : -CLPFD_MAXINT2;
  } else {
    TAGGED tset = t->set(wam, t);
    TAGGED tlst = fd_predecessor(tset,MakeSmall(deadline-dur(t)+1));
    return (tlst!=Inf ? GetSmall(tlst) : -CLPFD_MAXINT2);
  }
}

/* earliest completion time of a task that starts no earlier than release */
static SP_integer
limited_ect(Wam wam, 
	    struct task *t,
	    SP_integer release)
{
  if (release==CLPFD_MAXINT2) {
    return release;
  } else if (est(t)+dur(t)==lct(t)) {
    return lst(t) >= release ? ect(t) : CLPFD_MAXINT2;
  } else {
    TAGGED tset = t->set(wam, t);
    TAGGED tect = fd_successor(tset,MakeSmall(release-1));
    return (tect!=Sup ? GetSmall(tect)+dur(t) : CLPFD_MAXINT2);
  }
}

/* perfect pruning for 2 tasks */
static int
unary_filtering_2(Wam wam,
		  struct task *t1,
		  struct task *t2)
{
  int rc = 0;
  SP_integer a, b;
  TAGGED *top;

  t1->refresh(wam, t1);
  t2->refresh(wam, t2);
  NumstackAlloc(0,top);
  a = lst(t2)-dur(t1)+1;
  b = ect(t2)-1;
  rc |= unary_prune_interval(wam, t1,a,b);
  a = lst(t1)-dur(t2)+1;
  b = ect(t1)-1;
  rc |= unary_prune_interval(wam, t2,a,b);
  if (rc==0)
    numstack_trim(w,top);
  return rc;
}

static int
unary_filtering_sub_3(Wam wam,
		      struct task *t1,
		      struct task *t2,
		      struct task *t3)
{
  TAGGED *top;
  int rc=0;
  SP_integer d1m1 = dur(t1)-1;
  SP_integer lst2 = lst(t2);
  SP_integer lst3 = lst(t3);
  SP_integer ect2 = ect(t2);
  SP_integer ect3 = ect(t3);
  SP_integer lst23 = limited_lst(wam, t2,lst3);
  SP_integer lst32 = limited_lst(wam, t3,lst2);
  SP_integer ect23 = limited_ect(wam, t3,ect2);
  SP_integer ect32 = limited_ect(wam, t2,ect3);
  TAGGED a, b, fr1, fr2;

  NumstackAlloc(0,top);
  if (lst23<lst32)		/* compute max */
    lst23 = lst32;
  if (ect23>ect32)		/* compute min */
    ect23 = ect32;
  a = lst23 ==-CLPFD_MAXINT2 ? Inf : MakeSmall(lst23-d1m1);
  b = ect23 == CLPFD_MAXINT2 ? Sup : MakeSmall(ect23-1);
  if (FDgt(a,b))
    goto ret;
  fr1 = fd_interval(wam, a,b);
  fr2 = fd_union(wam, fd_interval(wam, Inf,MakeSmall(ect2-1)),fd_interval(wam, MakeSmall(lst3-d1m1),Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;
  fr2 = fd_union(wam, fd_interval(wam, Inf,MakeSmall(ect3-1)),fd_interval(wam, MakeSmall(lst2-d1m1),Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;
  rc = unary_prune_set(wam, t1,fr1);
 ret:
  if (rc==0)
    numstack_trim(w,top);
  return rc;
}



/* perfect pruning for 3 tasks */
static int
unary_filtering_3(Wam wam,
		  struct task *t1,
		  struct task *t2,
		  struct task *t3)
{
  int rc = 0;

  t1->refresh(wam, t1);
  t2->refresh(wam, t2);
  t3->refresh(wam, t3);
  rc |= unary_filtering_sub_3(wam, t1,t2,t3);
  if (rc<2)
    rc |= unary_filtering_sub_3(wam, t2,t3,t1);
  if (rc<2)
    rc |= unary_filtering_sub_3(wam, t3,t1,t2);
  return rc;
}

static int
unary_filtering_sub_4(Wam wam,
		      struct task *t1,
		      struct task *t2,
		      struct task *t3,
		      struct task *t4)
{
  TAGGED *top;
  int rc=0;
  SP_integer d1m1 = dur(t1)-1;
  SP_integer lst2 = lst(t2);
  SP_integer lst3 = lst(t3);
  SP_integer lst4 = lst(t4);
  SP_integer ect2 = ect(t2);
  SP_integer ect3 = ect(t3);
  SP_integer ect4 = ect(t4);
  SP_integer lst23 = limited_lst(wam, t2,lst3);
  SP_integer lst24 = limited_lst(wam, t2,lst4);
  SP_integer lst32 = limited_lst(wam, t3,lst2);
  SP_integer lst34 = limited_lst(wam, t3,lst4);
  SP_integer lst42 = limited_lst(wam, t4,lst2);
  SP_integer lst43 = limited_lst(wam, t4,lst3);
  SP_integer ect23 = limited_ect(wam, t3,ect2);
  SP_integer ect24 = limited_ect(wam, t4,ect2);
  SP_integer ect32 = limited_ect(wam, t2,ect3);
  SP_integer ect34 = limited_ect(wam, t4,ect3);
  SP_integer ect42 = limited_ect(wam, t2,ect4);
  SP_integer ect43 = limited_ect(wam, t3,ect4);
  SP_integer lst234 = limited_lst(wam, t2,lst34);
  SP_integer lst243 = limited_lst(wam, t2,lst43);
  SP_integer lst324 = limited_lst(wam, t3,lst24);
  SP_integer lst342 = limited_lst(wam, t3,lst42);
  SP_integer lst423 = limited_lst(wam, t4,lst23);
  SP_integer lst432 = limited_lst(wam, t4,lst32);
  SP_integer ect234 = limited_ect(wam, t4,ect23);
  SP_integer ect243 = limited_ect(wam, t3,ect24);
  SP_integer ect324 = limited_ect(wam, t4,ect32);
  SP_integer ect342 = limited_ect(wam, t2,ect34);
  SP_integer ect423 = limited_ect(wam, t3,ect42);
  SP_integer ect432 = limited_ect(wam, t2,ect43);
  TAGGED a, b, fr1, fr2;

  NumstackAlloc(0,top);
  if (lst234<lst243)		/* compute max */
    lst234 = lst243;
  if (lst234<lst324)
    lst234 = lst324;
  if (lst234<lst342)
    lst234 = lst342;
  if (lst234<lst423)
    lst234 = lst423;
  if (lst234<lst432)
    lst234 = lst432;
  if (ect234>ect243)		/* compute min */
    ect234 = ect243;
  if (ect234>ect324)
    ect234 = ect324;
  if (ect234>ect342)
    ect234 = ect342;
  if (ect234>ect423)
    ect234 = ect423;
  if (ect234>ect432)
    ect234 = ect432;

  a = lst234 ==-CLPFD_MAXINT2 ? Inf : MakeSmall(lst234-d1m1);
  b = ect234 == CLPFD_MAXINT2 ? Sup : MakeSmall(ect234-1);
  if (FDgt(a,b))
    goto ret;
  fr1 = fd_interval(wam, a,b);

  if (lst34<lst43)
    lst34 = lst43;
  a = lst34 ==-CLPFD_MAXINT2 ? Inf : MakeSmall(lst34-d1m1);
  fr2 = fd_union(wam, fd_interval(wam, Inf,MakeSmall(ect2-1)),fd_interval(wam, a,Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;

  if (lst24<lst42)
    lst24 = lst42;
  a = lst24 ==-CLPFD_MAXINT2 ? Inf : MakeSmall(lst24-d1m1);
  fr2 = fd_union(wam, fd_interval(wam, Inf,MakeSmall(ect3-1)),fd_interval(wam, a,Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;

  if (lst23<lst32)
    lst23 = lst32;
  a = lst23 ==-CLPFD_MAXINT2 ? Inf : MakeSmall(lst23-d1m1);
  fr2 = fd_union(wam, fd_interval(wam, Inf,MakeSmall(ect4-1)),fd_interval(wam, a,Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;

  if (ect34>ect43)
    ect34 = ect43;
  b = ect34 == CLPFD_MAXINT2 ? Sup : MakeSmall(ect34-1);
  fr2 = fd_union(wam, fd_interval(wam, Inf,b),fd_interval(wam, MakeSmall(lst2-d1m1),Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;

  if (ect24>ect42)
    ect24 = ect42;
  b = ect24 == CLPFD_MAXINT2 ? Sup : MakeSmall(ect24-1);
  fr2 = fd_union(wam, fd_interval(wam, Inf,b),fd_interval(wam, MakeSmall(lst3-d1m1),Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;

  if (ect23>ect32)
    ect23 = ect32;
  b = ect23 == CLPFD_MAXINT2 ? Sup : MakeSmall(ect23-1);
  fr2 = fd_union(wam, fd_interval(wam, Inf,b),fd_interval(wam, MakeSmall(lst4-d1m1),Sup));
  fr1 = fd_intersection(wam, fr1,fr2);
  if (fr1==EmptySet)
    goto ret;

  rc = unary_prune_set(wam, t1,fr1);
 ret:
  if (rc==0)
    numstack_trim(w,top);
  return rc;
}



/* perfect pruning for 4 tasks */
static int
unary_filtering_4(Wam wam,
		  struct task *t1,
		  struct task *t2,
		  struct task *t3,
		  struct task *t4)
{
  int rc = 0;

  t1->refresh(wam, t1);
  t2->refresh(wam, t2);
  t3->refresh(wam, t3);
  t4->refresh(wam, t4);
  rc |= unary_filtering_sub_4(wam, t1,t2,t3,t4);
  if (rc<2)
    rc |= unary_filtering_sub_4(wam, t2,t3,t4,t1);
  if (rc<2)
    rc |= unary_filtering_sub_4(wam, t3,t4,t1,t2);
  if (rc<2)
    rc |= unary_filtering_sub_4(wam, t4,t1,t2,t3);
  return rc;
}

/* flags: 0x2 - use task intervals and "slack filtering" methods
          0x4 - use dynamic programming methods
          0x8 - all heights and durations are fixed
 */
/* DOES NOT find a fixpoint */
static int
unary_filtering_general(Wam wam,
			struct task *tasks,
			int nbtasks,
			int nbtotal,
			int flags,
			Dvar diffvar,
			struct diff_constraint *dc,
			int nbdiffs,
			SP_integer *lst,
			SP_integer *ect,
			struct size_mem *size_mem)
{
  struct unary_data *pdata;
  int rc=0, i;

  if (nbtasks==1 && (flags&0x8)) {
    *lst = lst(tasks);
    *ect = ect(tasks);
    return 0;
  }

  pdata = unary_alloc(wam, tasks,nbtasks,nbtotal,flags,size_mem);
  for (i=nbtasks-1; i>=0; i--) {
    pdata->lb[i] = est(tasks+i);
    pdata->ub[i] = lctmax(tasks+i);
  }
  tt_init(wam, pdata);
  if (!(unary_detect_precedences(wam, pdata) &&
	unary_not_first_last(wam, pdata) &&
	unary_first_last(pdata,lst,ect))) {
    rc = 0x2;
    goto ret;
  }
  rc |= unary_apply(wam, pdata);
  if (rc==0 && nbdiffs>0) {
    init_ranks(wam, pdata);	/* [MC] 4.4 needs refreshing */
    rc |= unary_detect_all_precedences(wam, pdata,diffvar,dc,nbdiffs);
    rc |= unary_apply(wam, pdata);
  }
 ret:
  PermFree(pdata,size_mem);
  return rc;
}

static void
insert_sort_by_est(struct task **array, int n)
{
  int i, j;

  for (i=1; i<n; i++) {
    struct task *t = array[i];
    for (j=i; j>0 && est(array[j-1])>est(t); j--)
      array[j] = array[j-1];
    array[j] = t;
  }
}

static void
insert_sort_by_lct(struct task **array, int n)
{
  int i, j;

  for (i=1; i<n; i++) {
    struct task *t = array[i];
    for (j=i; j>0 && lct(array[j-1])<lct(t); j--)
      array[j] = array[j-1];
    array[j] = t;
  }
}

static void
unary_lst_ect(struct task **array, int n, SP_integer *lst, SP_integer *ect)
{
  SP_integer lst1 =  CLPFD_MAXINT2;
  SP_integer ect1 = -CLPFD_MAXINT2;
  int i, j;
  
  insert_sort_by_est(array, n);
  for (i=0; i<n; i++) {
    SP_integer ect2 = est(array[i]);
    for (j=i; j<n; j++)
      ect2 += dur(array[j]);
    ect1 = ect2 > ect1 ? ect2 : ect1;
  }
  
  insert_sort_by_lct(array, n);
  for (i=0; i<n; i++) {
    SP_integer lst2 = lct(array[i]);
    for (j=i; j<n; j++)
      lst2 -= dur(array[j]);
    lst1 = lst2 < lst1 ? lst2 : lst1;
  }

  *lst = lst1;
  *ect = ect1;
}



static int
unary_filtering(Wam wam,
		struct task *tasks,
		int nbtasks,
		int nbtotal,
		int flags,
		Dvar diffvar,
		struct diff_constraint *dc,
		int nbdiffs,
		SP_integer *lst,
		SP_integer *ect,
		struct size_mem *size_mem)
{
  int rc=0, i, nbground=0;
  struct task *array[] = {NULL,NULL,NULL,NULL};
  SP_integer lst1 =  CLPFD_MAXINT2;
  SP_integer ect1 = -CLPFD_MAXINT2;
  SP_integer lst2 =  CLPFD_MAXINT2;
  SP_integer ect2 = -CLPFD_MAXINT2;

  if (!(flags&0x8) || nbdiffs>0)
    return unary_filtering_general(wam, tasks,nbtasks,nbtotal,flags,diffvar,dc,nbdiffs,lst,ect,size_mem);

  /* count residual nonground tasks */
  for (i=0; i<nbtasks; i++) {
    struct task *ti = tasks+i;
    if (est(ti)+dur(ti)==lct(ti)) {
      nbground++;
      lst1 = est(ti) < lst1 ? est(ti) : lst1;
      ect1 = lct(ti) > ect1 ? lct(ti) : ect1;
    } else {
      array[3]=array[2]; array[2]=array[1]; array[1]=array[0]; array[0]=ti;
    }
  }

  switch (nbtasks-nbground) {
  case 0:
    break;
  case 1:
    unary_lst_ect(array,1,&lst2,&ect2);
    break;
  case 2:
    rc |= unary_filtering_2(wam, array[0],array[1]);
    unary_lst_ect(array,2,&lst2,&ect2);
    break;
  case 3:
    rc |= unary_filtering_3(wam, array[0],array[1],array[2]);
    unary_lst_ect(array,3,&lst2,&ect2);
    break;
  case 4:
    rc |= unary_filtering_4(wam, array[0],array[1],array[2],array[3]);
    unary_lst_ect(array,4,&lst2,&ect2);
    break;
  default:
    rc |= unary_filtering_general(wam, tasks,nbtasks,nbtotal,flags,diffvar,dc,nbdiffs,&lst2,&ect2,size_mem);
    break;
  }
  *lst = lst1 < lst2 ? lst1 : lst2;
  *ect = ect1 > ect2 ? ect1 : ect2;
  return rc;
}


#if CONSECUTIVE_STAIRS_METHOD
/* Normalization rules:
   - First segment is <-infty,0>
   - Last  segment is <+infty,0>
   - No consecutive segments of the same height, except
     last but one may have zero height.
*/
static SP_BOOL
fixed_parts(Wam wam,
		   struct unary_data *pdata,
		   SP_integer start,
		   SP_integer end)
{
  int nbtasks = pdata->nbtasks;
  struct task *tasks = pdata->tasks;
  struct event *event = pdata->event;
  struct comppart *cp = pdata->cp;
  SP_integer height0, height;
  int i, j, nbev;

  for (i=0, j=0; i<nbtasks; i++) {
    struct task *t = tasks+i;
    
    if (enable(t)==2 &&
	est(t) == lst(t) &&
	est(t) < end &&
	lct(t) > start) {
      event[j].key = ((est(t) > start ? est(t) : start)*2)+1;
      event[j].value.task = t;
      event[j+1].key = ((lct(t) < end ? lct(t) : end)*2);
      event[j+1].value.task = t;
      j += 2;      
    }
  }
  nbev = j;
  start = -CLPFD_MAXINT;
  height0 = -CLPFD_MAXINT;
  height = 0;
  qsort_asc_event_ptrs(wam, pdata,nbev);
  for (i=0, j=0; j<nbev; j++) {
    struct event *ev = pdata->eventp[j];
    SP_integer key = ev->key;
    SP_integer evdate = key>>1;
    struct task *t = ev->value.task;

    if (start<evdate && height0!=height) {
      cp[i].start = start;
      cp[i++].height = height;
      height0 = height;
    }
    start = evdate;
    if (key & 1)		/* start event */
      height += res(t);
    else			/* end event */
      height -= res(t);
  }
  if (height0!=height) {
    cp[i].start = start;
    cp[i++].height = height;
  }
  cp[i].start = CLPFD_MAXINT;
  cp[i++].height = 0;
  return TRUE;
}

static int 
slack_fixed_parts(Wam wam,
		  struct unary_data *pdata,
		  SP_integer est,
		  SP_integer lct)
{
  struct comppart *cp = pdata->cp;
  int p, q;
  
  fixed_parts(wam, pdata,-CLPFD_MAXINT,CLPFD_MAXINT);
  if (cp[1].start==est) {
    q = 0;
  } else {
    cp[0].start = est;
    cp[0].height = 0;
    q = 1;
  }
  for (p=1; cp[p].start < lct; p++, q++)
    cp[q] = cp[p];
  cp[q].start = lct;
  cp[q].height = 0;
  return q;
}

static SP_BOOL
consecutive_stairs(struct unary_data *pdata,
		   int p,
		   SP_integer limit)
{
  SP_integer i0 = pdata->cp[p  ].start;
  SP_integer i1 = pdata->cp[p+1].start;
  SP_integer i2 = pdata->cp[p+2].start;
  SP_integer l0 = i1 - i0;
  SP_integer g0 = limit-pdata->cp[p  ].height;
  SP_integer g1 = limit-pdata->cp[p+1].height;
  int card=0, i;
  SP_integer sum_surf1=0, sum_surf2=pdata->slack, emin=0, dmin, hmin, sumlim;

  for (i=0; i<pdata->nbtasks; i++) {
    struct task *t = pdata->tasks+i;

    if (est(t)!=lst(t)) {
      if (est(t)<i0)
	return TRUE;
      if (est(t)<i1 && ect(t)>i1) {
	if (card==0 || ect(t)<emin)
	  emin = ect(t);
	card++;
      }
      if (ect(t)<=i1)
	sum_surf1 += dur(t)*res(t);
    }
  }
  if (card==0)
    return TRUE;
  dmin = FDMIN(emin,i2) - i1;
  hmin = g0 - (pdata->slack+sum_surf1)/l0;
  if (g1-hmin > g0)
    return TRUE;
  sumlim = g0*l0 + (g1-hmin)*dmin;
  for (i=0; i<pdata->nbtasks; i++) {
    struct task *t = pdata->tasks+i;

    if (est(t)!=lst(t) && est(t)<i2) {
      SP_integer inc = dur(t);
      if (inc > l0+dmin)
	inc = l0+dmin;
      if (inc > i2-est(t))
	inc = i2-est(t);
      sum_surf2 += inc*res(t);
      if (sum_surf2>=sumlim)
	return TRUE;
    }
  }
  return FALSE;
}
#endif

/* Normalization rules:
   - First segment is <-infty,0>
   - Last  segment is <+infty,0>
   - No consecutive segments of the same height, except
     last but one may have zero height.
*/
static SP_BOOL
compulsory_parts(Wam wam,
			struct unary_data *pdata,
			struct task *except,
			SP_integer start,
			SP_integer end,
			Dvar limitvar)
{
  int nbtasks = pdata->nbtasks;
  struct task *tasks = pdata->tasks;
  struct event *event = pdata->event;
  SP_integer *udate = (SP_integer *)pdata->eventp;
  struct comppart *cp = pdata->cp;
  SP_integer height0, height;
  int i, j, k, nbev;

  for (i=0, j=0; i<nbtasks; i++) {
    struct task *t = tasks+i;
    
    if (t!=except &&
	enable(t)==2 &&
	lst(t) < ect(t) &&
	lst(t) < end &&
	ect(t) > start) {
      event[j].key = ((lst(t) > start ? lst(t) : start)*2)+1;
      event[j].value.task = t;
      event[j+1].key = ((ect(t) < end ? ect(t) : end)*2);
      event[j+1].value.task = t;
      j += 2;      
    }
  }
  nbev = j;
  start = -CLPFD_MAXINT;
  height0 = -CLPFD_MAXINT;
  height = 0;
  qsort_asc_event_ptrs(wam, pdata,nbev);
  for (i=0, j=0, k=0; j<nbev; j++) {
    struct event *ev = pdata->eventp[j];
    SP_integer key = ev->key;
    SP_integer evdate = key>>1;
    struct task *t = ev->value.task;

    if (start<evdate)
      udate[k++] = start;
    if (start<evdate && height0!=height) {
      if (dvar_fix_min_l(limitvar,height)<0)
	return FALSE;
      cp[i].start = start;
      cp[i++].height = height;
      height0 = height;
    }
    start = evdate;
    if (key & 1)		/* start event */
      height += res(t);
    else			/* end event */
      height -= res(t);
  }
  udate[k++] = start;
  udate[k++] = CLPFD_MAXINT;
  if (height0!=height) {
    cp[i].start = start;
    cp[i++].height = height;
  }
  cp[i].start = CLPFD_MAXINT;
  cp[i++].height = 0;
  return TRUE;
}

static int
comppart_emit(SP_integer start,
	     SP_integer height,
	     struct comppart *cp,
	     int e2)
{
  cp[e2].start = start;
  cp[e2++].height = height;
  if (e2>=2 && cp[e2-2].start==cp[e2-1].start) {
    e2--;
    cp[e2-1].height = cp[e2].height;
  }
  if (e2>=2 && cp[e2-2].height==cp[e2-1].height) {
    e2--;
  }
  return e2;
}

static void
compulsory_part_delete(struct unary_data *pdata,
		       SP_integer start,
		       SP_integer end,
		       SP_integer delta)
{
  struct comppart *cp = pdata->cp;
  struct comppart *cp2 = pdata->cp2;
  int e1, e2;

  e1 = 0;
  e2 = 0;
  while (cp[e1+1].start <= start) {
    e2 = comppart_emit(cp[e1].start, cp[e1].height, cp2, e2);
    e1++;
  }

  /* emit crumb before start (maybe empty) */
  e2 = comppart_emit(cp[e1].start, cp[e1].height, cp2, e2);

  /* emit subtracted except last part */
  while (cp[e1+1].start <= end) {
    e2 = comppart_emit(start, cp[e1].height - delta, cp2, e2);
    start = cp[e1+1].start;
    e1++;
  }

  /* emit subtracted, last part (maybe empty) */
  e2 = comppart_emit(start, cp[e1].height - delta, cp2, e2);
  start = end;

  /* emit everything after subtracted */
  while (start < CLPFD_MAXINT) {
    e2 = comppart_emit(start, cp[e1].height, cp2, e2);
    start = cp[e1+1].start;
    e1++;
  }
  cp2[e2++] = cp[e1++];
}


static int
cp_task_profile(Wam wam,
		       struct unary_data *pdata,
		Dvar limitvar) 
{
  SP_integer limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  int i, rc=0;

  if (!compulsory_parts(wam, pdata,NULL,-CLPFD_MAXINT,CLPFD_MAXINT,limitvar))
    rc |= 2;
  for (i=0; i<nbtasks && rc<2; i++) {
    struct task *t = pdata->tasks+i;
    SP_integer est = est(t);
    SP_integer lctmax = lctmax(t);
    SP_integer limt = limit - res(t);
    TAGGED prune = EmptySet;
    struct comppart *cp;
    int exact = (enable(t)==2 && lct(t)==lctmax(t)); /* fixed duration */
    int e1;
    
    if (ect(t)==lctmax(t))
      continue;
    if (lst(t) < ect(t) && enable(t)==2) {
      compulsory_part_delete(pdata,lst(t),ect(t),res(t));
      cp = pdata->cp2;
    } else {
      cp = pdata->cp;
    }
    for (e1=0; cp[e1].start < lctmax(t); e1++) {
      if (cp[e1].height>limt) {
	if (ect(t)>cp[e1].start && est(t)<cp[e1+1].start)
	  est(t) = cp[e1+1].start;
	if (exact) {
	  if (cp[e1+1].start==CLPFD_MAXINT)
	    prune = fd_union_interval(wam, prune, MakeSmall(cp[e1].start-dur(t)+1), MakeSmall(lst(t)));
	  else if (cp[e1+1].start > est(t))
	    prune = fd_union_interval(wam, prune, MakeSmall(cp[e1].start-dur(t)+1), MakeSmall(cp[e1+1].start-1));
	}
      }
    }
    for (e1--; cp[e1+1].start > est(t); e1--) {
      if (cp[e1].height>limt) {
	if (lctmax(t)-dur(t)<cp[e1+1].start && lctmax(t)>cp[e1].start)
	  lctmax(t) = cp[e1].start;
      }
    }
    if (est<est(t) || lctmax>lctmax(t)) {
      SYNCRC(t);
    }
    if (prune!=EmptySet && rc<2)
      rc |= unary_prune_set(wam, t,prune);
  }
  return rc;
}

static SP_BOOL
can_place(struct task *t,
	  struct comppart *cp,
	  SP_integer limtu)
{
  int e1 = 0;
  SP_integer lb = est(t);
  
  while (cp[e1+1].start <= est(t))
    e1++;
  /* cp[e1].start <= est(t) < cp[e1+1].start */
  while (cp[e1].start < lb+dur(t)) {
    if (cp[e1].height > limtu)
      lb = cp[e1+1].start;
    e1++;
  }
  return (lst(t) >= lb);
}      

static int 
dr_cp_ends(Wam wam,
		  struct unary_data *pdata,
		  Dvar limitvar)
{
  SP_integer limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  struct comppart *cp = pdata->cp;
  int i, j, rc=0;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (ect(t) == lct(t))
      continue;
    if (!compulsory_parts(wam, pdata,t,lst(t),lct(t),limitvar))
      return 0x2;
    for (j=0; j<nbtasks && rc==0; j++) {
      struct task *u = pdata->tasks+j;
      SP_integer limtu;
      
      if (enable(u)<2 || u==t || ect(u) <= lst(t) || lst(u) >= lct(t))
	continue;
      limtu = limit - res(t) - res(u);
      if (limtu<0) {
	lctmax(t) = lst(u);
	SYNCRC(t);
      } else if (lst(u) >= ect(u) && !can_place(u,cp,limtu)) {
	int e1 = 0;
	SP_integer ub = lctmax(t);

	while (cp[e1+1].start < ub)
	  e1++;
	/* cp[e1].start < lctmax(t) <= cp[e1+1].start */
	while (cp[e1+1].start > ub-dur(t) && cp[e1+1].start > lst(u)) {
	  if (cp[e1].height > limtu)
	    ub = cp[e1].start > lst(u) ? cp[e1].start : lst(u);
	  e1--;
	}
	lctmax(t) = ub;
	SYNCRC(t);
      }
    }
  }
  return rc;
}

static int 
dr_cp_starts(Wam wam,
		    struct unary_data *pdata,
		    Dvar limitvar)
{
  SP_integer limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  struct comppart *cp = pdata->cp;
  int i, j, rc=0;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (ect(t) == lct(t))
      continue;
    if (!compulsory_parts(wam, pdata,t,est(t),ect(t),limitvar))
      return 0x2;
    for (j=0; j<nbtasks && rc==0; j++) {
      struct task *u = pdata->tasks+j;
      SP_integer limtu;
      
      if (enable(u)<2 || u==t || ect(u) <= est(t) || lst(u) >= ect(t))
	continue;
      limtu = limit - res(t) - res(u);
      if (limtu<0) {
	est(t) = ect(u);
	SYNCRC(t);
      } else if (lst(u) >= ect(u) && !can_place(u,cp,limtu)) {
	int e1 = 0;
	SP_integer lb = est(t);

	while (cp[e1+1].start <= est(t))
	  e1++;
	/* cp[e1].start <= est(t) < cp[e1+1].start */
	while (cp[e1].start < lb+dur(t) && cp[e1].start < ect(u)) {
	  if (cp[e1].height > limtu)
	    lb = cp[e1+1].start < ect(u) ? cp[e1+1].start : ect(u);
	  e1++;
	}
	est(t) = lb;
	SYNCRC(t);
      }
    }
  }
  return rc;
}


/* FILTERING */

/* use unary methods from inside discrete filtering */
static SP_BOOL
cp_interference_graph(Wam wam,
			     struct unary_data *pdata,
			     Dvar limitvar)
{
  SP_integer limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  int i, j;

  if (!compulsory_parts(wam, pdata,NULL,-CLPFD_MAXINT,CLPFD_MAXINT,limitvar))
    return FALSE;
  for (i=0; i<nbtasks-1; i++) {
    struct task *t = pdata->tasks+i;
    for (j=i+1; j<nbtasks; j++) {
      struct task *u = pdata->tasks+j;
      SP_integer limtu = limit - res(t) - res(u);

      pdata->edge[nbtasks*i + j] = 0;
      pdata->edge[nbtasks*j + i] = 0;
      if (limtu<0 || est(u)>=lctmax(t) || lctmax(u)<=est(t)) {
	goto edge;
      } else if (lst(t)<ect(t) && lst(t)<ect(u) && lst(u)<ect(t) && lst(u)<ect(u)) {
	goto noedge;
      } else {
	SP_integer a = est(t) > est(u) ? est(t) : est(u);
	SP_integer b = lctmax(t) < lctmax(u) ? lctmax(t) : lctmax(u);
	SP_integer tcpa = lst(t) > a ? lst(t) : a;
	SP_integer tcpb = ect(t) < b ? ect(t) : b;
	SP_integer ucpa = lst(u) > a ? lst(u) : a;
	SP_integer ucpb = ect(u) < b ? ect(u) : b;
	SP_integer total;
	int e1=0, nbev0=0, nbev;
	while (pdata->cp[e1+1].start <= a)
	  e1++;

	/* pdata->cp[e1].start <= a < pdata->cp[e1+1].start */
	/* Sufficient (can sharpen) condition for interference between t and u:
	   Profile\{t,u} > limtu for all points in [a,b) */

	total = pdata->cp[e1].height;
	if (total<=limtu)
	  goto noedge;
	pdata->event[nbev0].key = (a*2);
	pdata->event[nbev0++].value.delta = total;
	while (pdata->cp[e1+1].start < b) {
	  SP_integer delta = pdata->cp[e1+1].height - total;
	  total += delta;
	  if (total<=limtu)
	    goto noedge;
	  pdata->event[nbev0].key = (pdata->cp[e1+1].start*2)+(delta<0);
	  pdata->event[nbev0++].value.delta = delta;
	  e1++;
	}
	nbev = nbev0;
	if (tcpa<tcpb) {
	  pdata->event[nbev].key = (tcpa*2)+1;
	  pdata->event[nbev++].value.delta = -res(t);
	  pdata->event[nbev].key = (tcpb*2);
	  pdata->event[nbev++].value.delta = res(t);
	}
	if (ucpa<ucpb) {
	  pdata->event[nbev].key = (ucpa*2)+1;
	  pdata->event[nbev++].value.delta = -res(u);
	  pdata->event[nbev].key = (ucpb*2);
	  pdata->event[nbev++].value.delta = res(u);
	}
	if (nbev0 < nbev) {
	  qsort_asc_event_ptrs(wam, pdata,nbev);
	  total = 0;
	  for (e1=0; e1<nbev; e1++) {
	    total += pdata->eventp[e1]->value.delta;
	    if (total<=limtu)
	      goto noedge;
	  }
	}
	goto edge;
      }
	
      edge:
	pdata->edge[nbtasks*i + j] = 1;
	pdata->edge[nbtasks*j + i] = 1;
	pdata->degree[i]++;
	pdata->degree[j]++;
      noedge:
	continue;
    }
  }
  return TRUE;
}


static int
disj_2_clique(Wam wam,
	      struct task *ti,
	      struct task *tj) 
{
  int rc = 0;
  if (enable(ti)==2) {		/* ti is obligatory */
    if (enable(tj)==2 && lct(tj)==lctmax(tj)) { /* fixed duration */
      rc |= unary_prune_interval(wam, tj,lst(ti)-dur(tj)+1,ect(ti)-1);
    } else {		/* non-fixed duration */
      if (ect(tj)>lst(ti) && est(tj)<ect(ti)) {
	est(tj) = ect(ti);
	SYNCRC(tj);
      }
      if (lst(tj)<ect(ti) && lctmax(tj)>lst(ti)) {
	lctmax(tj) = lst(ti);
	SYNCRC(tj);
      }
    }
  }
  return rc;
}


static int
disjunctive_tasks(Wam wam,
		  struct unary_data *pdata,
		  struct task **map,
		  Dvar limitvar,
		  int *nbutasks)
{
  int i, j=0, rc=0;
  int nbtasks = pdata->nbtasks;
  SP_integer limit = dvar_max_l(limitvar);
  
  for (i=0; i<nbtasks; i++) {
    pdata->degree[i] = 0;
    pdata->edge[nbtasks*i + i] = 0;
  }
  /* fast case: is it the "serialized" case? */
  for (j=0; j<nbtasks; j++)
    if (pdata->tasks[j].res == limit)
      map[j] = &pdata->tasks[j];
    else
      goto clique;
  for (i=0; i<nbtasks-1; i++)
    for (j=i+1; j<nbtasks; j++) {
      pdata->edge[nbtasks*i + j] = 1;
      pdata->edge[nbtasks*j + i] = 1;
    }
  goto retj;
 clique:
  /* build a graph with edge<=>disjunction, */
  /* then find clique using the natural greedy algorithm: */
  /* pick node with max. degree, break ties by max. height */
  if (!cp_interference_graph(wam, pdata,limitvar)) {
    j = 0;
    rc = 2;
  } else {
    for (j=0; j<nbtasks; j++) {
      int best = 0;
      for (i=1; i<nbtasks; i++)
	if (pdata->degree[i]>pdata->degree[best] ||
	    (pdata->degree[i]==pdata->degree[best] &&
	     pdata->tasks[i].res>pdata->tasks[best].res))
	  best = i;
      if (pdata->degree[best]==0)
	break;
      map[j] = &pdata->tasks[best];
      for (i=0; i<nbtasks; i++)
	if (!pdata->edge[nbtasks*best + i])
	  pdata->degree[i] = 0;
    }
  }
 retj:
  *nbutasks = j;
  /* treat all 2-cliques */
  for (i=0; i<nbtasks-1 && rc<2; i++)
    for (j=i+1; j<nbtasks && rc<2; j++)
      if (pdata->edge[nbtasks*i + j]) {
	struct task *ti = pdata->tasks+i;
	struct task *tj = pdata->tasks+j;
	if (lctmax(ti)>est(tj) && lctmax(tj)>est(ti))
	  rc |= disj_2_clique(wam, ti,tj) | disj_2_clique(wam, tj,ti);
      }
  return rc;
}

/* DOES NOT find a fixpoint */
static int
unary_filtering_rec(Wam wam,
		    struct task **map,
		    int nbtasks,
		    int nbtotal,
		    int flags,
		    Dvar diffvar,
		    struct diff_constraint *dc,
		    int nbdiffs,
		    SP_integer *lst,
		    SP_integer *ect,
		    struct size_mem *size_mem)
{
  if (nbtasks==0) {
    *lst =  CLPFD_MAXINT2;
    *ect = -CLPFD_MAXINT2;
    return 0;
  } else {
    struct task *tasks = Malloc(nbtasks,struct task);
    int i, rc=0;
    
    for (i=nbtasks-1; i>=0; i--) {
      tasks[i] = *map[i];
      tasks[i].res = 1;
    }
    rc |= unary_filtering(wam, tasks,nbtasks,nbtotal,flags,diffvar,dc,nbdiffs,lst,ect,size_mem);
    if (rc>0)
      for (i=nbtasks-1; i>=0; i--) {
	map[i]->est = tasks[i].est;
	map[i]->lct = tasks[i].lct;
	map[i]->lctmax = tasks[i].lctmax;
	map[i]->enable = tasks[i].enable;
      }
    Free(tasks);
    return rc;
  }
}

#if PROFILE_METHOD
static SP_integer
max_inters(SP_integer min1, SP_integer max1, SP_integer inf, SP_integer sup, SP_integer len)
{
  SP_integer j1 = max1 - inf;
  SP_integer j2 = sup - min1;
  if (j1 > j2) j1 = j2;
  if (j1 > len) j1 = len;
  if (j1 > sup-inf) j1 = sup-inf;
  return j1 < 0 ? 0 : j1;
}

static int
intersects_interval(TAGGED set,
		    SP_integer min,
		    SP_integer max)
{
  FDITER it;
  TAGGED tmin, tmax;

  fditer_init(&it, set);
  while (!fditer_empty(&it)) {
    fditer_next_interval(&it, &tmin, &tmax);
    if (max<GetSmall0(tmin))
      return FALSE;
    else if (GetSmall0(tmax)>=min)
      return TRUE;
  }
  return FALSE;
}

static SP_integer
task_max_intersection(Wam wam,
		      struct task *t,
		      SP_integer inf,
		      SP_integer sup)
{
  SP_integer maxi = 0;
  
  if (t->is_interval(wam, t)) {
    maxi = max_inters(est(t), lct(t), inf, sup, dur(t));
  } else {
    TAGGED tmin, tmax;
    SP_integer dmin, dmax, inters, len=dur(t);
    FDITER it;

    fditer_init(&it, t->set(wam, t));
    while (!fditer_empty(&it) && maxi<len) {
      fditer_next_interval(&it, &tmin, &tmax);
      dmin = GetSmall(tmin);
      dmax = GetSmall(tmax);
      if (dmax+len>=inf && sup>=dmin) {
	inters = max_inters(dmin, dmax+len, inf, sup, len);
	if (maxi<inters)
	  maxi = inters;
      }
    }
  }
  return maxi;
}

static SP_integer
smallest_total_height_le(Wam wam,
			 struct unary_data *pdata,
			 SP_integer inf,
			 SP_integer sup,
			 SP_integer eps)
{
  int nbtasks = pdata->nbtasks;
  TAGGED set, *top;
  int i, j, nb, nbevent;
  SP_integer total=0;

  NumstackAlloc(0,top);
  set = fd_pair(wam, TaggedZero,TaggedZero);
  for (i=0, j=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    if ((res(t)<=eps) && ((inf<lst(t) && est(t)<sup) || (inf<lct(t) && ect(t)<sup))) {
      pdata->event[j].key = res(t);
      pdata->event[j++].value.delta = 1;
    }
  }
  nbevent = j;  
  qsort_asc_event_ptrs(wam, pdata,nbevent);

  for (i=0, nb=0; i<nbevent; i++) {
    if (i==0)
      nb++;
    else if (pdata->eventp[i-1]->key<pdata->eventp[i]->key)
      pdata->eventp[nb++] = pdata->eventp[i];
    else
      pdata->eventp[nb-1]->value.delta++;
  }

  for (i=nb-1; i>=0 && total<eps; i--) {
    SP_integer key = pdata->eventp[i]->key;
    int count = (int)pdata->eventp[i]->value.delta;
    set = fd_plus(wam, set, fd_multiples(wam, count,key), TaggedZero, MakeSmall(eps));
    total = GetSmall(fd_max(set));
  }
  numstack_trim(w,top);
  return total;
}


static SP_integer
get_minloss(Wam wam,
		  SP_integer len,
		  SP_integer low,
		  SP_integer up,
		  SP_integer eps,
		  SP_integer sigma,
		  lh_method *lh_method,
		  int closed)
{
  SP_integer lhlow = (*lh_method)(wam, low,eps,closed);
  SP_integer lhup  = (*lh_method)(wam, up,eps,closed);
  if (lhup<len) {
    return sigma+1;
  } else if (lhlow<len) {
    while (low<up) { /* compute low = smallest loss for this interval that does not violate longest hole table */
      SP_integer mid = (low+up)>>1;
      if ((*lh_method)(wam, mid,eps,closed)<len) {
	low = mid+1;
      } else {
	up = mid;
      }
    }
  }
  return low;
}



/* compute the min and max loss for every step of the profile */
static int
compulsory_profile_minloss(Wam wam,
			   struct unary_data *pdata,
			   int nbstep, /* of cp */
			   SP_integer limit,
			   lh_method *lh_method,
			   int flags)
{
  int i;

  for (i=0; i<nbstep; i++) {
    pdata->minloss[i] = 0;
    pdata->maxloss[i] = pdata->resslack;
  }

  if (pdata->resslack>0) {
    struct comppart *cp = pdata->cp;
    struct profile *prof;
    int nbtasks=pdata->nbtasks;
    int inside, step;
    SP_integer old, new, butmin, butmax;

    /* Phase 1: naive method based on placement space of each task */
    
    pdata->profile = fd_empty_profile();
  
    for (i=0; i<nbtasks; i++) {
      struct task *t = pdata->tasks+i;
    
      if (t->is_interval(wam, t)) {
	pdata->profile = fd_profile_update(wam, pdata->profile,est(t),lct(t),res(t));
      } else {
	TAGGED tmin, tmax;
	SP_integer dmin, dmax, done=est(t);
	FDITER it;
      
	fditer_init(&it, t->set(wam, t));
	while (!fditer_empty(&it)) {
	  fditer_next_interval(&it, &tmin, &tmax);
	  dmin = GetSmall(tmin) > done ? GetSmall(tmin) : done;
	  dmax = GetSmall(tmax)+dur(t);
	  pdata->profile = fd_profile_update(wam, pdata->profile,dmin,dmax,res(t));
	  done = dmax;
	}
      }
    }
    prof = pdata->profile;
    inside = 0;
    step = 0;
    for (old=cp[0].start; step<nbstep; old=new) {
      if (!prof ||
	  (inside && cp[step+1].start<=prof->end) ||
	  (!inside && cp[step+1].start<=prof->begin)) {
	new = cp[step+1].start;
	if (!inside)
	  pdata->minloss[step] += (new-old)*limit;
	else if (prof->erg<limit)
	  pdata->minloss[step] += (new-old)*(limit-prof->erg);
	step++;
      } else if (inside) {
	new = prof->end;
	if (prof->erg<limit)
	  pdata->minloss[step] += (new-old)*(limit-prof->erg);
	prof = prof->next;
	inside = 0;
      } else {
	new = prof->begin;
	pdata->minloss[step] += (new-old)*limit;
	inside = 1;
      }
    }
    fd_profile_dispose(wam, pdata->profile);

    /* Phase 2: detect loss lower bound based on longest hole table */

    butmin = pdata->resslack;
    for (i=0; i<nbstep; i++) {
      butmin -= pdata->minloss[i];
    }
    if (butmin<0)
      return FALSE;
    for (i=0; i<nbstep; i++) {
      SP_integer low = pdata->minloss[i];
      SP_integer up  = pdata->maxloss[i];
      SP_integer eps = limit-cp[i].height;
      SP_integer len = cp[i+1].start - cp[i].start;
      int  closed = (i==0 || i==nbstep-1);
      SP_integer minloss_before = 0;
      SP_integer minloss_after = 0;
      SP_integer minloss_within = get_minloss(wam, len,low,up,eps,pdata->slack,lh_method,closed);
      SP_integer maxloss_but_within;
      if (i>0)
	minloss_before = get_minloss(wam, cp[i].start-cp[0].start,0,pdata->slack,limit,pdata->slack,lh_method,TRUE);
      if (i<nbstep-1)
	minloss_after = get_minloss(wam, cp[nbstep].start-cp[i+1].start,0,pdata->slack,limit,pdata->slack,lh_method,TRUE);
      maxloss_but_within = pdata->slack - minloss_before - minloss_after;
      pdata->minloss[i] = pdata->minloss[i] > minloss_within ? pdata->minloss[i] : minloss_within;
      pdata->maxloss[i] = eps*len < pdata->maxloss[i] ? eps*len : pdata->maxloss[i];
      pdata->maxloss[i] = pdata->maxloss[i] > maxloss_but_within ? pdata->maxloss[i] : maxloss_but_within;
      if (pdata->minloss[i] > pdata->maxloss[i])
	return FALSE;
    }
    
    /* Phase 3: for each step: what is the smallest height<=limit that can be reached? */


    for (i=0; i<nbstep && (flags&0x4); i++) {
      SP_integer eps = limit-cp[i].height;
      if (eps>0) {
	SP_integer inf = cp[i].start;
	SP_integer sup = cp[i+1].start;
	SP_integer minsum = smallest_total_height_le(wam, pdata,inf,sup,eps);
	SP_integer loss = (sup-inf)*(eps-minsum);
	
	if (pdata->minloss[i] < loss)
	  pdata->minloss[i] = loss;
      }
    }

    /* Phase 4: tighten minloss, maxloss, resslack. */
    
    butmin = pdata->resslack;
    butmax = pdata->resslack;
    for (i=0; i<nbstep; i++) {
      butmin -= pdata->minloss[i];
      butmax -= pdata->maxloss[i];
    }
    if (butmin<0 || butmax>0)
      return FALSE;
    for (i=0; i<nbstep; i++) {
      SP_integer delta = pdata->maxloss[i] - (butmin + pdata->minloss[i]);
      if (delta>0) {
	pdata->maxloss[i] -= delta;
	butmax += delta;
      }
    }
    for (i=0; i<nbstep; i++) {
      SP_integer delta = pdata->minloss[i] - (butmax + pdata->maxloss[i]);
      if (delta<0) {
	pdata->minloss[i] -= delta;
	butmin += delta;
      }
    }
    pdata->resslack = butmin;
  }
  return TRUE;
}

static SP_BOOL
task_can_be_included(Wam wam,
		     struct task *t,
		     SP_integer inf,
		     SP_integer sup) 
{
  if (dur(t)>sup-inf)
    return FALSE;
  else if (t->is_interval(wam, t))
    return lst(t)>=inf && ect(t)<=sup;
  else
    return fd_intersect_interval(t->set(wam, t),MakeSmall(inf),MakeSmall(sup-dur(t)));
}

static SP_BOOL
task_can_cross_column(Wam wam,
		      struct task *t,
		      SP_integer b) 
{
  return (est(t)<=b && lct(t)>b &&
	  (t->is_interval(wam, t) ||
	   intersects_interval(t->set(wam, t),b-dur(t)+1,b)));
}

static SP_integer
cp_height(struct comppart *cp,
	  SP_integer key)
{
  while (cp->start<=key) cp++;
  return (cp-1)->height;
}

/** #define cp_overlaps_neither(t) (ect(t)<=lst(t) || sup<lst(t) || inf-1>=ect(t) || (inf-1<lst(t) && sup>=ect(t))) **/

#define task_contributes_to_i(t) (lst(t)<=inf-1 && inf-1<ect(t))
#define task_contributes_to_j(t) (lst(t)<=sup && sup<ect(t))
#define task_contributes_to_ij(t) (ect(t)>=inf && lst(t)<=sup && /* must cross one border, or be included in I */ \
	  !task_can_be_included(wam, t,inf,sup)) /* not included in I */


/* NOTE: we return as soon as something was pruned. The reason is that
   two tasks may share the same domain variable, if a diffs objects
   consists of several sboxes. So pruning task t1 may affect the
   value of task_can_be_included(wam, t2) for another task t2.
   Illustrating case:

:-	B in {1,3},
	H in{1,4},
	I in 2..3,
	J in{1,3},
	K in{1,4},
	Tuple = [3,B,1,3,4,2,1,H,I,J,K],
	diffs([object(1,3,[B,1]),object(2,3,[3,4]),object(3,2,[1,H]),object(4,I,[J,K])],
	      [sbox(4,[2,3],[4,1]),sbox(2,[4,4],[1,3]),sbox(3,[3,2],[3,3]),sbox(2,[1,3],[2,3])],
	      [cumulative(true)]).

 */

static int
compulsory_profile_2instants(Wam wam,
			     struct unary_data *pdata,
			     SP_integer inf, /* pseudocode's i = inf-1 */
			     SP_integer sup, /* pseudocode's j = sup */
			     struct comppart *cp,
			     SP_integer limit) 
{
  SP_integer hinf = cp_height(cp,inf-1);
  SP_integer hsup = cp_height(cp,sup);
  SP_integer flowsum=0, flowmax=0, hpq=0;
  SP_integer gapmin=limit-(hinf > hsup ? hinf : hsup);
  SP_integer gapmax=limit-(hinf < hsup ? hinf : hsup);
  SP_integer gapsum=gapmin+gapmax;
  SP_integer resmin_eps=limit, resmax_eps=0, ressum_eps=0;
  int nbtasks=pdata->nbtasks;
  struct task *tp=NULL, *tq=NULL;
  int i, rc=0;
  
  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (task_contributes_to_ij(t) && !task_contributes_to_i(t) && !task_contributes_to_j(t)) {
      flowsum += res(t);
      flowmax = res(t) > flowmax ? res(t) : flowmax;
      if (res(t)>=(gapmin+2)/2) {
	ressum_eps += res(t);
	if (res(t)<=gapmin)
	  resmax_eps = res(t) > resmax_eps ? res(t) : resmax_eps;
      }
    }
    if ((!task_contributes_to_i(t) && task_can_cross_column(wam, t,inf-1)) ||
	(!task_contributes_to_j(t) && task_can_cross_column(wam, t,sup))) {
      resmin_eps = res(t) < resmin_eps ? res(t) : resmin_eps;
    }
  }
  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (est(t)<=inf-1 &&
	inf-1<lst(t) &&
	lst(t)<ect(t) &&
	ect(t)<=sup &&
	sup<lct(t) &&
	!task_contributes_to_i(t) &&
	!task_contributes_to_j(t) &&
	!task_contributes_to_ij(t)) {
      if (!tp || res(tp)<res(t))
	tq=tp, tp=t;
      else if (!tq || res(tq)<res(t))
	tq=t;
    }
  }
  if (tq) {
    SP_integer low = lst(tp) < lst(tq) ? lst(tp) : lst(tq);
    SP_integer up  = ect(tp) > ect(tq) ? ect(tp) : ect(tq);
    SP_integer cplow = cp_height(cp,low-1);
    SP_integer cpup  = cp_height(cp,up);
    SP_integer mindur = dur(tp) < dur(tq) ? dur(tp) : dur(tq);
    if (res(tp)+res(tq)+(cplow < cpup ? cplow : cpup)>limit &&
	low+mindur>sup && up-mindur<inf)
      hpq = res(tp)+res(tq);
  }
  if (flowsum+hpq>gapsum ||
      ressum_eps-resmax_eps>gapmax ||
      (flowsum+resmin_eps>gapsum && flowsum+pdata->slack<gapsum))
    rc = 0x2;
  
  for (i=0; i<nbtasks && !rc; i++) {
    struct task *t = pdata->tasks+i;
    SP_integer flowsum2 = (t==tp ? flowsum : t==tq ? flowsum : flowsum+hpq);
    
    if (!task_contributes_to_i(t) && !task_contributes_to_j(t)) {
      if (task_contributes_to_ij(t)) {
	if (dur(t)>=sup-inf+2 && flowsum2+res(t)>gapsum) {
	  /* R8: prevent task with nonzero outflow from crossing 2 borders */
	  rc |= unary_prune_interval(wam, t,sup-dur(t)+1,inf-1);
	}
      } else {
	if (flowsum2+res(t)>gapsum /* R2 */ ||
	    (res(t)>=(gapmin+2)/2 /* R7 */ &&
	     ((res(t)>resmax_eps && res(t) <= gapmin && ressum_eps-res(t)+resmax_eps > gapmax) ||
	      (res(t)<=resmax_eps && ressum_eps+res(t)-resmax_eps > gapmax)))) {
	  /* R2,R7: prevent task with possibly zero outflow from crossing any border */
	  rc |= unary_prune_interval(wam, t,sup-dur(t)+1,sup);
	  rc |= unary_prune_interval(wam, t,inf-dur(t),inf-1);
	} else if (dur(t)>=sup-inf+2) {
	  if (flowsum2+2*res(t)>gapsum) {
	    /* R1: prevent task with possibly zero outflow from crossing 2 borders */
	    rc |= unary_prune_interval(wam, t,sup-dur(t)+1,inf-1);
	  } else if (flowsum2>gapmax-gapmin && res(t)>gapmin-(flowsum2-(gapmax-gapmin)+1)/2) {
	    /* R3: prevent task with possibly zero outflow from crossing 2 borders */
	    rc |= unary_prune_interval(wam, t,sup-dur(t)+1,inf-1);
	  } else if (res(t)>gapmax-flowmax) {
	    /* R4: prevent task with possibly zero outflow from crossing 2 borders */
	    rc |= unary_prune_interval(wam, t,sup-dur(t)+1,inf-1);
	  }
	}
      }
    } else if (!task_contributes_to_j(t)) {
	if (flowsum2+res(t)>gapsum) {
	  /* R5: prevent task crossing inf-1 from crossing sup */
	  rc |= unary_prune_interval(wam, t,sup-dur(t)+1,sup);
	}
    } else if (!task_contributes_to_i(t)) {
	if (flowsum2+res(t)>gapsum) {
	  /* R6: prevent task crossing sup from crossing inf-1 */
	  rc |= unary_prune_interval(wam, t,inf-dur(t),inf-1);
	}
    }
  }
  return rc;
}

static void
get_contrib_disj_tasks(Wam wam,
			     struct unary_data *pdata,
			     int nbc,
			     struct task *skip,
			     SP_integer rest,
			     SP_integer gap,
			     SP_integer slack,
			     SP_integer min_height,
			     lh_method *lh_method,
			     SP_integer *psurf_disj,
			     SP_integer *psum_surf_disj)
{
  SP_integer surf_disj=0, sum_surf_disj=0, surf_max1=0, surf_max2=0, surf_rest=0;
  int nbhalf=1;
  int i;

  for (i=0; i<nbc; i++) {
    struct task *t = pdata->contrib[i];
    SP_integer res = res(t);
    if (t!=skip && res<=gap && (res>gap>>1 || (gap==res<<1 && (nbhalf--)==1))) {
      SP_integer amount = pdata->cp2[t-pdata->tasks].height;
      SP_integer slength = amount/res;
      
      sum_surf_disj += amount;
      if (gap==res || gap-res>=min_height || slack>0) {
	if (rest>0) {
	  if (slength>rest)
	    slength = rest;
	  surf_disj += res*slength;
	  rest -= slength;
	}
      }
      if (slength==dur(t) && (*lh_method)(wam, slack, gap-res, FALSE)<slength) {
	if (amount>=surf_max1) {
	  surf_max2 = surf_max1;
	  surf_max1 = amount;
	} else if (amount>=surf_max2) {
	  surf_max2 = amount;
	}
      } else {
	surf_rest += amount;
      }
    }
  }
  surf_rest += surf_max1 + surf_max2;
  surf_disj = surf_disj < surf_rest ? surf_disj : surf_rest;
  *psurf_disj = surf_disj;
  *psum_surf_disj = sum_surf_disj;
}

static int
compulsory_profile_fill(Wam wam,
			struct unary_data *pdata,
			int  p, /* pth step of profile */
			SP_integer limit, /* of cumulative */
			lh_method *lh_method,
			SP_integer *minloss) 
{
  int i, rc=0, nbc=0;
  int nbtasks=pdata->nbtasks;
  SP_integer surf_disj, sum_surf_disj, to_fill2;
  struct comppart *cp = pdata->cp;
  SP_integer inf = cp[p].start;
  SP_integer sup = cp[p+1].start;
  SP_integer slack = pdata->maxloss[p];
  SP_integer gap = limit-cp[p].height;
  SP_integer to_fill = gap*(sup-inf) - slack;
  SP_integer min_height = gap+1;
  SP_integer lm;
  int  left_closed = TRUE;
  int  right_closed = TRUE;

  /* check whether any task can cross the left/right borders */
  for (i=0; i<nbtasks && left_closed; i++) {
    struct task *t = pdata->tasks+i;

    if (dur(t)>1 && est(t)<inf && lct(t)>inf)
      left_closed = FALSE;
  }
  
  for (i=0; i<nbtasks && right_closed; i++) {
    struct task *t = pdata->tasks+i;

    if (dur(t)>1 && est(t)<sup && lct(t)>sup)
      right_closed = FALSE;
  }

  lm = (*lh_method)(wam, slack, gap, left_closed|right_closed);
  if (gap>0 && lm<sup-inf)
    return 0x2;
  
  pdata->contrib = Malloc(nbtasks, struct task *);

  for (i=0; i<nbtasks && rc<0x2; i++) {
    struct task *t = pdata->tasks+i;
    int incp = (lst(t) <= inf && ect(t) >= sup);

    if (!incp) {
      if (res(t)<=gap) {
	SP_integer inter = task_max_intersection(wam, t, inf, sup);
	SP_integer lmax = (*lh_method)(wam, slack, gap-res(t), FALSE /* 20081008 left_closed|right_closed*/);

	inter = inter < lmax ? inter : lmax;
	if (inter>0) {
	  SP_integer amount = inter * res(t);
	  
	  to_fill -= amount;
	  pdata->cp2[i].height = amount;
	  pdata->contrib[nbc++] = t;
	  min_height = min_height < res(t) ? min_height : res(t);
	}
      } else {
	rc |= must_overlap_less_than(wam, t,1,inf,sup);
      }
    }
  }
  if (to_fill>0) {
    rc = 0x2; goto ret;
  }
  
  qsort_res(wam, pdata->contrib, nbc);

  get_contrib_disj_tasks(wam, pdata, nbc, NULL, sup-inf, gap, slack, min_height, lh_method,
			 &surf_disj, &sum_surf_disj);
  to_fill2 = to_fill+(sum_surf_disj-surf_disj);
  if (to_fill2>0) {
    rc = 0x2; goto ret;
  }
  *minloss = slack+to_fill2 > 0 ? slack+to_fill2 : 0;
  for (i=0; i<nbc && rc<0x2; i++) {
    struct task *t = pdata->contrib[i];
    SP_integer amount = pdata->cp2[t-pdata->tasks].height;
    SP_integer smin = to_fill + amount;
    SP_integer epsilon = gap-res(t);
    if (smin>0) {
      rc |= must_overlap_at_least(wam, t,(smin-1)/res(t)+1,inf,sup);
    }
    if (epsilon==0) {		/* t completely fills up to limit */
      if ((sup-inf)>(lm-dur(t))) /* 20080825 */
	rc |= must_overlap_at_least(wam, t, (sup-inf)-(lm-dur(t)), inf, sup);
    } else {
      SP_integer tsize = dur(t) < sup-inf ? dur(t) : sup-inf;
      SP_integer lmaxi, lmaxe, surf_disj_fill, sum_surf_disj_fill, epsilon_fill = 0;
      int j;
      
      if (epsilon>=min_height) {
	for (j=nbc-1; j>=0; j--) {
	  struct task *u = pdata->contrib[j];
	  if (u!=t) {
	    if (res(u)<=epsilon)
	      epsilon_fill += pdata->cp2[u-pdata->tasks].height;
	    else
	      break;
	  }
	}
	get_contrib_disj_tasks(wam, pdata, nbc, t, tsize, epsilon, slack, min_height, lh_method,
			       &surf_disj_fill, &sum_surf_disj_fill);
	epsilon_fill -= (sum_surf_disj_fill-surf_disj_fill);
      }
      if ((*lh_method)(wam, slack, epsilon, TRUE)<tsize) {
	if (left_closed)
	  rc |= unary_prune_value(wam, t, inf);
	if (right_closed)
	  rc |= unary_prune_value(wam, t, sup-dur(t));
      }
      lmaxe = (*lh_method)(wam, slack, epsilon, FALSE);
      lmaxi = (slack+epsilon_fill)/epsilon;
      lmaxi = lmaxi < lmaxe ? lmaxi : lmaxe;
      rc |= must_overlap_less_than(wam, t, lmaxi+1, inf, sup);
    }
  }
 ret:
  Free(pdata->contrib);
  return rc;
}
#endif /* PROFILE_METHOD */

#if KNAPSACK_COLUMN_METHOD
/*
For any point a, let the following be binaries:

s(i,a) iff task i starts at a
c(i,a) iff task i completes at a
o(i,a) iff task i starts before a and completes after a
n(i,a) iff none of the above

The following must hold:

  For each task i : s(i,a)+c(i,a)+n(i,a)+o(i,a) = 1

  \sum_i {height(i)*(c(i,a) + o(i,a))} in [Limit-Slack,Limit]

  \sum_i {height(i)*(s(i,a) + o(i,a))} in [Limit-Slack,Limit]

  \sum_i {height(i)*(c(i,a) + s(i,a) + 2*o(i,a))} in [2*Limit-Slack,2*Limit]

Algorithm:

Applied where a is the beginning of a segment of the compulsory profile,
except for the first segment.

1. Let the binaries be 0..1 variables and compute initial domains.

2. Solve the above system for the variables.

3. s(i,a)=1 ==> fix start(i)=a.

   c(i,a)=1 ==> fix complete(i)=a.

   o(i,a)=1 ==> ensure start(i)>=a-dur(i)+1, complete(i)<=a+dur(i)-1.

   n(i,a)=1 ==> remove a from dom(start(i)).
		remove a from dom(complete(i)).
		if max(start(i))<a then ensure complete(i)<=a.
		if min(complete(i))>a then ensure start(i)>=a.

   s(i,a)=0 ==> remove a from dom(start(i)).

   c(i,a)=0 ==> remove a from dom(complete(i)).

   n(i,a)=0 ==> ensure start(i)>=a-dur(i), complete(i)<=a+dur(i).

   o(i,a)=0 ==> if max(start(i))<a then ensure complete(i)<=a.
		if min(complete(i))>a then ensure start(i)>=a.
*/

/* (t1 & 0x1) is 1 iff the node leads to success */

struct bucket {
  SP_integer stamp, nextlr, sofarl, sofarr;
};

#if KNAPSACK_COLUMN_METHOD
static struct bucket *
knapsack_column_hash_lookup(struct unary_data *pdata,
			    int nextlr,
			    SP_integer sofarl,
			    SP_integer sofarr)
{
  TAGGED key;
  TAGGED t1, t2, t3;
  
  t3 = sofarr;
  t2 = sofarl;
  t1 = nextlr;
  t2 ^= t3;
  t1 ^= t2;
  key = (((t3 << 7) + t2) << 7) + t1 + 0x1000; /* 227272 hash collisions */
  return pdata->cache + key % pdata->hashsize;
}

static int
knapsack_column_eval(Wam wam,
			   struct unary_data *pdata,
			   int next,
			   int nbcand,
			   int satl,
			   int satr,
			   SP_integer sofarl,
			   SP_integer sofarr,
			   SP_integer lslack,
			   SP_integer rslack,
			   SP_integer sslack)
{
  int nextsup = 0;
  struct task *t;
  int class;
  SP_integer limit = pdata->limit;
  SP_integer res, sumres;
  struct bucket *b;
  int nextlr = (next*8);
 
  if (sofarl>limit || sofarr>limit) 
    return FALSE;

  nextlr += (satl<<2) + (satr<<1);
  if (next==nbcand) 
    return (sofarl>=limit-lslack 
	    && sofarr>=limit-rslack 
	    && sofarl+sofarr>=2*limit-sslack);

  t = pdata->cand[next];
  res = res(t);
  sumres = pdata->sumres[next];
  if (sofarl+sumres<limit-lslack
      || sofarr+sumres<limit-rslack
      || sofarl+sofarr+2*sumres<2*limit-sslack) {
    return FALSE;
  }

  b = knapsack_column_hash_lookup(pdata,nextlr,sofarl,sofarr);
  if (b->stamp==pdata->stamp
      && (b->nextlr & -2)==nextlr
      && b->sofarl==sofarl && b->sofarr==sofarr) {
    return (b->nextlr & 1);
  }

  class = t->support>>8;

  if (class&0x4) {
    SP_BOOL satl1 = class&0x40 ? TRUE : satl;
    SP_BOOL satr1 = class&0x10 ? TRUE : satr;
    
    nextsup |= knapsack_column_eval(wam, pdata, next+1, nbcand, satl1, satr1,
				    sofarl+res, sofarr+res, lslack, rslack,
				    sslack) ? 0x4 : 0x0;
  }

  /* class^0x80 implies that t is not the last task in knapsackl which may
     overlap both columns (similarly for class^0x20 and knapsackr) */
  if ((satl || (class^0x80)) && (satr || (class^0x20))) {
    if (!nextsup || pdata->nbrelevant>next)
      if (class & 0x1)
	nextsup |= knapsack_column_eval(wam, pdata, next+1, nbcand, satl, satr, 
					sofarl, sofarr+res, lslack, rslack, 
					sslack) ? 0x1 : 0x0;
    if (!nextsup || pdata->nbrelevant>next)
      if (class & 0x2)
	nextsup |= knapsack_column_eval(wam, pdata, next+1, nbcand, satl, satr,
					sofarl+res, sofarr, lslack, rslack, 
					sslack) ? 0x2 : 0x0;
    if (!nextsup || pdata->nbrelevant>next)
      if (class & 0x8)
	nextsup |= knapsack_column_eval(wam, pdata, next+1, nbcand, satl, satr, 
					sofarl, sofarr, lslack, rslack, 
					sslack) ? 0x8 : 0x0;
  }

  t->support |= nextsup;
  if (pdata->nbrelevant==next+1) {
    int c;
    for (c=next; c>=0; c--) {
      int sup = pdata->cand[c]->support;
      if ((sup>>8)!=(sup&0xff))
	break;
      pdata->nbrelevant--;
    }
  }
  nextsup = !nextsup ? 0 : 1;
  b->stamp  = pdata->stamp;
  b->nextlr = nextlr|nextsup;
  b->sofarl = sofarl;
  b->sofarr = sofarr;

  return nextsup;
}
#endif

static int
knapsack_column_classify(Wam wam,
			 struct task *t,
			 SP_integer border) 
{
  int class = 0;
  TAGGED start_dom = 0;
  SP_BOOL is_interval = t->is_interval(wam, t);
  SP_integer sborder = border-dur(t);
  if (est(t)<=border && lst(t)>=sborder && !is_interval)
    start_dom = t->set(wam, t);
  if (est(t)<=border && lst(t)>=border && (is_interval || fd_member(MakeSmall(border),start_dom)))	/* can start at border? */
    class |= 0x1;
  if (est(t)<=sborder && lst(t)>=sborder && (is_interval || fd_member(MakeSmall(sborder),start_dom))) /* can complete at border? */
    class |= 0x2;
  if (dur(t)>1 && est(t)<border && lst(t)>sborder) /* can be during border? */
    if (is_interval || intersects_interval(start_dom,sborder+1,border-1))
      class |= 0x4;
  if (est(t)<sborder || lst(t)>border) /* can be neither? */
    class |= 0x8;
  return class;
}

static int
knapsack_column_can_push_left(Wam wam,
			      struct unary_data *pdata, 
			      struct task *t,
			      SP_integer border)
{
  SP_integer a = border-dur(t);
  SP_integer b = ect(t) < lst(t) ? ect(t) : lst(t);

  pdata->profile = fd_profile_update(wam, pdata->profile,a,b,res(t));
  return fd_profile_at_most(pdata->profile,pdata->limit);
}

static void
knapsack_column_restore_left(Wam wam,
			     struct unary_data *pdata, 
			     struct task *t,
			     SP_integer border) 
{
  SP_integer a = border-dur(t);
  SP_integer b = ect(t) < lst(t) ? ect(t) : lst(t);

  pdata->profile = fd_profile_update(wam, pdata->profile,a,b,-res(t));
}

static int
knapsack_column_can_push_right(Wam wam,
			       struct unary_data *pdata, 
			       struct task *t,
			       SP_integer border)
{
  SP_integer a = lst(t) > ect(t) ? lst(t) : ect(t);
  SP_integer b = border+dur(t);

  pdata->profile = fd_profile_update(wam, pdata->profile,a,b,res(t));
  return fd_profile_at_most(pdata->profile,pdata->limit);
}

static void
knapsack_column_restore_right(Wam wam,
			      struct unary_data *pdata, 
			      struct task *t,
			      SP_integer border)
{
  SP_integer a = lst(t) > ect(t) ? lst(t) : ect(t);
  SP_integer b = border+dur(t);

  pdata->profile = fd_profile_update(wam, pdata->profile,a,b,-res(t));
}

static int
prune_from_support(Wam wam,
		   struct task *t,
		   int support,
		   SP_integer border) 
{
  int sup = ((support>>4)&0xf0)|(support&0xf);
  int rc = 0;
  if (sup>>4 != (sup & 0xf))
    switch (sup & 0xf) {
    case 0x1:			/* starts -- fix it */
      est(t) = border;
      lctmax(t) = border+dur(t);
      SYNCRC(t);
      break;	
    case 0x2:			/* completes -- fix it */
      lctmax(t) = border;
      est(t) = border-dur(t);
      SYNCRC(t);
      break;	
    case 0x3:			/* starts || completes -- forbid neither, during */
      if (est(t)<border-dur(t))
	est(t) = border-dur(t);
      if (lctmax(t)>border+dur(t))
	lctmax(t) = border+dur(t);
      SYNCRC(t);
      if (sup&0x40)
	rc |= unary_prune_interval(wam, t,border-dur(t)+1,border-1);
      break;	
    case 0x4:			/* during -- forbid starts, completes, neither */
      est(t) = border-dur(t)+1;
      lctmax(t) = border+dur(t)-1;
      SYNCRC(t);
      break;	
    case 0x5:			/* starts || during -- forbid completes, neither */
      if (est(t)<border-dur(t)+1)
	est(t) = border-dur(t)+1;
      if (lctmax(t)>border+dur(t))
	lctmax(t) = border+dur(t);
      SYNCRC(t);
      break;	
    case 0x6:			/* completes || during -- forbid starts, neither */
      if (est(t)<border-dur(t))
	est(t) = border-dur(t);
      if (lctmax(t)>border+dur(t)-1)
	lctmax(t) = border+dur(t)-1;
      SYNCRC(t);
      break;	
    case 0x7:			/* starts || completes || during -- forbid neither */
      if (est(t)<border-dur(t))
	est(t) = border-dur(t);
      if (lctmax(t)>border+dur(t))
	lctmax(t) = border+dur(t);
      SYNCRC(t);
      break;	
    case 0x8:			/* neither -- forbid starts, completes, during */
      rc |= unary_prune_interval(wam, t,border-dur(t),border);
      break;
    case 0x9:			/* starts || neither -- forbid completes, during */
      rc |= unary_prune_interval(wam, t,border-dur(t),border-1);
      break;
    case 0xa:			/* completes || neither -- forbid starts, during */
      rc |= unary_prune_interval(wam, t,border-dur(t)+1,border);
      break;
    case 0xb:			/* starts || completes || neither -- forbid during */
      rc |= unary_prune_interval(wam, t,border-dur(t)+1,border-1);
      break;
    case 0xc:			/* during || neither -- forbid starts, completes */
      rc |= unary_prune_value(wam, t,border);
      rc |= unary_prune_value(wam, t,border-dur(t));
      break;
    case 0xd:			/* starts || during || neither -- forbid completes */
      rc |= unary_prune_value(wam, t,border-dur(t));
      break;
    case 0xe:			/* completes || during || neither -- forbid starts */
      rc |= unary_prune_value(wam, t,border);
      break;
    }
  
  return rc;
}

/* 
 * Given a task t = pdata->tasks+i, the lower 16 bits abcdefghijklmnop of 
 * t->support (and pdata->support[i]) are used as follows:
 *
 * Bits efgh and mnop are used to denote the support of t before respectively 
 * after pruning where (similarly for pomn):
 * h==1 implies t may start at border, 
 * g==1 implies t may complete at border,
 * f==1 implies t may be during border,
 * e==1 implies t may be neither.
 *
 * Bits abcd and ijkl are used to denote if t is in left (ab) and right (cd) 
 * knapsack constraint (i.e., in the arrays knapsackl and knapsackr). These
 * bits must always be the same before and after pruning. Bit b==1 implies
 * that t is in knapsackl and a==1 implies that t is the last such task in
 * knapsackl (similarly for cd and knapsackr).
 */

static int
knapsack_column_prune(Wam wam,
		      struct unary_data *pdata,
		      SP_integer border,
		      SP_integer lslack,
		      SP_integer rslack,
		      SP_integer sslack,
		      dp_hash_method *hasher) 
{
  SP_integer sumres=0, sofarl=0, sofarr=0;
  int nbcand=0, rc=0;
  int nbtasks = pdata->nbtasks;
  int i, isaux, nbovl, nbovr;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    int class = knapsack_column_classify(wam, t,border);

    pdata->support[i] = (class<<8)+class;
  }

  if ((*hasher)(wam, 1, pdata->support, nbtasks,
		lslack, rslack, sslack)) {
    /* prune directly from the support */
    for (i=0; i<nbtasks && rc<0x2; i++) {
      rc |= prune_from_support(wam, pdata->tasks+i, (int)pdata->support[i], border);
    }
  }
  else {
    for (i=0; i<nbtasks; i++) {
      struct task *t = pdata->tasks+i;
      t->support = (int)pdata->support[i];
      switch (t->support&0xf) {
      case 0x1:			/* must start */
	sofarr += res(t);
	break;
      case 0x2:			/* must complete */
	sofarl += res(t);
	break;
      case 0x4:			/* must be during */
	sofarl += res(t);
	sofarr += res(t);
	break;
      case 0x8:			/* must be neither */
	break;
      default:
	t->support &= 0xfff0;	/* compute the supported bits below */
	pdata->cand[nbcand++] = t;
	break;
      }
    }
    qsort_res(wam, pdata->cand,nbcand); /* sort by decreasing res */

    nbovl = 0;
    isaux = 0;
    /* form left knapsack constraint: sum(Oi)>=1 */
    for (i=0; i<nbcand && !isaux; i++) { 
      struct task *t = pdata->cand[i];
      if (lst(t)<border && ect(t)<=border
 	  && ect(t)+dur(t)>border && lct(t)>border) {
	pdata->knapsackl[nbovl++] = i;
	if (!knapsack_column_can_push_left(wam, pdata,t,border))
	  isaux = 1;
      }
    }
    for (i=0; i<nbovl; i++) {	/* restore cumulative profile */
      struct task *t = pdata->cand[pdata->knapsackl[i]];
      knapsack_column_restore_left(wam, pdata,t,border);
    }

    nbovl = isaux ? nbovl : 0;

    /* add bits for hashing */
    for (i=0; i<nbovl; i++) {
      struct task *t = pdata->cand[pdata->knapsackl[i]];
      int ti = (int)(t-pdata->tasks);
      int lset = (i==nbovl-1) ? 0xc0c0 : 0x4040;
      pdata->support[ti] |= lset;
      t->support |= lset;
    }

    nbovr = 0;
    isaux = 0;
    /* form right knapsack constraint: sum(Oi)>=1 */
    for (i=0; i<nbcand && !isaux; i++) { 
      struct task *t = pdata->cand[i];
      if (ect(t)>border && lst(t)>=border
 	  && lst(t)-dur(t)<border && est(t)<border) {
	pdata->knapsackr[nbovr++] = i;
	if (!knapsack_column_can_push_right(wam, pdata,t,border))
	  isaux = 1;
      }
    }
    for (i=0; i<nbovr; i++) {	/* restore cumulative profile */
      struct task *t = pdata->cand[pdata->knapsackr[i]];
      knapsack_column_restore_right(wam, pdata,t,border);
    }

    nbovr = isaux ? nbovr : 0;

    /* add bits for hashing */
    for (i=0; i<nbovr; i++) {
      struct task *t = pdata->cand[pdata->knapsackr[i]];
      int ti = (int)(t-pdata->tasks);
      int rset = (i==nbovr-1) ? 0x3030 : 0x1010;
      pdata->support[ti] |= rset;
      t->support |= rset;
    }

    for (i=nbcand-1; i>=0; i--) {
      struct task *t = pdata->cand[i];
      sumres += res(t);
      pdata->sumres[i] = sumres;
    }
    if (sofarl >= pdata->limit-lslack &&
	sofarr >= pdata->limit-rslack &&
	sofarl+sofarr >= 2*pdata->limit-sslack &&
	sofarl+sofarr+2*sumres <= 2*pdata->limit)
      goto ret; 

    if (nbovl+nbovr>0
	&& (*hasher)(wam, 1,pdata->support,nbtasks,
		     lslack,rslack,sslack)) {
      
      /* prune directly from the support (with additional bits) */
      for (i=0; i<nbtasks && rc<0x2; i++) {
	rc |= prune_from_support(wam, pdata->tasks+i, (int)pdata->support[i], border);
      }

      goto ret;
    }

    /* compute support for all candidates */
    /* support bit per candidate:
       0x1=starts, 0x2=completes, 0x4=during 0x8=neither */
    pdata->nbrelevant = nbcand;
    if (!knapsack_column_eval(wam, pdata, 0, nbcand,
			      nbovl==0 ? TRUE : FALSE, nbovr==0 ? TRUE : FALSE,
			      sofarl, sofarr, lslack, rslack, sslack))
      rc |= 0x2;

    for (i=0; i<nbcand && rc<0x2; i++) {
      struct task *t = pdata->cand[i];
      rc |= prune_from_support(wam, t, t->support, border);
    }

    if (rc>=0x2) goto ret;

    for (i=0; i<nbtasks; i++) {
      struct task *t = pdata->tasks+i;
      pdata->support[i] = t->support;
    }
    if (rc<0x2) 
      (*hasher)(wam, 2, pdata->support, nbtasks,
		lslack, rslack, sslack);
  }
  
 ret:
  return rc;
}

static int
knapsack_column_main(Wam wam,
		     struct unary_data *pdata,
		     int step,
		     SP_integer border,
		     SP_integer est,
		     SP_integer lct,
		     dp_hash_method *hasher,
		     lh_method *lh_method) 
{
  SP_integer before1 = get_minloss(wam, border-est-1,0,pdata->slack,pdata->limit,pdata->slack,lh_method,TRUE);
  SP_integer before2 = get_minloss(wam, border-est,  0,pdata->slack,pdata->limit,pdata->slack,lh_method,TRUE);
  SP_integer after1  = get_minloss(wam, lct-border,  0,pdata->slack,pdata->limit,pdata->slack,lh_method,TRUE);
  SP_integer after2  = get_minloss(wam, lct-border-1,0,pdata->slack,pdata->limit,pdata->slack,lh_method,TRUE);
  SP_integer lslack = pdata->slack - before1 - after1;
  SP_integer rslack = pdata->slack - before2 - after2;
  SP_integer sslack = pdata->slack - before1 - after2;

  if (border==pdata->cp[step].start) { /* columns in different profile steps */
    SP_integer ub1 = pdata->maxloss[step-1];
    SP_integer ub2 = pdata->maxloss[step];
    SP_integer ub3 = pdata->minloss[step-1] + pdata->minloss[step] + pdata->resslack;
    lslack = lslack < ub1 ? lslack : ub1;
    rslack = rslack < ub2 ? rslack : ub2;
    sslack = sslack < ub1+ub2 ? sslack : ub1+ub2;
    sslack = sslack < ub3 ? sslack : ub3;
  } else {			/* columns in same profile step */
    SP_integer ub = pdata->maxloss[step];
    lslack = lslack < ub ? lslack : ub;
    rslack = rslack < ub ? rslack : ub;
    sslack = sslack < ub ? sslack : ub;
  }
  pdata->stamp++;
  return knapsack_column_prune(wam, pdata,border,lslack,rslack,sslack,hasher);
}
#endif /* KNAPSACK_COLUMN_METHOD */

#if PARALLEL_CONFLICT_PROFILE_METHOD

/* Sort by increasing <height,length>. */

static int 
cmp_lh_incr(Wam wam, struct lhs *t1, struct lhs *t2)
{
  SP_integer h1 = t1->h;
  SP_integer h2 = t2->h;

  (void)wam;
  if (h1<h2)
    return -1;
  else if (h1>h2)
    return 1;

  h1 = t1->l;
  h2 = t2->l;

  if (h1<h2)
    return -1;
  else if (h1>h2)
    return 1;
  else
    return 0;
}

#define QType struct lhs
#define QCmp  cmp_lh_incr
#define QSort qsort_lh_incr
#include "qsort.ic"

struct pcframe {
  SP_integer psum;
  SP_integer pj;
  SP_integer epsilon_max;
  SP_integer prev_divide;
  int pnslices;
  int tnslices;
};

static void
parallel_conflict_on_profile_available_slices(SP_integer limit,
					      int nbtasks_no_cp,
					      int nheights_cp,
					      struct lhs *tab_lh,
					      struct lhs *tab_eps,
					      SP_integer *tab_nslices,
					      SP_integer *tab_sumh,
					      int i,
					      SP_integer *max_gap,
					      SP_integer *level,
					      int *nb_available_slices)
{
  int j=0, k;
  SP_integer sumh=0;

  *nb_available_slices = 0;
  *max_gap = 0;
  *level = limit;
  for (k=0; k<nheights_cp; k++) {
    if (i==0) {
      while (j<nbtasks_no_cp && sumh+tab_lh[j].h<=tab_eps[k].h)
	sumh += tab_lh[j++].h;
    } else {
      sumh = tab_sumh[k];
      if (sumh>0)
	sumh -= tab_lh[i-1].h;
      j = (int)(i-1+tab_nslices[k]);
      if (j<i) j = i;
      if (j<nbtasks_no_cp && sumh+tab_lh[j].h<=tab_eps[k].h)
	sumh += tab_lh[j++].h;
      /* invariant j==nbtasks_no_cp || sumh + tab_lh[j].h > tab_eps[k].h */
      tab_sumh[k] = sumh;
    }
    tab_nslices[k] = j-i;
    tab_sumh[k] = sumh;
    *max_gap = *max_gap > tab_eps[k].h-sumh ? *max_gap : tab_eps[k].h-sumh;
    if (j==i && *level>limit-tab_eps[k].h)
      *level = limit-tab_eps[k].h;
  }
  for (k=0; k<nheights_cp; k++) {
    *nb_available_slices += (int)(tab_eps[k].l*tab_nslices[k]);
  }
}


static int
parallel_conflict_on_profile_needed_slices(struct pcframe *pc,
					   int nbtasks_no_cp,
					   struct lhs *tab_lh,
					   int i)
{
  SP_integer gap, divide, maxh;
  int k, nb_needed_slices, remove=0;

  while (pc->pj<nbtasks_no_cp && pc->psum+tab_lh[pc->pj].h<=pc->epsilon_max) {
    pc->psum += tab_lh[pc->pj].h;
    pc->pnslices += (int)tab_lh[pc->pj].l;
    if (pc->prev_divide>0)
      remove += (int)(tab_lh[pc->pj].h / pc->prev_divide * tab_lh[pc->pj].l);
    pc->pj++;
  }

  gap = pc->epsilon_max-pc->psum;
  maxh = (pc->pj>0) ? tab_lh[pc->pj-1].h : 0;
  divide = gap+1>maxh ? gap+1 : maxh;
  if (divide!=pc->prev_divide) {
    pc->tnslices = 0;
    for (k=(int)pc->pj; k<nbtasks_no_cp; k++)
      pc->tnslices += (int)(tab_lh[k].h / divide * tab_lh[k].l);
  } else {
    pc->tnslices -= remove;
  }
  nb_needed_slices = pc->pnslices+pc->tnslices;
  pc->prev_divide = divide;
  if (pc->psum>0) {
    pc->psum -= tab_lh[i].h;
    pc->pnslices -= (int)tab_lh[i].l;
  }
  return nb_needed_slices;
}

static int
parallel_conflict_on_profile_intervals(SP_integer level,
				       int nbsteps,
				       struct comppart *cp,
				       SP_integer *pruning_low,
				       SP_integer *pruning_up)
{
  int npruning=0, in=0, i;

  for (i=0; i<nbsteps; i++) {
    if (in) {
      if (cp[i].height>=level) {
	pruning_up[npruning++] = cp[i].start-1;
	in = 0;
      }
    } else {
      if (cp[i].height<level) {
	pruning_low[npruning] = cp[i].start;
	in = 1;
      }
    }
  }
  if (in)
    pruning_up[npruning++] = cp[i].start-1;
  return npruning;
}

static int
parallel_conflict_on_profile_prune(Wam wam,
				   int i,
				   SP_integer level,
				   SP_integer minh,
				   SP_integer margin,
				   int nbsteps,
				   struct comppart *cp,
				   SP_integer *pruning_low,
				   SP_integer *pruning_up,
				   struct lhs *tab_eps,
				   struct lhs *tab_lh,
				   SP_integer *tab_max_cum,
				   SP_integer *tab_sumh,
				   SP_integer *cp_hindex,
				   struct task *tasks) 
{
  int rc=0;
  
  if (i>=1 && tab_max_cum[i-1]>margin) {
    int npruning = parallel_conflict_on_profile_intervals(level,nbsteps,cp,pruning_low,pruning_up);
    int j, k;
    for (j=i-1; rc<2 && j>=0 && tab_max_cum[j]>margin && tab_lh[j].h>minh; j--) {
      for (k=0; k<npruning; k++) {
	SP_integer inter = minh<0 ? margin/tab_lh[j].h+1 : margin+1;
	if (tab_lh[j].l>=inter &&
	    pruning_up[k]-pruning_low[k]+1>=inter)
	  rc |= unary_prune_interval(wam, tasks+tab_lh[j].s, pruning_low[k]+inter-tab_lh[j].l, pruning_up[k]-inter+1);
      }
    }
  }
  if (minh > -1) {
    int j, k;
    for (j=i-1; rc<2 && j>=0 && tab_max_cum[j]>margin; j--) {
      for (k=0; k<nbsteps; k++) {
	if (cp[k].height<level) {
	  SP_integer inter = margin+1;
	  int hindex = (int)cp_hindex[k];
	  SP_integer gap = tab_eps[hindex].h - tab_sumh[hindex];

	  if (tab_lh[j].h>gap && tab_lh[j].l>=inter &&
	      cp[k+1].start-cp[k].start>=inter)
	    rc |= unary_prune_interval(wam, tasks+tab_lh[j].s, cp[k].start+inter-tab_lh[j].l, cp[k+1].start-inter);
	}
      }
    }
  }
  return rc;
}

static int
parallel_conflict_on_profile_surface_check(Wam wam,
					   SP_integer limit,
					   int nbsteps,
					   int nheights_cp,
					   int nbtasks_no_cp,
					   struct comppart *cp,
					   SP_integer *pruning_low,
					   SP_integer *pruning_up,
					   struct lhs *tab_eps,
					   struct lhs *tab_lh,
					   SP_integer *tab_max_cum,
					   SP_integer *tab_sumh,
					   SP_integer *cp_hindex,
					   struct task *tasks) 
{
  int i, j=nheights_cp, rc=0;
  SP_integer available=0, needed=0, level=limit, margin;

  for (i=nbtasks_no_cp-1; i>=0; i--) {
    while (j-1>=0 && tab_eps[j-1].h>=tab_lh[i].h) {
      j--;
      available += tab_eps[j].l * tab_eps[j].h;
      level = limit-tab_eps[j].h;
    }
    needed += tab_lh[i].l * tab_lh[i].h;
    margin = available-needed;
    if (margin<0)
      rc |= 2;
    else
      rc |= parallel_conflict_on_profile_prune(wam, i,level+1,-1,margin,nbsteps,cp,pruning_low,pruning_up,tab_eps,tab_lh,tab_max_cum,tab_sumh,cp_hindex,tasks);
  }
  return rc;
}

static int
parallel_conflict_on_profile(Wam wam,
			     struct unary_data *pdata,
			     int nbsteps,
			     SP_integer limit) 
{
  struct comppart *cp = pdata->cp;
  int nbtasks = pdata->nbtasks;
  int nbtasks_no_cp=0, nheights_cp=0, nbtab_cp;
  SP_integer *tab_sumh = Malloc(6*nbtasks+9*nbsteps, SP_integer); /* nbtasks */
  SP_integer *tab_nslices = tab_sumh + 1*nbtasks; /* nbtasks */
  SP_integer *tab_max_cum = tab_sumh + 2*nbtasks; /* nbtasks */
  struct lhs *tab_lh = (struct lhs *)(tab_sumh + 3*nbtasks); /* 3*nbtasks */
  struct lhs *tab_cp = (struct lhs *)(tab_sumh + 6*nbtasks); /* 3*nbsteps */
  struct lhs *tab_eps = (struct lhs *)(tab_sumh + 6*nbtasks + 3*nbsteps); /* 3*nbsteps */
  SP_integer *pruning_low = tab_sumh + 6*nbtasks + 6*nbsteps;	/* nbsteps */
  SP_integer *pruning_up = tab_sumh + 6*nbtasks + 7*nbsteps; /* nbsteps */
  SP_integer *cp_hindex = tab_sumh + 6*nbtasks + 8*nbsteps; /* nbsteps */
  struct pcframe pc = {0,0,0,0,0,0};
  int i, j, rc=0;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (enable(t)==2 && lst(t)>=ect(t)) {
      tab_lh[nbtasks_no_cp].l = dur(t);
      tab_lh[nbtasks_no_cp].h = res(t);
      tab_lh[nbtasks_no_cp].s = i;
      nbtasks_no_cp++;
    }
  }
  qsort_lh_incr(wam, tab_lh,nbtasks_no_cp);

  for (i=0, j=0; i<nbsteps; i++)
    if (cp[i].height<limit) {
      tab_cp[j].h = limit-cp[i].height;
      tab_cp[j].l = cp[i+1].start - cp[i].start;
      j++;
    }
  nbtab_cp = j;
  qsort_lh_incr(wam, tab_cp,nbtab_cp);

  for (i=0, j=-1; i<nbtab_cp; i++) { /* TODO O(n log n) variant */
    if (i==0 || tab_cp[i-1].h!=tab_cp[i].h) {
      j++;
      tab_eps[j].l = 0;
      tab_eps[j].h = tab_cp[i].h;
    }
    tab_eps[j].l += tab_cp[i].l;
  }
  nheights_cp = j+1;
  for (j=0; j<nbsteps; j++) {
    for (i=0; i<nheights_cp; i++) {
      if (limit-cp[j].height == tab_eps[i].h) {
	cp_hindex[j] = i;
	break;
      }
    }
  }  
  for (i=0; i<nbtasks_no_cp; i++) {
    SP_integer prod = tab_lh[i].l * tab_lh[i].h;
    SP_integer prev = i==0 ? 0 : tab_max_cum[i-1];
    tab_max_cum[i] = prod > prev ? prod : prev;
  }
  rc |= parallel_conflict_on_profile_surface_check(wam, limit,nbsteps,nheights_cp,
						   nbtasks_no_cp,cp,pruning_low,
						   pruning_up,tab_eps,tab_lh,
						   tab_max_cum,tab_sumh,
						   cp_hindex, pdata->tasks);
  for (i=0; i<nbtasks_no_cp; i++) {
    SP_integer prev = i==0 ? 0 : tab_max_cum[i-1];
    tab_max_cum[i] = tab_lh[i].l > prev ? tab_lh[i].l : prev;
  }
  pc.epsilon_max = tab_cp[nbtab_cp-1].h;
  for (i=0; i<nbtasks_no_cp && rc<2; i++) {
    SP_integer minh, level;
    int nb_needed_slices =
      parallel_conflict_on_profile_needed_slices(&pc,nbtasks_no_cp,tab_lh,i);
    int nb_available_slices;
    
    parallel_conflict_on_profile_available_slices(limit,nbtasks_no_cp,
						  nheights_cp,tab_lh,tab_eps,
						  tab_nslices, tab_sumh, 
						  i, &minh, &level,
						  &nb_available_slices);
    if (nb_available_slices < nb_needed_slices)
      rc |= 2;
    else
      rc |= parallel_conflict_on_profile_prune(wam, i,level,minh,
					       nb_available_slices
					       -nb_needed_slices,
					       nbsteps,cp,pruning_low,
					       pruning_up,tab_eps,tab_lh,
					       tab_max_cum,tab_sumh,cp_hindex,
					       pdata->tasks);
  }
  Free(tab_sumh);
  return rc;
}

#endif

#if EXTRA_METHOD
static int 
slack_compulsory_parts(Wam wam,
			     struct unary_data *pdata,
			     Dvar limitvar,
			     SP_integer est,
			     SP_integer lct)
{
  struct comppart *cp = pdata->cp;
  SP_integer *udate = (SP_integer *)pdata->eventp;
  int p, q, nb;
  
  compulsory_parts(wam, pdata,NULL,-CLPFD_MAXINT,CLPFD_MAXINT,limitvar);
  q = 0;
  if (udate[1]!=est)
    udate[q++] = est;
  for (p=1; udate[p] < lct; p++, q++)
    udate[q] = udate[p];
  udate[q] = lct;
  nb = q;

  /* nb steps of the profile */
  /* now split steps of cp that have been merged */

  q = 0;
  if (cp[1].start!=est) {
    cp[q].start = est;
    cp[q++].height = 0;
  }
  for (p=1; cp[p].start < lct; p++, q++)
    cp[q] = cp[p];
  cp[q].start = lct;
  cp[q].height = 0;

  for (p=nb; p>=0; p--) {
    cp[p].start = udate[p];
    cp[p].height = cp[q].height;
    if (udate[p]==cp[q].start)
      q--;
  }

  return nb;
}

static int
slack_filtering(Wam wam,
		struct unary_data *pdata, 
		Dvar limitvar,
		SP_integer est,
		SP_integer lct,
		SP_integer durmax,
		lh_method *lh_method,
		int flags,
		dp_hash_method *hasher,
		struct size_mem *size_mem
		)
{
  SP_integer limit = dvar_max_l(limitvar);
  struct comppart *cp = pdata->cp;
  int nbtasks = pdata->nbtasks;
  SP_integer *udate = (SP_integer *)pdata->eventp;
  int i, p, q, r, rc=0;
  (void)flags;
  (void)hasher;
  (void)size_mem;
  (void)lh_method;
  
  pdata->limit = limit;
  pdata->resslack = pdata->slack;
#if CONSECUTIVE_STAIRS_METHOD
  q = slack_fixed_parts(wam, pdata,est,lct);
  for (p=0; p<q-1 && rc<2; p++) {
    if (cp[p].height < limit &&
	cp[p+1].height < cp[p].height &&
	cp[p+1].height > 0)
      if (!consecutive_stairs(pdata,p,limit))
	return 2;
  }
#endif
  q = slack_compulsory_parts(wam, pdata,limitvar,est,lct);
  /* no compulsory parts start or end inside some step */
  /* in general, stop as soon as some pruning is done (rc>0) */
#if PARALLEL_CONFLICT_PROFILE_METHOD
  rc |= parallel_conflict_on_profile(wam, pdata,q,limit);
#endif
#if PROFILE_METHOD
  for (p=0; p<q-1 && rc<1; p++) {
    SP_integer inf = udate[p];
    for (r=p+1; r<q && rc<1; r++) {
      SP_integer sup = udate[r];
      if (inf>est && sup<lct &&
	  inf+durmax>sup	/* Heuristic: sometimes, 2instants can be useful even when this cond. is false. */
	  )
	rc |= compulsory_profile_2instants(wam, pdata,inf,sup,cp,limit);
    }
  }
  if (rc<1) {
    if (!compulsory_profile_minloss(wam, pdata,q,limit,lh_method,flags))
      rc = 2;
    for (p=0; p<q && rc<1; p++) {
      SP_integer gap = limit-cp[p].height;
      SP_integer inf = cp[p].start;
      SP_integer sup = cp[p+1].start;
      SP_integer slack = pdata->maxloss[p];
      SP_integer to_fill = gap*(sup-inf) - slack;
      SP_integer altloss = 0;
      if (sup-inf<lct-est && to_fill>0) {
	rc |= compulsory_profile_fill(wam, pdata,p,limit,lh_method,&altloss);
	if (altloss>pdata->minloss[p]) {
	  SP_integer delta = altloss-pdata->minloss[p];
	  pdata->minloss[p] += delta;
	  pdata->resslack   -= delta; /* TODO: should propagate to maxloss */
	}
      }
    }
  }
#endif

#if KNAPSACK_COLUMN_METHOD
  if (rc<1 && (flags&0x4) && hasher && pdata->slack<2*limit) {
    char *ptr;
    int hashsize;
    int alloc;

    SP_ASSERT(pdata->limit >= 0);
    hashsize = (int)(nbtasks * pdata->limit * (long)sqrt((double)pdata->limit));
    alloc = (int)(3*nbtasks*sizeof(SP_integer) + hashsize*sizeof(struct bucket) +    
		  2*nbtasks*sizeof(SP_integer));
    pdata->hashsize = hashsize;
    ptr = PermAlloc(alloc,char,size_mem);
    pdata->cand = (struct task **)ptr;
    ptr += nbtasks*sizeof(struct task *);
    pdata->knapsackl = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->knapsackr = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->support = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->sumres = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->cache = (struct bucket *)ptr;
    ptr += hashsize*sizeof(struct bucket);
    (void)ptr;                  /* suppress clang warning about value stored is never read. */
    for (i=0; i<hashsize; i++)
      pdata->cache[i].stamp = 0;
    pdata->stamp = 0;

#if KNAPSACK_COLUMN_METHOD
    pdata->profile = fd_empty_profile();
    for (p=q-1; p>=0; p--)
      pdata->profile = fd_profile_update(wam, pdata->profile,cp[p].start,cp[p+1].start,cp[p].height);

    for (p=q-1; p>=0 && rc<2; p--) {
      SP_integer inf = cp[p].start;
      SP_integer sup = cp[p+1].start;
      SP_integer b;

      for (b=sup-1; b>=inf && b>est && rc<2; b--)
	rc |= knapsack_column_main(wam, pdata,p,b,est,lct,hasher,lh_method);
    }
    fd_profile_dispose(wam, pdata->profile);
#endif
    PermFree(pdata->cand,size_mem);
  }
#endif
  return rc;
}
#endif

/* DOES NOT find a fixpoint */
/* flags: 0x2 - use task intervals and "slack filtering" methods
          0x4 - use dynamic programming methods
          0x8 - all heights and durations are fixed
 */
int
fd_discrete_filtering(Wam wam,
		      struct task *tasks,
		      int nbtasks,
		      int nbtotal,
		      int flags,
		      Dvar diffvar,
		      Dvar limitvar,
		      Dvar lowervar,
		      Dvar uppervar,
		      struct diff_constraint *dc,
		      int nbdiffs,
		      lh_method *lh_method,
		      dp_hash_method *hasher,
		      struct size_mem *size_mem) 
{
  int rc=0, i;
  SP_integer est = CLPFD_MAXINT2;
  SP_integer lct = -CLPFD_MAXINT2;
  SP_integer durmax = 0;
  SP_integer slack = 0;
  SP_integer lower = TagIsSmall(dvar_min_t(lowervar)) ? dvar_min_l(lowervar) : -CLPFD_MAXINT2;
  SP_integer upper = TagIsSmall(dvar_max_t(uppervar)) ? dvar_max_l(uppervar) :  CLPFD_MAXINT2;

  slack = 0;
  for (i=0; i<nbtasks; i++) {
    struct task *t = tasks+i;

    if (enable(t)==2) {
      slack -= dur(t)*res(t);
      if (durmax<dur(t))
	durmax = dur(t);
    }
    if (est(t)<lower) {
      est(t) = lower;
      SYNCRC(t);
    }
    if (lctmax(t)>upper) {
      lctmax(t) = upper;
      SYNCRC(t);
    }
    if (est>est(t))
      est = est(t);
    if (lct<lctmax(t))
      lct = lctmax(t);
  }
  slack += dvar_max_l(limitvar)*(lct-est);
  if (slack<0)
    rc |= 2;
  if (rc<2) {
    struct unary_data *pdata = unary_alloc(wam, tasks,nbtasks,nbtotal,(flags & ~1),size_mem);
    pdata->slack = slack;
    if (nbtasks>1) {
      struct task **map = PermAlloc(nbtasks,struct task *,size_mem+1);
      int nbutasks;
      SP_integer lst=CLPFD_MAXINT2, ect=-CLPFD_MAXINT2;

      rc |= disjunctive_tasks(wam, pdata,map,limitvar,&nbutasks);
      if (rc<2)
	rc |= unary_filtering_rec(wam, map,nbutasks,nbtotal,flags,diffvar,dc,nbdiffs,&lst,&ect,size_mem+2);
      if (rc<2 && lst < CLPFD_MAXINT2 && dvar_fix_interval_t(lowervar,Inf,MakeSmall(lst))<0)
	rc |= 2;
      if (rc<2 && ect >-CLPFD_MAXINT2 && dvar_fix_interval_t(uppervar,MakeSmall(ect),Sup)<0)
	rc |= 2;
      PermFree(map,size_mem+1);
    }
    if (rc<2 /*rc==0 && nbutasks<nbtasks -- unary_filtering may ignore ground tasks*/) {
      rc |= cp_task_profile(wam, pdata,limitvar);
      if (rc==0)
	rc |= dr_cp_ends(wam, pdata,limitvar);
      if (rc==0)
	rc |= dr_cp_starts(wam, pdata,limitvar);
      if (rc==0 && (flags&0x2))
	rc |= task_interval_filtering(wam, pdata,limitvar,flags);
#if EXTRA_METHOD
      if (rc==0 && (flags&0xa)==0xa)
        {
          rc |= slack_filtering(wam, pdata,limitvar,est,lct,durmax,lh_method,
                                flags,hasher,size_mem+1 
                                );
        }
#endif
    }
    PermFree(pdata,size_mem);
  }
  return rc;
}


/* CUMULATIVE REVISITED */
typedef SP_integer TASK;

#define STATUS_SOURCE 0x1
#define STATUS_TARGET 0x2
#define STATUS_CONNECTED 0x4
#define STATUS_SOURCE_LATER 0x10
#define STATUS_TARGET_LATER 0x20

#define TARGET(I)   (pdata->target[I])
#define STATUS(t) (pdata->status[t])
#define ORIGVAR(t) (pdata->origvar+(t))
#define DURVAR(t) (pdata->durvar+(t))
#define ENDVAR(t) (pdata->endvar+(t))
#define USEVAR(t) (pdata->usevar+(t))
#define DURmin(T) dvar_min_l(DURVAR(T))
#define DURmax(T) dvar_max_l(DURVAR(T))
#define DURfix(T) dvar_is_integer(DURVAR(T))
#define USEmin(T) dvar_min_l(USEVAR(T))
#define USEmax(T) dvar_max_l(USEVAR(T))
#define USEfix(T) dvar_is_integer(USEVAR(T))
#define EST(T) dvar_min_l(ORIGVAR(T))
#define LaST(T) dvar_max_l(ORIGVAR(T))
#define ECT(T) dvar_min_l(ENDVAR(T))
#define LCTmax(T) dvar_max_l(ENDVAR(T))
#define LCTmin(T) (LaST(T)+DURmin(T))
#define RefLimAttr     (pdata->refbase)
#define RefLim         (pdata->refbase + 1)
#define RefLowerAttr   (pdata->refbase + 2)
#define RefLower       (pdata->refbase + 3)
#define RefUpperAttr   (pdata->refbase + 4)
#define RefUpper       (pdata->refbase + 5)
#define RefOrigAttr(T) (pdata->refbase + ((T)<<3) + 6)
#define RefOrig(T)     (pdata->refbase + ((T)<<3) + 7)
#define RefDurAttr(T)  (pdata->refbase + ((T)<<3) + 8)
#define RefDur(T)      (pdata->refbase + ((T)<<3) + 9)
#define RefEndAttr(T)  (pdata->refbase + ((T)<<3) + 10)
#define RefEnd(T)      (pdata->refbase + ((T)<<3) + 11)
#define RefUseAttr(T)  (pdata->refbase + ((T)<<3) + 12)
#define RefUse(T)      (pdata->refbase + ((T)<<3) + 13)
#define DIFFVAR(t) (pdata->diffvar+(t))
#define RefDiffAttr(T) (pdata->drefbase + ((T)<<1))
#define RefDiff(T) (pdata->drefbase + ((T)<<1) + 1)

/* The constraint frame. */
struct cumulative_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;

  int nrefs;			/* static, 2 + 8*nbtasks + nbdiffs */
  SP_globref refbase;		/* static */
  SP_globref drefbase;		/* static */
  SP_integer stamp;
  int nbdiffs;
  int nbtasks;			/* static, #tasks */
  int ntargets;			/* #tasks that may be targets, := nbtasks */
  int nsources;			/* #tasks that may be sources only, := 0 */
  int flags;			/* static */
  SP_BOOL change;
  SP_integer use_limit;		/* capacity limit */
  int earliest;			/* -1 or task with smallest est */
  int latest;			/* -1 or task with largest lct */
  Dvar limitvar;
  Dvar lowervar;
  Dvar uppervar;
  Dvar origvar;
  Dvar durvar;
  Dvar endvar;
  Dvar usevar;
  TASK *target;			/* [nbtasks] */
  SP_integer *status;			/* [nbtasks] */
  SP_integer *lch_table;
  SP_integer *lh_table;
  Dvar diffvar;
  struct diff_constraint *dc;
  /* space for the above arrays */
};

static void SPCDECL cumulative_destructor(void *pdata_v)
{
  struct cumulative_data *pdata = (struct cumulative_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->lh_table) {
    Free(pdata->lch_table);
    Free(pdata->lh_table);
  }
  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}


static void decompose(struct cumulative_data *pdata)
{
  SP_integer est = CLPFD_MAXINT;
  SP_integer lct = -CLPFD_MAXINT;
  int nbtasks = pdata->nbtasks;
  int nbdiffs = pdata->nbdiffs;
  int ntargets = pdata->ntargets;
  int nactive_items = ntargets + pdata->nsources;
  int i;
  
  for (i=0; i<nactive_items; i++) {
    TASK si = TARGET(i);
    
    if (STATUS(si)&STATUS_TARGET) {
      if (est>EST(si))
	est = EST(si);
      if (lct<LCTmax(si))
	lct = LCTmax(si);
    }
  }

  /* forget sources that can no longer prune */
  for (i=ntargets; i<nactive_items; i++) {
    TASK si = TARGET(i);
    STATUS(si) &= ~STATUS_CONNECTED;
  }
  for (i=nbdiffs-1; i>=0; i--) {
    struct diff_constraint *dc = pdata->dc+i;

    if (STATUS(nbtasks+i) & STATUS_TARGET) {
      STATUS(dc->si) |= STATUS_CONNECTED;
      STATUS(dc->sj) |= STATUS_CONNECTED;
    }
  }
  for (i=0; i<nactive_items; i++) {
    TASK si = TARGET(i);
    
    if ((STATUS(si)&(STATUS_SOURCE|STATUS_TARGET|STATUS_CONNECTED))==STATUS_SOURCE &&
	(LCTmax(si)<=est || lct<=EST(si)))
      STATUS(si) -= STATUS_SOURCE;
  }
}


/* maintain BOUND-CONSISTENT origin + duration = end */
/* arc-consistent would involve a trickier termination test,
   and cumulative_prune_interval_method would have to call here */
static SP_BOOL 
task_renormalize(struct cumulative_data *pdata,
		 TASK t) 
{
  for (;;) {
    SP_integer est = EST(t);
    SP_integer lst = LaST(t);
    SP_integer ect = ECT(t);
    SP_integer lct = LCTmax(t);
    SP_integer mindur = DURmin(t);
    SP_integer maxdur = DURmax(t);
    SP_integer lb1 = ect-maxdur;
    SP_integer ub1 = lct-mindur;
    SP_integer lb2 = ect-lst;
    SP_integer ub2 = lct-est;
    SP_integer lb3 = est+mindur;
    SP_integer ub3 = lst+maxdur;

    if (mindur>0 && (dvar_fix_min_l(pdata->limitvar, USEmin(t))<0 ||
		     dvar_fix_max_l(USEVAR(t), pdata->use_limit)<0))
      return FALSE;
    
    if (!(est<lb1 || lst>ub1 ||
	  mindur<lb2 || maxdur>ub2 ||
	  ect<lb3 || lct>ub3))
      return TRUE;
    
    /* origin in min(end)-max(dur) ... max(end)-min(dur) */
    if (dvar_fix_interval_l(ORIGVAR(t), lb1, ub1)<0)
      return FALSE;
  
    /* dur in min(end)-max(origin) ... max(end)-min(origin) */
    if (dvar_fix_interval_l(DURVAR(t), lb2, ub2)<0)
      return FALSE;
  
    /* end in min(origin)+min(dur) ... max(origin)-max(dur) */
    if (dvar_fix_interval_l(ENDVAR(t), lb3, ub3)<0)
      return FALSE;
  }
}

static int
diff_renormalize(struct cumulative_data *pdata,
		 int i) /* maintain Si-Sj = Dij */
{
  TASK si = (pdata->dc+i)->si;
  TASK sj = (pdata->dc+i)->sj;
  Dvar sivar = ORIGVAR(si);
  Dvar sjvar = ORIGVAR(sj);
  Dvar dijvar = DIFFVAR(i);
  int rc = 0;
  
  for (;;) {
    SP_integer minsi = dvar_min_l(sivar);
    SP_integer maxsi = dvar_max_l(sivar);
    SP_integer minsj = dvar_min_l(sjvar);
    SP_integer maxsj = dvar_max_l(sjvar);
    SP_integer mindij = dvar_min_l(dijvar);
    SP_integer maxdij = dvar_max_l(dijvar);
    SP_integer lb1 = minsj-maxdij;
    SP_integer ub1 = maxsj-mindij;
    SP_integer lb2 = minsj-maxsi;
    SP_integer ub2 = maxsj-minsi;
    SP_integer lb3 = minsi+mindij;
    SP_integer ub3 = maxsi+maxdij;

    if (!(minsi<lb1 || maxsi>ub1 ||
	  mindij<lb2 || maxdij>ub2 ||
	  minsj<lb3 || maxsj>ub3))
      return rc;
    
    /* Si in min(Sj)-max(dij) ... max(Sj)-min(dij) */
    switch (dvar_fix_interval_l(sivar, lb1, ub1)) {
    case -1:
      return 0x2;
    case 0:
      break;
    default:
      rc |= 0x1;
      pdata->change = TRUE;
      if (!task_renormalize(pdata,si))
	return 0x2;
    }
  
    /* dij in min(Sj)-max(Si) ... max(Sj)-min(Si) */
    if (dvar_fix_interval_l(dijvar, lb2, ub2)<0)
      return 0x2;
  
    /* Sj in min(Si)+min(dij) ... max(Si)-max(dij) */
    switch (dvar_fix_interval_l(sjvar, lb3, ub3)) {
    case -1:
      return 0x2;
    case 0:
      break;
    default:
      rc |= 0x1;
      pdata->change = TRUE;
      if (!task_renormalize(pdata,sj))
	return 0x2;
    }
  }
}





/* '$fd_cumulative'(+State0, -State, -Actions).

   Filtering algorithm for serialized/[2,3] and cumulative/[4,5].

   State0 = State = f(N,Tasks,Diffs,Limit,Lower,Upper,Flags,NTargets,NSources,Handle,Stamp).

   Tasks are the tasks task(Si,SMi,Di,DMi,Ri,RMi,i,_) to be scheduled,
   where i in 0..N-1.

   Diffs is a list of d(i,j,Mij) where Mij is a mutable whose value is a
   domain (a..-Dj)\/(Di..b) if tasks i and j are disjunctive or have a
   precedence relation; otherwise, 0 may be in the domain.

   Flags include: 0x1 - precedences
                  0x2 - task intervals rule

   No unbounded domains!

   The following invariants hold when a fixpoint is reached:

   [0. cumulative]
	   The total task load does not exceed limit anywhere.

   [1. differences]
	   Si + dij = Sj

   [3. mutual exclusion]

	   If tasks i and j are exclusive, then
		   Di=0 | Dj=0 | Si+Di =< Sj | Sj+Dj =< Si
	   Hence
		   Si+Di =< max(Sj) | Si >= min(Sj)+Dj
		      ##                            ##
		   Sj+Dj =< max(Si) | Sj >= min(Si)+Di
		      ##                            ##

   Can be tightened by replacing ## by values from relevant Dij variable.

   [4. resource restriction]

	   Bi0*R0 +...+ Bin*Rm =< L,  at all points in time i, where

	   Bij = 1, if Sj =< i < Sj+Dj
	   Bij = 0, otherwise

*/
static SP_BOOL
add_precedence(struct unary_data *pdata,
	       int i,
	       int j,
	       char *precedence)
{
  int nbtasks = pdata->nbtasks;
  struct task *tasks = pdata->tasks;
  int irow = i*nbtasks;
  int jrow = j*nbtasks;
  int a, b;
  
  if (!precedence[irow+j] && !must_precede(tasks+i,tasks+j)) {
    if (must_precede(tasks+j,tasks+i) || precedence[jrow+i])
      return FALSE;
    precedence[irow+j] = 1;
    /* Transitive closure: precedence a->b is added iff one of following: */
    /* a->i->j->b     */
    /*  a=i->j->b */
    /* a->i->j=b */
    for (a = 0; a < nbtasks; a++) {
      int arow = a*nbtasks;
      for (b = 0; b < nbtasks; b++) {
	if (a==b || precedence[arow+b])
	  continue;
	if (a == i && b == j)
	  continue;
	if (a != i && !precedence[arow+i])
	  continue;
	if (b != j && !precedence[jrow+b])
	  continue;
	precedence[arow+b] = 1;
      }
    }
  }
  return TRUE;
}

static int
unary_detect_all_precedences(Wam wam,
				    struct unary_data *pdata,
				    Dvar diffvar,
				    struct diff_constraint *dc,
				    int nbdiffs)
{
  int nbtasks = pdata->nbtasks;
  int nbprec = 0;
  struct task *tasks = pdata->tasks;
  char *precedence = Malloc(nbtasks*nbtasks,char);
  SP_integer ao;
  int i, j, k, rc=0;

  for (i=nbtasks-1; i>=0; i--) {
    int irow = i*nbtasks;
    for (j=nbtasks-1; j>=0; j--)
      precedence[irow+j] = 0;
  }

  for (k=0; k<nbdiffs; k++) {
    i = (int)pdata->id_map[(dc+k)->si];
    j = (int)pdata->id_map[(dc+k)->sj];
    if (i>=0 && j>=0 && enable(tasks+i)==2 && enable(tasks+j)==2) { /* 4.4 */
      if (dvar_min_l(diffvar+k) > -dur(tasks+j)) {
	nbprec++;
	if (!add_precedence(pdata,i,j,precedence)) {
	  rc = 2; goto ret;
	}
      }
      if (dvar_max_l(diffvar+k) < dur(tasks+i)) {
	nbprec++;
	if (!add_precedence(pdata,j,i,precedence)) {
	  rc = 2; goto ret;
	}
      }
    }
  }

  if (nbprec>0) {
  
    /* adjust ESTs */
    for (i = 0; i < nbtasks; i++) {
      struct task *ti = tasks+i;
      if (enable(ti)==0)
	continue;
    
      /* \Omega = \emptyset */
      ao = -CLPFD_MAXINT; /* "after" \Omega, future EST(i) */

      /* Put all activities which have to be processed BEFORE  */
      /* the activity i into the set \Omega. */
      for (j = 0; j < nbtasks; j++) {
	struct task *tj = pdata->est_rank[j];
	int tji = (int)(tj-tasks);

	if (ti==tj || enable(tj)<2 || !(precedence[tji*nbtasks+i] || must_precede(tj,ti)))
	  continue;

	/* Add activity j into the \Omega */
	if (ao < est(tj)) {
	  ao = est(tj) + dur(tj);
	} else {
	  ao += dur(tj);
	}
      }

      if (pdata->lb[i] < ao) {
	pdata->lb[i] = ao; 
      }
    }

    /* adjust LCTs */
    for (i = 0; i < nbtasks; i++) {
      struct task *ti = tasks+i;
      if (enable(ti)==0)
	continue;
    
      /* \Omega = \emptyset */
      ao = CLPFD_MAXINT; /* "before" \Omega, future LCT(i) */

      /* Put all activities which have to be processed AFTER  */
      /* the activity i into the set \Omega. */
      for (j = 0; j < nbtasks; j++) {
	struct task *tj = pdata->lct_rank[j];
	int tji = (int)(tj-tasks);

	if (ti==tj || enable(tj)<2 || !(precedence[i*nbtasks+tji] || must_precede(ti,tj)))
	  continue;

	/* Add activity j into the \Omega */
	if (ao > lct(tj)) {
	  ao = lct(tj) - dur(tj);
	} else {
	  ao -= dur(tj);
	}
      }

      if (pdata->ub[i] > ao) {
	pdata->ub[i] = ao; 
      }
    }

    /* back propagate precedences */
    for (k=0; k<nbdiffs && rc<2; k++) {
      i = (int)pdata->id_map[(dc+k)->si];
      j = (int)pdata->id_map[(dc+k)->sj];
      if (i>=0 && j>=0 && (diffvar+k)->set /* 0 means "read-only" fake dvar from geost */) {
	struct task *ti = tasks+i;
	struct task *tj = tasks+j;
	if (enable(ti)==2 && enable(tj)==2) {
	  if (must_precede(ti,tj)) {
	    switch (dvar_fix_min_l((diffvar+k), dur(ti))) {
	    case -1:
	      rc |= 2;
	    case 0:
	      break;
	    default:
	      rc |= 1;
	    }
	  }
	  if (must_precede(tj,ti)) {
	    switch (dvar_fix_max_l(diffvar+k, -dur(tj))) {
	    case -1:
	      rc |= 2;
	    case 0:
	      break;
	    default:
	      rc |= 1;
	    }
	  }
	}
      }
    }
  }
 ret:
  Free(precedence);
  return rc;
}

/* return one of 0(succeed), 0x1(propagate), 0x2(fail) */
static int
cumulative_propagate_method(Wam wam,
				  struct task *t)
{
  struct cumulative_data *pdata = fd.gdata;
  SP_BOOL pruned = FALSE;
  TASK si = t->id;
  
  switch (enable(t)) {
  case 1:
    if (lctmax(t) > est(t)) {
      switch (dvar_fix_max_l(DURVAR(si),lctmax(t)-est(t))) {
      case -1:
	return 0x2;
      case 0:
	break;
      default:
	pruned = TRUE;
      }
      break;
    }
    /* FALLTHROUGH */
  case 0:
    switch (dvar_fix_max_l(DURVAR(si),0)) {
    case -1:
      return 0x2;
    case 0:
      break;
    default:
      pruned = TRUE;
    }
    break;
  case 2:
    {
      SP_integer dtask_j_est = est(t);
      switch (dvar_fix_min_l(ORIGVAR(si), dtask_j_est)) {
      case -1:
	return 0x2;
      case 0:
	break;
      default:
	pruned = TRUE;
      }
    }
    switch (dvar_fix_max_l(ENDVAR(si), lctmax(t))) {
    case -1:
      return 0x2;
    case 0:
      break;
    default:
      pruned = TRUE;
    }
    if (!dvar_is_integer(DURVAR(si)))
      switch (dvar_fix_max_l(DURVAR(si),lctmax(t)-est(t))) {
      case -1:
	return 0x2;
      case 0:
	break;
      default:
	pruned = TRUE;
      }
  }
  if (pruned && !task_renormalize(pdata,si))
    return 0x2;
  if (pruned)
    pdata->change = TRUE;
  return (pruned ? 0x1 : 0x0);
}

static TAGGED
cumulative_set_method(Wam wam, struct task *t) 
{
  struct cumulative_data *pdata = fd.gdata;

  return dvar_set(ORIGVAR(t->id));
}

static void
cumulative_refresh_method(Wam wam, struct task *t) 
{
  struct cumulative_data *pdata = fd.gdata;

  (void)dvar_set(ORIGVAR(t->id));
}

static int
cumulative_prune_interval_method(Wam wam,
				 struct task *t,
				 SP_integer min, SP_integer max) 
{
  struct cumulative_data *pdata = fd.gdata;
  Dvar dvar = ORIGVAR(t->id);
  int rc;
  
  switch (rc=dvar_prune_interval_l(dvar,min,max)) {
  case -1:
    return 0x2;
  case 0:
    return 0x0;
  }
  if (enable(t)==2 && lct(t)==lctmax(t)) { /* fixed duration */
    switch (dvar_prune_interval_l(ENDVAR(t->id),min+dur(t),max+dur(t))) {
    case -1:
      return 0x2;
    }
  }
  if (rc & (DV_PRUNED_MIN|DV_PRUNED_MAX)) { /* [MC] 4.2.3: was missing */
    if (!task_renormalize(pdata,t->id))
      return 0x2;
    t->est = dvar_min_l(dvar);
    t->lct = dvar_max_l(dvar)+dur(t);
    t->lctmax = t->lct;
  }
  pdata->change = TRUE;
  return 0x1;
}

static int
cumulative_prune_set_method(Wam wam, struct task *t, TAGGED set) 
{
  struct cumulative_data *pdata = fd.gdata;
  Dvar dvar = ORIGVAR(t->id);
  int rc;
  
  switch (rc=dvar_prune_set(dvar,set)) {
  case -1:
    return 0x2;
  case 0:
    return 0x0;
  }
  if (enable(t)==2 && lct(t)==lctmax(t)) { /* fixed duration */
    switch (dvar_prune_set(ENDVAR(t->id),fd_lsh(wam, set,MakeSmall(dur(t))))) {
    case -1:
      return 0x2;
    }
  }
  if (rc & (DV_PRUNED_MIN|DV_PRUNED_MAX)) { /* [MC] 4.2.3: was typo */
    if (!task_renormalize(pdata,t->id))	    /* [MC] 4.2.3: was missing */
      return 0x2;
    t->est = dvar_min_l(dvar);
    t->lct = dvar_max_l(dvar)+dur(t);
    t->lctmax = t->lct;
  }
  pdata->change = TRUE;
  return 0x1;
}

static int
cumulative_is_interval_method(Wam wam,
				    struct task *t)
{
  struct cumulative_data *pdata = fd.gdata;
  
  return dvar_is_interval(ORIGVAR(t->id));
}

static SP_integer
cumulative_lh_method(Wam wam,
			   SP_integer slack,
			   SP_integer epsilon,
			   int closed)
{
  struct cumulative_data *pdata = fd.gdata;
  
  (void)slack;
  if (epsilon==0 || !pdata->lh_table)
    return CLPFD_MAXINT2;
  return closed ? pdata->lch_table[epsilon-1] : pdata->lh_table[epsilon-1];
}


void SPCDECL
prolog_fd_cumulative(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  TAGGED tasks, diffs, handle, tmp;
  int i, j, k, nbtasks, nbdiffs;
  int ent = -1;			/* disentailed unless otherwise */
  SP_BOOL committed, posted;
  int flags;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  SP_integer total_size, state_stamp, use_limit;
  struct cumulative_data *pdata;
  struct task *dtasks;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  fd_init_profile(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct cumulative_data,handle);
    fd.gdata = pdata;
    flags = pdata->flags;
    nbtasks = pdata->nbtasks;
    nbdiffs = pdata->nbdiffs;
  } else {			/* build persistent state */
				/* compute flags, nbtasks */
    posted = TRUE;
    DerefArg(tmp,X(0),1);		/* get N */
    nbtasks = GetSmall_int(tmp);
    if (nbtasks==0) {
      ent = 1;
      goto ret1;
    }
    DerefArg(tmp,X(0),3);		/* get Diffs */
    nbdiffs = fd_list_length(tmp);
    DerefArg(tmp,X(0),7);		/* get Flags */
    flags = GetSmall_int(tmp);
    total_size = (2*nbtasks + nbdiffs)*sizeof(SP_integer) +
                 (3 + 4*nbtasks + nbdiffs)*sizeof(struct dvar) +
                 nbdiffs*sizeof(struct diff_constraint);
    pdata = Palloc(struct cumulative_data, total_size, handle); /* GC */
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->limitvar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->lowervar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->uppervar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->origvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->durvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->endvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->usevar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->target = (TASK *)ptr;
    ptr = (char *)(pdata->target+nbtasks);
    pdata->status = (SP_integer *)ptr;
    ptr = (char *)(pdata->status+nbtasks+nbdiffs);
    pdata->diffvar = (Dvar)ptr;
    ptr += nbdiffs*sizeof(struct dvar);
    pdata->dc = (struct diff_constraint *)ptr;
    ptr += nbdiffs*sizeof(struct diff_constraint);
    SP_ASSERT(ptr==(char *)(pdata+1)+total_size);
    pdata->destructor = cumulative_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = 2*nbdiffs + 8*nbtasks + 6;
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    pdata->drefbase = pdata->refbase + 8*nbtasks + 6;
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nbtasks = nbtasks;
    pdata->nbdiffs = nbdiffs;
    pdata->flags = flags;
    pdata->lch_table = NULL;
    pdata->lh_table = NULL;
    pdata->earliest = -1;
    pdata->latest = -1;

    DerefArg(tmp,X(0),4);	/* get Limit */
    fd_get_var_and_attr(tmp,RefLimAttr);
    dvar_init(pdata->limitvar, RefLimAttr, RefLim);
    DerefArg(tmp,X(0),5);	/* get Lower */
    fd_get_var_and_attr(tmp,RefLowerAttr);
    dvar_init(pdata->lowervar, RefLowerAttr, RefLower);
    DerefArg(tmp,X(0),6);	/* get Upper */
    fd_get_var_and_attr(tmp,RefUpperAttr);
    dvar_init(pdata->uppervar, RefUpperAttr, RefUpper);
    DerefArg(tasks,X(0),2);	/* get Tasks */
    for (i=0; i<nbtasks; i++) {
      TASK si = i;
      TAGGED elt;
      DerefCar(elt,tasks);
      DerefCdr(tasks,tasks);
      DerefArg(tmp,elt,1);
      fd_get_var_and_attr(tmp,RefOrigAttr(si));
      DerefArg(tmp,elt,2);
      fd_get_var_and_attr(tmp,RefDurAttr(si));
      DerefArg(tmp,elt,3);
      fd_get_var_and_attr(tmp,RefEndAttr(si));
      DerefArg(tmp,elt,4);
      fd_get_var_and_attr(tmp,RefUseAttr(si));
      dvar_init(ORIGVAR(si), RefOrigAttr(si), RefOrig(si));
      dvar_init(DURVAR(si),  RefDurAttr(si), RefDur(si));
      dvar_init(ENDVAR(si),  RefEndAttr(si), RefEnd(si));
      dvar_init(USEVAR(si),  RefUseAttr(si), RefUse(si));
      DerefArg(tmp,elt,5);
      TARGET(i) = GetSmall(tmp);
      STATUS(si) = (STATUS_SOURCE_LATER|STATUS_TARGET_LATER);
    }
    
    DerefArg(diffs,X(0),3);
    for (i=0; TagIsLST(diffs); i++) {
      struct diff_constraint *dc = pdata->dc+i;
      TAGGED delt, tmp;
      DerefCar(delt,diffs);	/* get d/3: d(J,I,DIJ): Oi+Dij = Oj */
      DerefCdr(diffs,diffs);
      DerefArg(tmp,delt,1);	/* get task J's ID */
      j = GetSmall_int(tmp);
      for (k=0; k<nbtasks; k++)
	if (TARGET(k)==j)
	  break;
      dc->sj = k;
      DerefArg(tmp,delt,2);	/* get task I's ID */
      j = GetSmall_int(tmp);
      for (k=0; k<nbtasks; k++)
	if (TARGET(k)==j)
	  break;
      dc->si = k;
      DerefArg(tmp,delt,3);	/* get Dij pair */
      fd_get_var_and_attr(tmp,RefDiffAttr(i));
      dvar_init(DIFFVAR(i), RefDiffAttr(i), RefDiff(i));
    }

    for (i=nbtasks-1; i>=0; i--)
      TARGET(i) = i;
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),3) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_export_start(wam);
  fd.gdata = pdata;
  dvar_refresh(pdata->limitvar);
  use_limit = dvar_max_l(pdata->limitvar);
  dvar_refresh(pdata->lowervar);
  dvar_refresh(pdata->uppervar);

  DerefArg(tmp,X(0),8);
  pdata->ntargets = GetSmall_int(tmp);
  DerefArg(tmp,X(0),9);
  pdata->nsources = GetSmall_int(tmp);
  nactive_items = pdata->ntargets + pdata->nsources;

  if (state_stamp != pdata->stamp ||
      use_limit != pdata->use_limit) { /* trust nothing */

    pdata->nsources = 0;
    pdata->ntargets = nbtasks;
    nactive_items = nbtasks;

    /* refresh all task variables */
  
    for (i=nactive_items-1; i>=0; i--) {
      TASK si = TARGET(i);
      
      STATUS(si) |= STATUS(si)>>4;
    }
    if (pdata->earliest>=0 && STATUS(pdata->earliest)&STATUS_TARGET)
      pdata->earliest = -1;
    if (pdata->latest>=0 && STATUS(pdata->latest)&STATUS_TARGET)
      pdata->latest = -1;
  }
  pdata->use_limit = use_limit;
  pdata->stamp = state_stamp+1;
  
  for (i=0; i<nactive_items; i++) {
    TASK si = TARGET(i);
    if (STATUS(si) & STATUS_TARGET) {
      dvar_refresh(ORIGVAR(si));
      dvar_refresh(DURVAR(si));
      dvar_refresh(ENDVAR(si));
      dvar_refresh(USEVAR(si));
      if (USEmin(si)>pdata->use_limit && dvar_fix_max_l(DURVAR(si),0)<0)
	goto ret1;
      if (!task_renormalize(pdata,si))
	goto ret1;
    }
  }
  for (i=0; i<nbdiffs; i++) {
    struct diff_constraint *dc = pdata->dc+i;
    STATUS(nbtasks+i) = STATUS(dc->si)|STATUS(dc->sj);
    if (STATUS(nbtasks+i) & STATUS_TARGET) { /* ensure that bounds are fixed, which is a
					       prerequisite for diff_renormalize */
      SP_integer minsi = dvar_min_l(ORIGVAR(dc->si));
      SP_integer maxsi = dvar_max_l(ORIGVAR(dc->si));
      SP_integer minsj = dvar_min_l(ORIGVAR(dc->sj));
      SP_integer maxsj = dvar_max_l(ORIGVAR(dc->sj));
      dvar_refresh(DIFFVAR(i));
      if (dvar_fix_interval_l(DIFFVAR(i), minsj-maxsi, maxsj-minsi)<0)
	goto ret1;
    }
  }

  dtasks = Malloc(nactive_items,struct task);
  if (posted && (flags&2)) {
    SP_integer estall = CLPFD_MAXINT;
    SP_integer lctall = -CLPFD_MAXINT;
    SP_integer limit = dvar_max_l(pdata->limitvar);
    SP_integer slack=0;

    for (i=j=0; i<nactive_items; i++) {
      TASK si = TARGET(i);
      if (estall > EST(si))
	estall = EST(si);
      if (lctall < LCTmax(si))
	lctall = LCTmax(si);
      if (DURfix(si) && USEfix(si) && DURmin(si)>0 && USEmin(si)>0) {
	slack -= DURmin(si)*USEmin(si);
	j++;
      }
    }
    slack += limit * (lctall-estall);
#if LONGEST_HOLE
    if (j>0 && slack>=0) {	/* 4.4 */
      struct lhs *lhs = Malloc(j, struct lhs);
      
      pdata->lch_table = Malloc(limit, SP_integer);
      pdata->lh_table = Malloc(limit, SP_integer);
      for (i=j=0; i<nactive_items; i++) {
	TASK si = TARGET(i);
	if (DURfix(si) && USEfix(si) && DURmin(si)>0 && USEmin(si)>0) {
	  lhs[j].l = DURmin(si);
	  lhs[j++].h = USEmin(si);
	}
      }
      fd_lh_init(wam, 
		 slack,
		 0,
		 1,
		 lctall-estall,
		 limit,
		 j,
		 lhs,
		 pdata->lh_table,
		 pdata->lch_table,
		 LONGEST_HOLE_NBACKS,
		 2);
      Free(lhs);
    }
#endif
  }
  
  /* END OF RESUMPTION */

  do {
    SP_BOOL fixdurres = TRUE;
    struct size_mem size_mem[] = {{0,NULL},{0,NULL},{0,NULL}};
    pdata->change = FALSE;

    for (i=0; i<nbdiffs; i++) {
      if (STATUS(nbtasks+i) & STATUS_TARGET) {
	if (diff_renormalize(pdata,i) >= 0x2) {
	  goto ret;
	}
      }
    }
    if (pdata->change)
      break/*continue*/;

    for (i=j=0; i<nactive_items; i++) {
      TASK si = TARGET(i);

      if (DURmax(si)>0 && USEmin(si)>0) {
	est(dtasks+j) = EST(si);
	lct(dtasks+j) = LCTmin(si);
	lctmax(dtasks+j) = LCTmax(si);
	dur(dtasks+j) = DURmin(si);
	res(dtasks+j) = USEmin(si);
	fixdurres &= (DURfix(si) && USEfix(si));
	enable(dtasks+j) = 2;
	(dtasks+j)->diffid = (int)si;
	(dtasks+j)->id = (int)si;
	(dtasks+j)->propagate = cumulative_propagate_method;
	(dtasks+j)->refresh = cumulative_refresh_method;
	(dtasks+j)->set = cumulative_set_method;
	(dtasks+j)->prune_interval = cumulative_prune_interval_method;
	(dtasks+j)->prune_set = cumulative_prune_set_method;
	(dtasks+j)->is_interval = cumulative_is_interval_method;
	if (dur(dtasks+j)==0) {
	  SP_integer inc = dvar_successor_l(DURVAR(si),0);
	  lct(dtasks+j) += inc;
	  if (lct(dtasks+j) > lctmax(dtasks+j))
	    lct(dtasks+j) = lctmax(dtasks+j);
	  dur(dtasks+j) += inc;
	  enable(dtasks+j) = 1;
	}
	j++;
      }
    }
    if (j>0)
      switch (fd_discrete_filtering(wam, dtasks,j,
				    nbtasks,
				    flags+(fixdurres<<3),
				    pdata->diffvar,
				    pdata->limitvar,
				    pdata->lowervar,
				    pdata->uppervar,
				    pdata->dc,
				    nbdiffs,
				    cumulative_lh_method,
				    NULL,
				    size_mem
				    )) {
      case 3:
      case 2:
	goto ret;
      case 1:
	pdata->change = TRUE;
      }
  } while (FALSE/*pdata->change*/);

  for (i=0; i<nactive_items; i++) {
    TASK si = TARGET(i);
    
    if (!pdata->change &&
	(STATUS(si) & STATUS_TARGET) &&
	dvar_is_integer(ORIGVAR(si)) &&
	dvar_is_integer(DURVAR(si)) &&
	dvar_is_integer(ENDVAR(si)) &&
	dvar_is_integer(USEVAR(si)))
      STATUS(si) &= ~STATUS_TARGET;

    if (dvar_fix_interval_t(pdata->lowervar,Inf,MakeSmall(LaST(si)))<0)
      goto ret;
    if (dvar_fix_interval_t(pdata->uppervar,MakeSmall(ECT(si)),Sup)<0)
      goto ret;
  }
  if (!pdata->change)
    decompose(pdata);

  if (pdata->earliest<0 || EST(pdata->earliest)>dvar_min_l(pdata->lowervar)) {
    SP_integer est = CLPFD_MAXINT;

    for (i=0; i<nbtasks; i++) {
      TASK si = TARGET(i);

      if (est>=EST(si)) {	/* break ties by max. i */
	est = EST(si);
	pdata->earliest = (int)si;
      }
    }
    if (dvar_fix_interval_t(pdata->lowervar,MakeSmall(est),Sup)<0)
      goto ret;
  }
  if (pdata->latest<0 || LCTmax(pdata->latest)<dvar_max_l(pdata->uppervar)) {
    SP_integer lct = -CLPFD_MAXINT;

    for (i=0; i<nbtasks; i++) {
      TASK si = TARGET(i);

      if (lct<=LCTmax(si)) {	/* break ties by max. i */
	lct = LCTmax(si);
	pdata->latest = (int)si;
      }
    }
    if (dvar_fix_interval_t(pdata->uppervar,Inf,MakeSmall(lct))<0)
      goto ret;
  }

  for (i=nbdiffs-1; i>=0; i--) {
    if (STATUS(nbtasks+i) & STATUS_TARGET)
      dvar_pruning_done( DIFFVAR(i));
  }
  
  for (i=pdata->ntargets-1; i>=0; i--) {
    dvar_pruning_done( ORIGVAR(TARGET(i)));
    dvar_pruning_done( DURVAR(TARGET(i)));
    dvar_pruning_done( ENDVAR(TARGET(i)));
    dvar_pruning_done( USEVAR(TARGET(i)));
  }
  dvar_pruning_done( pdata->limitvar);
  dvar_pruning_done( pdata->lowervar);
  dvar_pruning_done( pdata->uppervar);

  /* OK to GC from here */
  
  ent = !!dvar_is_integer(pdata->limitvar); /* 4.3 */
  for (i=pdata->ntargets-1; i>=0; i--) {
    TASK si = TARGET(i);

    if (STATUS(si) & STATUS_TARGET)
      ent = 0;
    dvar_export(ORIGVAR(si));
    dvar_export(DURVAR(si));
    dvar_export(ENDVAR(si));
    dvar_export(USEVAR(si));
  }
  dvar_export(pdata->limitvar);
  dvar_export(pdata->lowervar);
  dvar_export(pdata->uppervar);

  for (i=nbdiffs-1; i>=0; i--) {
    if (STATUS(nbtasks+i) & STATUS_TARGET)
      dvar_export(DIFFVAR(i));
  }

  if (ent || pdata->change)
    goto ret;

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets-1;
    TASK held = TARGET(sup); /* sup is the hole */
    TASK current = TARGET(inf);
    
    while (inf<=sup) {
      if (STATUS(current) & STATUS_TARGET) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    delta = pdata->ntargets - inf;
    pdata->ntargets -= delta;
    pdata->nsources += delta;
  }
  
  /* find the last SOURCE */

  {
    int delta, sup;
    
    for (sup=nactive_items; sup>pdata->ntargets; --sup)
      if (STATUS(TARGET(sup-1)) & STATUS_SOURCE)
        break;
    delta = nactive_items - sup;
    pdata->nsources -= delta;
    nactive_items -= delta;
  }

  if (committed && dvar_is_integer(pdata->limitvar))
    for (i=pdata->ntargets; i<nactive_items; i++) {
      TASK si = TARGET(i);
      
      STATUS(si) &= ~(STATUS_TARGET<<4);
    }

 ret:
  Free(dtasks);
  CTagToArg(X(0),8) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),9) = MakeSmall(pdata->nsources);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam, Actions, ent);
}

#if DBG
extern void dump_tasks(struct unary_data *pdata);

void
dump_tasks(struct unary_data *pdata)
{
  int i;
  
  for (i=0; i<pdata->nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    printf("task %d\n", i);
    printf("  start in %" SPRIdINTEGER "..%" SPRIdINTEGER "\n\n", (SP_integer)t->est, (SP_integer)t->lct-t->dur);
  }
}
#endif /* DBG */
