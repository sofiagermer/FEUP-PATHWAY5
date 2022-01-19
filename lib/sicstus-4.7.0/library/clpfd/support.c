/* Copyright(C) 1999, Swedish Institute of Computer Science */

/* Before including anything. Ensures we do not try to call debug
   versions that are not exported from the runtime. */
#define SPIO_FORCE_INLINE_INTRINSICS 1

#include "fd.h"
#include "spio/spio_intrinsics.h"
#if __clang__
/* [PM] 4.2.1 Suppress clang warnings. I do not want to modify the code. */
#pragma clang diagnostic ignored "-Warray-bounds"
#endif  /* __clang__ */

/* Returns FD attribute for a dvar or integer.
   Heap consumption bounds:
   existing dvar - 0
   integer - INT_ATTRIBUTE_SIZE
   non-domain variable - FD_ATTRIBUTE_SIZE + ARITYLIMIT + 4
*/
TAGGED fd_check_argument(Wam wam,
			 TAGGED argument,
			 TAGGED min, TAGGED max, TAGGED size, SP_BOOL is_fd)
{
  TAGGED *h, *slot, t1, attr;
  int j;
  (void)is_fd;

  h = w->global_top;
  DerefSwitch(argument,;);
  switch (TagOf(argument)) {
  case REF_TAG:
    if (!VarIsSVA(argument,h) && GVarIsCVA(argument)) {
      if ((t1=get_attributes(argument,fd.fd_module)))
	return t1;
    }
    SP_ASSERT(!is_fd);
    attr = MakeStructure(h);
    for (j=0; j<FD_ATTRIBUTE_SIZE; j++) {
      t1 = fd_attribute[j];
      if (!TagIsAtomic(t1))
	t1 += TagREF(w->global_top);
      *h++ = t1;
    }
    slot = w->global_top + FD_ATTR_VAR_OFFSET;
    w->global_top[FD_ATTR_MIN_OFFSET] = min;
    w->global_top[FD_ATTR_MAX_OFFSET] = max;
    w->global_top[FD_ATTR_SIZE_OFFSET] = size;
    w->global_top = h;
    put_attributes(argument,attr,fd.fd_module); /* cannot GC */
    DerefSwitch(argument,;);
    *slot = argument;
    return attr;
  case CONST_TAG:
    SP_ASSERT(!is_fd);
    if (TagIsATM(argument)) {
      return ERRORTAG;
    } else if (argument==TaggedZero) {
      attr = CTagToArg(RefTerm(CLPFD_DATA_TERM_REF),1);
    } else if (argument==TaggedOne) {
      attr = CTagToArg(RefTerm(CLPFD_DATA_TERM_REF),2);
    } else {
      attr = MakeStructure(h);
      *h++ = fd_attribute[0];
      *h++ = fd_attribute[1];
      *h++ = argument;
      *h++ = attr + WD(7);
      *h++ = TaggedZero;
      *h++ = TaggedZero;
      *h++ = TaggedZero;	/* AFC, 4.7.0 */
      /* 7*/*h++ = functor_Dmutable;
      *h++ = attr + WD(10);
      *h++ = TaggedZero;
      /*10*/*h++ = functor_dom4;
      *h++ = MakeList(w->global_top+15);
      /*12*/*h++ = argument;
      *h++ = argument;
      *h++ = TaggedOne;
      /*15*/*h++ = MakeList(w->global_top+12);
      *h++ = atom_nil;
      /* XREF: #define INT_ATTRIBUTE_SIZE 17 */
      w->global_top = h;
    }
    return attr;
  default:
    SP_ASSERT(!is_fd);
    return ERRORTAG;
  }
}


/* '$fd_arg_attribute'(+Var, +Finitep, -Attr)
*/
void SPCDECL
prolog_fd_arg_attribute(Wam wam,
			SP_term_ref Var,
			SP_integer finitep,
			SP_term_ref Attr)
{
  TAGGED attr, domain;

  attr = fd_check_argument(wam,RefTerm(Var),Inf,Sup,Sup,FALSE);
  if (attr && finitep) {
    DomFromAttr(domain,attr); /* dom/4 term */
    if (!AreSmall(DomainMin(domain),DomainMax(domain)))
      attr = ERRORTAG;
  }
  if (attr)
    RefTerm(Attr) = attr;
  else
    SP_fail();  
}

/* '$fd_dvar_list'(+List, +Finitep)
*/
void SPCDECL
prolog_fd_dvar_list(Wam wam,
		    SP_term_ref List,
		    SP_integer finitep)
{
  TAGGED domain;

  /* [MC] SPRM 8731 - don't over-estimate memory need */
  /* 4.3.5 - UWN #170 - reject cyclic lists */

  DEREF(X(0),RefTerm(List));
  X(2) = X(0);			/* X(2) moves twice as fast as X(0) */
  while (TagIsLST(X(0))) {
    DerefCar(X(1),X(0));
    DerefCdr(X(0),X(0));
    if (TagIsLST(X(2))) {
      DerefCdr(X(2),X(2));
      if (TagIsLST(X(2))) {
	DerefCdr(X(2),X(2));
	if (X(2)==X(0))		/* X(2) caught up with X(0) = loop */
	  goto fail;
      }
    }
    if (!TagIsSmall(X(1))) {
      FdMemRequireHeap(FD_CHECK_ARGUMENT_NEED,3);
      X(1) = fd_check_argument(wam,X(1),Inf,Sup,Sup,FALSE);
      if (!X(1))
	goto fail;
      if (finitep) {
	DomFromAttr(domain,X(1)); /* dom/4 term */
	if (!AreSmall(DomainMin(domain),DomainMax(domain)))
	  goto fail;
      }
    }
  }
  if (X(0)==atom_nil)		/* type error? */
    goto ret;
 fail:
  SP_fail();
 ret:
  SP_ASSERT(w->global_top <= w->heap_warn);
}

/* '$fd_coref'(+List)
   Succeeds if List contains F1(X) and F2(X) for some X.
*/
void SPCDECL
prolog_fd_coref(Wam wam, SP_term_ref List)
{
  TAGGED list, var;

  DEREF(list,RefTerm(List));
  while (TagIsLST(list)) {
    DerefCar(var,list);
    DerefCdr(list,list);
    var = CTagToArg(var,1);
    DerefHeapSwitch(var,goto bind;);
    if (var==atom_nil)		/* F(Var), 2nd occurrence */
      return;
    else
      continue;			/* F(Integer) */
  bind:				/* F(Var), 1st occurrence */
    TrailPushCheck(var);
    CTagToPointer(var) = atom_nil;
  }
  SP_fail();			/* No coreference found. */
}

SP_BOOL fd_member(TAGGED x, TAGGED set)
{
  TAGGED range, min, max;

  while (set!=EmptySet) {
    range = CTagToCar(set); 
    set = CTagToCdr(set);
    min = RangeMin(range);
    max = RangeMax(range);
    if (!AreSmall(min,max)) {
      if (min==Inf)
	min = InfAsINT;
      if (max==Sup)
	max = SupAsINT;
    }
    if (Tle(x,max))
      return Tge(x,min);
  }
  return FALSE;
}


SP_BOOL fd_intersects_else(TAGGED x, TAGGED y, TAGGED set, TAGGED *tail)
{
  TAGGED range, min, max;

  if (!AreSmall(x,y)) {
    if (x==Inf)
      x = InfAsINT;
    if (y==Sup)
      y = SupAsINT;
  }
  while (set!=EmptySet) {
    range = CTagToCar(set); 
    set = CTagToCdr(set);
    min = RangeMin(range);
    max = RangeMax(range);
    if (!AreSmall(min,max)) {
      if (min==Inf)
	min = InfAsINT;
      if (max==Sup)
	max = SupAsINT;
    }
    if (Tle(x,max))
      return Tge(y,min);
    *tail = set;
  }
  return FALSE;
}

SP_BOOL fd_check_overflow(Wam wam, TAGGED goal)
{
  if (!fd.fd_overflow) {
    return TRUE;
  } else if (!fd.overflowing) {
    SP_fail();
    return FALSE;
  } else {
    int res;
    SP_term_ref goalref = SP_new_term_ref();
    SP_term_ref argref = SP_new_term_ref();

    RefTerm(goalref) = goal;
    RefTerm(argref) = MakeSmall(fd.fd_overflow); /* [MC] SPRM 13682 */

    res = SP_query(fd.overflow_action2, goalref, argref);
    if (res == SP_ERROR) {
      if (SP_exception_term(goalref) == 0) {
	/* [PM] 4.4.0 SP_ERROR, but no exception term. E.g. invalid
	   arguments to SP_query(), this should never happen. */
	SP_SOFT_ASSERT(fd.overflow_action2 != NULL); /* Happened in 4.0.0beta2 early build */
	SP_SOFT_ASSERT(0);
	SP_put_string(goalref, "CLPFD Internal Error");
      }
      SP_raise_exception(goalref);
    } else {
      SP_ASSERT(SP_exception_term(goalref) == 0); /* should always hold, unless SP_ERROR */
    }
    
    SP_reset_term_refs(goalref);
    SP_fail();
    return FALSE;
  }
}

/* Build a copy of old on the heap.
   Precondition: old is built entirely on the numstack.
*/
TAGGED fd_globalize(Wam wam,
		    TAGGED old, SP_integer req, int ar)
{
  TAGGED d1, r1, b, e, *h, value;
  TAGGED *valuep = &value;

  d1 = old;
  while (d1!=EmptySet) {
    req += 4;
    d1 = CTagToCdr(d1);
  }
  FdMemRequireHeap(req,ar);
  h = w->global_top;
  d1 = old;
  while (d1!=EmptySet) {
    r1 = CTagToCar(d1);
    d1 = CTagToCdr(d1);
    b = RangeMin(r1);
    e = RangeMax(r1);
    *valuep = MakeList(h);
    valuep = h+1;
    h[0] = MakeList(h+2);
    h[2] = b;
    h[3] = e;
    h += 4;
    if (b==Sup)
      fd.fd_overflow = 2;		/* FD integer overflow */
    else if (e==Inf)
      fd.fd_overflow = 1;		/* FD integer underflow */
  }
  *valuep = atom_nil;
  w->global_top = h;
  return value;
}

/* Build a copy of old on the heap.
   Precondition: old has NOT been fd_localized.
   In this context, no dvars to be exported can have a live dset pointing to the heap.
*/
TAGGED fd_globalize_unsafe(Wam wam,
			   TAGGED old, SP_integer pad, int ar)
{
  TAGGED d1, r1, b, e, *h, value;
  TAGGED *valuep = &value;
  
  ClearGCHazard;
  while (TRUE) {
    SP_integer req = pad;
    d1 = old;
    while (d1!=EmptySet && !OnHeap((TAGGED *)d1)) {
      r1 = CTagToCar(d1);
      d1 = CTagToCdr(d1);
      req += (OnHeap((TAGGED *)r1) ? 2 : 4);
    }
    if (w->stack_start-w->global_top >= CALLPAD+req)
      break;
    old = fd_localize(wam,old);
    call_overflow(w,req, ar);
  }
  SetGCHazard;
  h = w->global_top;
  d1 = old;
  while (d1!=EmptySet && !OnHeap((TAGGED *)d1)) {
    r1 = CTagToCar(d1);
    d1 = CTagToCdr(d1);
    *valuep = MakeList(h);
    valuep = h+1;
    h += 2;
    if (OnHeap((TAGGED *)r1)) {
      h[-2] = r1;
    } else {
      b = RangeMin(r1);
      e = RangeMax(r1);
      h[-2] = MakeList(h);
      h[0] = b;
      h[1] = e;
      h += 2;
      if (b==Sup)
	fd.fd_overflow = 2;		/* FD integer overflow */
      else if (e==Inf)
	fd.fd_overflow = 1;		/* FD integer underflow */
    }
  }
  *valuep = d1;
  w->global_top = h;
  return value;
}



void fd_update_mutable(Wam wam, TAGGED new_value, TAGGED mutable)
{
  TAGGED *h, *arg;
  
  arg = TagToArg(mutable,0);
  if (arg[2] < TrailToInt(w->node->trail_top) &&
      arg < w->global_uncond) {
    h = w->trail_top; /* trail mutable if need be */
    *h++ = mutable;
    *h++ = arg[1];
    *h++ = arg[2];
    arg[1] = new_value;	/* must come BEFORE sp_choice_overflow */
    arg[2] = TrailToInt(w->trail_top);
    w->trail_top = h;
    if ((TAGGED *)w->node-CHOICEPAD<w->trail_top)
      sp_choice_overflow(w,CHOICEPAD);
  } else
    arg[1] = new_value;
}

/* ASSERT: var already dvar */
/* Heap need: FD_LINK_NEED words, ensured by caller */
TAGGED fd_link(Wam wam,
	       TAGGED var,
	       TAGGED key,
	       TAGGED item,
	       TAGGED trigger)
{
  DEREF(var,var);
  if (IsVar(var)) {
    TAGGED attr = fd_check_argument(wam,var,Inf,Sup,Sup,TRUE);
    fd_link_attr(wam, attr, key, item, trigger);
    return attr;
  } else {
    return TaggedZero;
  }
}

/* Heap need: FD_LINK_NEED words, ensured by caller */
void fd_link_attr(Wam wam,
		  TAGGED attr,
		  TAGGED key,
		  TAGGED item,
		  TAGGED trigger)
{
  TAGGED mutable, dom, *h, lists, *s=NULL, tail;
  TAGGED functor = TagToHeadfunctor(item);
  SP_integer mask, count = 1;
  DECL_UPDATE_MUTABLE;
  int lix=0;
  
  AttrToDomM(attr,mutable);
  dom = RefMutable(mutable);
  if (DomainSize(dom)==TaggedOne)
    return;
  AttrToSuspM(attr,mutable);
  lists = RefMutable(mutable);
  h = w->global_top;
  
  if (TagToSTR(lists) < w->global_uncond)/* can't smash */ {
    TAGGED new_lists = MakeStructure(h);
    int i;
	  
    for (s = TagToSTR(lists), i=0; i<FD_LISTS_SIZE; i++)
      *h++ = *s++;
    FD_UPDATE_MUTABLE(new_lists,mutable);
    lists = new_lists;
  }
  if (key==functor_dom1) {
    mask = IStep(MASK_DOM);
    if (functor==functor_ix8) {
      lix = FD_LIST_DOM_IX;
    } else if (functor==functor_daemon6) {
      lix = FD_LIST_DOM_DAEMON;
    } else if (functor==functor_global6) {
      lix = FD_LIST_DOM_GLOBAL;
    }
  } else if (key==fd.functor_min) {
    mask = IStep(MASK_MIN);
    if (functor==functor_ix8) {
      lix = FD_LIST_MIN_IX;
    } else if (functor==functor_daemon6) {
      lix = FD_LIST_MIN_DAEMON;
    } else if (functor==functor_global6) {
      lix = FD_LIST_MIN_GLOBAL;
    }
  } else if (key==fd.functor_max) {
    mask = IStep(MASK_MAX);
    if (functor==functor_ix8) {
      lix = FD_LIST_MAX_IX;
    } else if (functor==functor_daemon6) {
      lix = FD_LIST_MAX_DAEMON;
    } else if (functor==functor_global6) {
      lix = FD_LIST_MAX_GLOBAL;
    }
  } else if (key==fd.functor_minmax) {
    mask = IStep(MASK_MINMAX);
    if (functor==functor_ix8) {
      lix = FD_LIST_MINMAX_IX;
    } else if (functor==functor_daemon6) {
      lix = FD_LIST_MINMAX_DAEMON;
    } else if (functor==functor_global6) {
      lix = FD_LIST_MINMAX_GLOBAL;
    }
  } else {
    mask = IStep(MASK_VAL);
    if (functor==functor_ix8) {
      lix = FD_LIST_VAL_IX;
    } else if (functor==functor_daemon6) {
      lix = FD_LIST_VAL_DAEMON;
    } else if (functor==functor_global6) {
      lix = FD_LIST_VAL_GLOBAL;
    } else if (functor==functor_watcher5) {
      lix = FD_LIST_VAL_WATCHER;
    } else if (functor==functor_disequation4) {
      lix = FD_LIST_VAL_DISEQ;
    } else {
      lix = FD_LIST_VAL_IMP0 + GetSmall_int0(trigger);
    }
  }
  s = TagToArg(lists,lix);
  /* begin [MC] 4.0.5: skip prefix of entailed propagators */
  tail = *s;
  count += GetSmall(CTagToArg(lists,1));
  if (lix < FD_LIST_VAL_IMP0 || lix > FD_LIST_VAL_DISEQ)
    while (tail!=EmptySet) {
      TAGGED qitem = CTagToCar(tail);
      TAGGED entvar = CTagToArg(qitem,4);
    
      DerefSwitch(entvar,goto nonent;);
      count--;
      tail = CTagToCdr(tail);
    }
 nonent:
  /* end   [MC] 4.0.5 */
  CTagToArg(lists,1) = MakeSmall(count);
  CTagToArg(lists,2) |= mask;
  HeapPush(h,item);
  HeapPush(h,tail);
  *s = MakeList(h-2);
  w->global_top = h;
}


/*** support for queues of indexicals and globals ***/

/* [PM] 4.1.0 break out magic number */
COMPILE_TIME_ASSERT_DECLARATION(CLPFD_MUTABLE_TERM_REF == 5);
#define CLPFD_MUTABLE RefTerm(CLPFD_MUTABLE_TERM_REF) /* xref Emulator/sicstus.c */

void fd_sync(Wam wam)
{
  TAGGED ptr = RefMutable(CLPFD_MUTABLE);
  struct propagator *current = (struct propagator *)TermToPointer(ptr);
  struct propagator *cur = fd.current_propagator;

  while (cur && cur!=current) {
    int i;
    for (i=0; i<FD_NB_QUEUES; i++) {
      cur->queue[i].head = cur->queue[i].tail = cur->queue[i].bottom;
    }
    cur->hint = 0;
    fd.current_propagator = cur->next;
    cur->next = fd.free_propagators;
    fd.free_propagators = cur;
    cur = fd.current_propagator;
  }
  /* [MC 3.11.1] see SPRM 7785.  Shouldn't happen, but does.  Exact cause
     not understood.  Hypothesis: a combination of freeze and
     backtracking causes contents of CLPFD_MUTABLE to be outside the
     fd.current_propagator chain.  Rescue op: pop fd.free_propagators
     stack until found. */

  while (cur!=current) {
    cur = fd.free_propagators;
    fd.free_propagators = cur->next;
    cur->next = fd.current_propagator;
    fd.current_propagator = cur;
  }
}


/* '$fd_begin'
*/
void SPCDECL
prolog_fd_begin(Wam wam)
{
  struct propagator *cur;
  int i;
  DECL_UPDATE_MUTABLE;
  
  if (!fd.batching) {
    fd_sync(wam);
    if (fd.free_propagators) {
      cur = fd.free_propagators;
      fd.free_propagators = cur->next;
    } else {
      cur = (struct propagator *)sp_checkalloc(sizeof(struct propagator), TRUE);
      for (i=0; i<FD_NB_QUEUES; i++) {
	cur->queue[i].bottom = (SP_globref)sp_checkalloc(4*sizeof(*cur->queue[i].bottom), TRUE);
	memset((char *)cur->queue[i].bottom, 0, 4*sizeof(*cur->queue[i].bottom));
	cur->queue[i].head = cur->queue[i].tail = cur->queue[i].bottom;
	cur->queue[i].top = cur->queue[i].bottom + 4;
      }
      cur->hint = 0;
    }
    cur->next = fd.current_propagator;
    fd.current_propagator = cur;
    FD_UPDATE_MUTABLE(PointerToTerm(cur),CLPFD_MUTABLE);
  }
}


/* '$fd_end'
*/
void fd_end(Wam wam)
{
  struct propagator *cur;
  DECL_UPDATE_MUTABLE;

  if (!fd.batching) {
    fd_sync(wam);
    cur = fd.current_propagator;
    fd.current_propagator = cur->next;
    cur->next = fd.free_propagators;
    fd.free_propagators = cur;
    FD_UPDATE_MUTABLE(PointerToTerm(fd.current_propagator),CLPFD_MUTABLE);
  }
}


void
fd_dealloc(Wam wam)
{
  struct propagator *cur;
  
  RefMutable(CLPFD_MUTABLE) = TaggedZero;
  fd_sync(wam);
  while ((cur=fd.free_propagators)) {
    int i;
    for (i=0; i<FD_NB_QUEUES; i++)
      sp_checkdealloc((TAGGED *)cur->queue[i].bottom,
		      (cur->queue[i].top-cur->queue[i].bottom)*sizeof(*cur->queue[i].bottom), 
		      TRUE); /* 4.0.5 [MC] */
    fd.free_propagators = cur->next;
    sp_checkdealloc((TAGGED *)cur, sizeof(struct propagator), TRUE);
  }
}  


/* support function for stack shifting and gc */

void SPCDECL
fd_manager_hook(Wam wam, struct worker *ignore,int msg,TAGGED *ptr)
{
  SP_integer reloc;
  struct propagator *cur;
  TAGGED t, *p;
  int j;

  (void)ignore;
  switch (msg) {
  case 1:			/* stack shifter */
    reloc = (SP_integer)ptr;
    fd_sync(wam);
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	struct prop_queue *q = &cur->queue[j];
	SP_globref r = q->head;
	while (r != q->tail) {
	  if (!TagIsAtomic(r->term[0]))
	    r->term[0] += reloc;
	  r++;
	  if (r == q->top)
	    r = q->bottom;
	}
      }
      cur = cur->next;
    }
    break;
  case 2:			/* gc, mark phase */
    fd_sync(wam);
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	struct prop_queue *q = &cur->queue[j];
	SP_globref r = q->head;
	while (r != q->tail) {
	  markTerm(r->term[0]);
	  r++;
	  if (r == q->top)
	    r = q->bottom;
	} 
      }
      cur = cur->next;
    }
    break;
  case 3:			/* gc, sweep phase */
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	struct prop_queue *q = &cur->queue[j];
	SP_globref r = q->head;
	while (r != q->tail) {
	  t = r->term[0];
	  p = TagToPointer(t);
	  if (!TagIsAtomic(t) && OffHeaptop(p,ptr))
	    intoRelocationChain(w,p,&r->term[0]);
	  r++;
	  if (r == q->top)
	    r = q->bottom;
	}
      }
      cur = cur->next;
    }
    break;
  case 4:			/* gc, proofreading */
    fd_sync(wam);
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	struct prop_queue *q = &cur->queue[j];
	SP_globref r = q->head;
	while (r != q->tail) {
	  (*(void(*)(TAGGED *))ptr)(&r->term[0]);
	  r++;
	  if (r == q->top)
	    r = q->bottom;
	}
      }
      cur = cur->next;
    }
    break;
  case 5:			/* labeling */
    fd_labeling(wam, ptr);
    break;
  }
}



struct daemon_frame {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,&global) */
};

void fd_expand_prop_queue(Wam wam, struct prop_queue *q)
{
  ptrdiff_t size = q->top - q->bottom;
  ptrdiff_t offset = q->head - q->bottom;
  SP_globref s;

  s = q->bottom = (SP_globref)sp_checkrealloc((TAGGED *)q->bottom,
					      size*sizeof(*s),
					      2*size*sizeof(*s), TRUE);
  q->top = s + 2*size;
  q->head = s + offset;
  q->tail = s + size;
  memset((char *)(s+size), 0, size*sizeof(*s));
  while (s < q->head)
    *q->tail++ = *s++;
}

/* enqueue if not already in the queue */
/* where is as above */
static void fd_enqueue_global(Wam wam, TAGGED item, int queue, int append)
{
  TAGGED t1 = CTagToArg(item,3);
  TAGGED status = RefMutable(t1);
  struct propagator *cur = fd.current_propagator;
  DECL_UPDATE_MUTABLE;
  
  if (!(status & IStep(1))) {	/* STATUS: not enqueued */
    if (append) {
      fd_enqueue_append(wam,item,queue);
    } else {
      fd_enqueue_prepend(wam,item,queue);
    }
  }
}

/*-1 -- failure
   0 -- empty queue
   1 -- dequeued indexical
   2 -- dequeued global
*/
int fd_dequeue(Wam wam, TAGGED *item)
{
  struct propagator *cur = fd.current_propagator;
  int queue;

 start:
  if (cur->hint==0)
    return 0;
  queue = spio_count_trailing_zeros32(cur->hint);
  switch (queue) {
  case FD_QUEUE_OFFSET:
    {
      struct prop_queue *q = &cur->queue[FD_QUEUE_OFFSET];
      SP_globref s = q->head;

      while (s != q->tail) {
	TAGGED dom = (s+0)->term[0];
	TAGGED why = (s+1)->term[0];
	TAGGED value = (s+2)->term[0];
	TAGGED list = (s+3)->term[0];
	s += 4;
	if (s == q->top)
	  s = q->bottom;
	q->head = s;
	if (!propagate_offsets(wam, dom, why, value, list))
	  return -1;
	s = q->head;
      }
      cur->hint &= ~(1U << FD_QUEUE_OFFSET);
      break;
    }

  case FD_QUEUE_IMP:
    {
      struct prop_queue *q = &cur->queue[FD_QUEUE_IMP];
      SP_globref s = q->head;

      while (s != q->tail) {
	TAGGED list = (s++)->term[0];
	if (s == q->top)
	  s = q->bottom;
	q->head = s;
	if (!propagate_implications(wam, list))
	  return -1;
	s = q->head;
      }
      cur->hint &= ~(1U << FD_QUEUE_IMP);
      break;
    }

  case FD_QUEUE_DISEQ:
    {
      struct prop_queue *q = &cur->queue[FD_QUEUE_DISEQ];
      SP_globref s = q->head;
      TAGGED list = (s++)->term[0];
      
      if (s == q->top)
	s = q->bottom;
      q->head = s;
      if (s == q->tail)
	cur->hint &= ~(1U << FD_QUEUE_DISEQ);
      if (!propagate_disequations(wam, list))
	return -1;
      break;
    }

  case FD_QUEUE_WATCHER:
    {
      struct prop_queue *q = &cur->queue[FD_QUEUE_WATCHER];
      SP_globref s = q->head;
      TAGGED value = (s+0)->term[0];
      TAGGED list = (s+1)->term[0];
      
      s += 2;
      if (s == q->top)
	s = q->bottom;
      q->head = s;
      if (s == q->tail)
	cur->hint &= ~(1U << FD_QUEUE_WATCHER);
      if (!propagate_watchers(wam, list, value))
	return -1;
      break;
    }

  case FD_QUEUE_DAEMON:
    {
      struct prop_queue *q = &cur->queue[FD_QUEUE_DAEMON];
      SP_globref s = q->head;
      while (s != q->tail) {
	TAGGED term = (s++)->term[0];
	TAGGED entvar;

	if (s == q->top)
	  s = q->bottom;
	Deref1(entvar,CTagToArg(term,4));
	if (!IsVar(entvar))
	  continue;
	{
	  TAGGED global = CTagToArg(term,1);
	  SP_globref attr_ref = (SP_globref)TermToPointer(CTagToArg(term,2));
	  int queue_ix = GetSmall_int(CTagToArg(term,5));
	  struct daemon_frame *handle = (struct daemon_frame *)TermToPointer(CTagToArg(term,6));
	  
	  switch ((*handle->daemon)(wam, handle, attr_ref, &global)) {
	  case DAEMON_NOFIX:
	  case DAEMON_ENTAIL:
	    fd_enqueue_global(wam, global, queue_ix, TRUE);
	    break;
	  case DAEMON_FIX:
	    break;
	  /* case DAEMON_ENTAIL: not useful, because the propagator must dealloc memory */
	  /*   BindHVA(entvar,TaggedOne); */
	  /*   break; */
	  case DAEMON_FAIL:
	    global_bump_afc(global);
	    return -1;
	  }
	}
      }
      q->head = s;
      cur->hint &= ~(1U << FD_QUEUE_DAEMON);
      break;
    }

  case FD_QUEUE_IX:
  case FD_QUEUE_VAL:
  case FD_QUEUE_MINMAX:
  case FD_QUEUE_DOM:
    {
      struct prop_queue *q = &cur->queue[queue];
      SP_globref s = q->head;

      while (s != q->tail) {
	TAGGED term = (s++)->term[0];
	TAGGED entvar;
	
	if (s == q->top)
	  s = q->bottom;
	q->head = s;
	if (s == q->tail)
	  cur->hint &= ~(1U << queue);
	Deref1(entvar,CTagToArg(term,4));
	if (!IsVar(entvar))
	  continue;
	*item = term;
	return queue==FD_QUEUE_IX ? 1 : 2;
      }
      break;
    }

 case FD_QUEUE_WAKE:
  default:
   {
     struct prop_queue *q = &cur->queue[FD_QUEUE_WAKE];
     SP_globref s = q->head;
     while (s != q->tail) {
       TAGGED term = (s++)->term[0];
       TAGGED var;

       if (s == q->top)
	 s = q->bottom;
       DerefArg(var,term,1);
       if (IsVar(var)) {
	 delete_attributes(var, fd.fd_module, TRUE, EVAL_ARITY); /* GC + deref */
       }
     }
     s = q->head;
     while (s != q->tail) {
       TAGGED term = (s++)->term[0];
       TAGGED var, value;

       if (s == q->top)
	 s = q->bottom;
       DerefArg(var,term,1);
       DerefArg(value,term,2);
       if (IsVar(var)) {
	 Wake; BindCVA(var,value); /* [MC] SPRM 12274 */
       } else if (var!=value) {
	 return -1;
       }
     }
     q->head = s;
     cur->hint &= ~(1U << FD_QUEUE_WAKE);
     break;
   }
  }
  goto start;
}

#define PROTECTED_LISTS_MUTABLE X(3)
#define PROTECTED_LIST          X(4)
#define PROTECTED_TAIL          X(5)
#define NB_PROTECTED               3

static void fd_enqueue_list_gc_daemon(Wam wam, struct propagator *cur, int index)
{
  TAGGED list_term = RefMutable(PROTECTED_LISTS_MUTABLE);
  TAGGED list = CTagToArg(list_term,index);
  
  if (TagToSTR(list_term) >= w->global_uncond) { /* can smash: skip prefix of entailed frobs */
    while (list!=EmptySet) {
      TAGGED item = CTagToCar(list);
      TAGGED entvar;

      Deref1(entvar, CTagToArg(item,4));
      if (IsVar(entvar))
	break;
      list = CTagToCdr(list);
    }
    CTagToArg(list_term,index) = list;
  }
  while (list!=EmptySet) {
    TAGGED item = CTagToCar(list);
    TAGGED status = RefMutable(CTagToArg(item,3));
    
    list = CTagToCdr(list);
    if (!(status & IStep(16)) && (status & IStep(12)) != IStep(8)) { /* STATUS: !entailed and not current or not idempotent */
      fd_enqueue_append_nomut(wam, item, FD_QUEUE_DAEMON);
    }
  }
}

static void fd_enqueue_list_gc_global_or_ix(Wam wam, struct propagator *cur, int index, int queue, TAGGED safe_time_stamp)
{
  TAGGED list_term = RefMutable(PROTECTED_LISTS_MUTABLE);
  TAGGED list = CTagToArg(list_term,index);

  if (TagToSTR(list_term) >= w->global_uncond) { /* can smash: skip prefix of entailed frobs */
    while (list!=EmptySet) {
      TAGGED item = CTagToCar(list);
      TAGGED entvar;

      Deref1(entvar, CTagToArg(item,4));
      if (IsVar(entvar))
	break;
      list = CTagToCdr(list);
    }
    CTagToArg(list_term,index) = list;
  }
  while (list!=EmptySet) {
    TAGGED item = CTagToCar(list);
    TAGGED entvar;
    
    list = CTagToCdr(list);
    Deref1(entvar,CTagToArg(item,4));
    if (IsVar(entvar)) {
      TAGGED status_mut = CTagToArg(item,3);
      TAGGED status = RefMutable(status_mut);
      TAGGED reif_dom_mutable, reif_dom;
      if (status & IStep(0x71)) { /* STATUS: guarded indexical, or entailed, or enqueued */
	if (status & IStep(0x11)) /* STATUS: entailed, or enqueued */
	  continue;
	AttrToDomM(CTagToArg(item,7),reif_dom_mutable);
	reif_dom = RefMutable(reif_dom_mutable);
	if ((status & IStep(0x60)) == IStep(0x60)) {
	  if (DomainSize(reif_dom) == TaggedOne)
	    continue;
	} else if (status & IStep(0x40)) {
	  if (DomainMin(reif_dom) != TaggedOne)
	    continue;
	  else
	    FD_UPDATE_MUTABLE(status & ~IStep(0x60),status_mut); /* STATUS: ~guarded indexical */
	} else {
	  if (DomainMax(reif_dom) != TaggedZero)
	    continue;
	  else
	    FD_UPDATE_MUTABLE(status & ~IStep(0x60),status_mut); /* STATUS: ~guarded indexical */
	}
      }
      fd_enqueue_append(wam, item, queue);
    }
  }
}

/* ASSERT: no GC in scope of this */
static void fd_enqueue_list_daemon(Wam wam, struct propagator *cur, int index)
{
  TAGGED list = CTagToArg(RefMutable(PROTECTED_LISTS_MUTABLE),index);
  
  while (list!=EmptySet) {
    TAGGED item = CTagToCar(list);
    TAGGED status = RefMutable(CTagToArg(item,3));
    
    list = CTagToCdr(list);
    if (!(status & IStep(16)) && (status & IStep(12)) != IStep(8)) { /* STATUS: !entailed and not current or not idempotent */
      fd_enqueue_prepend_nomut(wam, item, FD_QUEUE_DAEMON);
    }
  }
}

/* ASSERT: no GC in scope of this */
static void fd_enqueue_list_global_or_ix(Wam wam, struct propagator *cur, int index, int queue, TAGGED safe_time_stamp)
{
  TAGGED list = CTagToArg(RefMutable(PROTECTED_LISTS_MUTABLE),index);

  while (list!=EmptySet) {
    TAGGED item = CTagToCar(list);
    TAGGED entvar;
    
    list = CTagToCdr(list);
    Deref1(entvar,CTagToArg(item,4));
    if (IsVar(entvar)) {
      TAGGED status_mut = CTagToArg(item,3);
      TAGGED status = RefMutable(status_mut);
      TAGGED reif_dom_mutable, reif_dom;
      if (status & IStep(0x71)) { /* STATUS: guarded indexical, or entailed, or enqueued */
	if (status & IStep(0x11)) /* STATUS: entailed, or enqueued */
	  continue;
	AttrToDomM(CTagToArg(item,7),reif_dom_mutable);
	reif_dom = RefMutable(reif_dom_mutable);
	if ((status & IStep(0x60)) == IStep(0x60)) {
	  if (DomainSize(reif_dom) == TaggedOne)
	    continue;
	} else if (status & IStep(0x40)) {
	  if (DomainMin(reif_dom) != TaggedOne)
	    continue;
	  else
	    FD_UPDATE_MUTABLE(status & ~IStep(0x60),status_mut); /* STATUS: ~guarded indexical */
	} else {
	  if (DomainMax(reif_dom) != TaggedZero)
	    continue;
	  else
	    FD_UPDATE_MUTABLE(status & ~IStep(0x60),status_mut); /* STATUS: ~guarded indexical */
	}
      }
      fd_enqueue_prepend(wam, item, queue);
    }
  }
}

/* assuming bits > 0 */
/* X(1), X(2) must survive over this */
void fd_enqueue_all(Wam wam, int bits, TAGGED lists_loc, TAGGED value)
{
  struct propagator *cur = fd.current_propagator;
  TAGGED lists = RefMutable(lists_loc);
  DECL_UPDATE_MUTABLE;

  PROTECTED_LISTS_MUTABLE = lists_loc;
  if (bits & MASK_SINGLETON) {
    /* 3.9: enqueue all constraints when ground,
       (a) to maximize entailment detection,
       (b) to handle co-references */
    bits |= GetSmall_int(CTagToArg(lists,2)); /* bitmask of susp. lists */
    /* we are about to prepend---prepend slowest first! */
    if (bits & MASK_DOM) {
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_DOM_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_daemon(wam, cur, FD_LIST_DOM_DAEMON);
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_DOM_GLOBAL, FD_QUEUE_DOM, safe_time_stamp);
    }
    if (bits & MASK_MIN) {
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_MIN_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_daemon(wam, cur, FD_LIST_MIN_DAEMON);
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_MIN_GLOBAL, FD_QUEUE_MINMAX, safe_time_stamp);
    }
    if (bits & MASK_MAX) {
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_MAX_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_daemon(wam, cur, FD_LIST_MAX_DAEMON);
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_MAX_GLOBAL, FD_QUEUE_MINMAX, safe_time_stamp);
    }
    if (bits & MASK_MINMAX) {
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_MINMAX_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_daemon(wam, cur, FD_LIST_MINMAX_DAEMON);
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_MINMAX_GLOBAL, FD_QUEUE_MINMAX, safe_time_stamp);
    }
    if (bits & MASK_VAL) {
      TAGGED list;
      if (value == TaggedZero) {
	list = CTagToArg(lists,FD_LIST_VAL_IMP0);
	if (list!=EmptySet) {
	  fd_enqueue_imp(wam, list);
	}
      }
      if (value == TaggedOne) {
	list = CTagToArg(lists,FD_LIST_VAL_IMP1);
	if (list!=EmptySet) {
	  fd_enqueue_imp(wam, list);
	}
      }
      if ((value | TaggedOne) == TaggedOne) {
	list = CTagToArg(lists,FD_LIST_VAL_DISEQ);
	if (list!=EmptySet) {
	  fd_enqueue_diseq(wam, list);
	}
	list = CTagToArg(lists,FD_LIST_VAL_WATCHER);
	if (list!=EmptySet) {
	  fd_enqueue_watcher(wam, value, list);
	}
      }
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_VAL_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_daemon(wam, cur, FD_LIST_VAL_DAEMON);
      fd_enqueue_list_global_or_ix(wam, cur, FD_LIST_VAL_GLOBAL, FD_QUEUE_VAL, safe_time_stamp);
    }
  } else {
    /* append slowest last! */
    if (bits & MASK_MINMAX) {
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_MINMAX_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_gc_daemon(wam, cur, FD_LIST_MINMAX_DAEMON);
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_MINMAX_GLOBAL, FD_QUEUE_MINMAX, safe_time_stamp);
    }
    if (bits & MASK_MAX) {
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_MAX_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_gc_daemon(wam, cur, FD_LIST_MAX_DAEMON);
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_MAX_GLOBAL, FD_QUEUE_MINMAX, safe_time_stamp);
    }
    if (bits & MASK_MIN) {
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_MIN_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_gc_daemon(wam, cur, FD_LIST_MIN_DAEMON);
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_MIN_GLOBAL, FD_QUEUE_MINMAX, safe_time_stamp);
    }
    if (bits & MASK_DOM) {
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_DOM_IX, FD_QUEUE_IX, safe_time_stamp);
      fd_enqueue_list_gc_daemon(wam, cur, FD_LIST_DOM_DAEMON);
      fd_enqueue_list_gc_global_or_ix(wam, cur, FD_LIST_DOM_GLOBAL, FD_QUEUE_DOM, safe_time_stamp);
    }
  }
}

/* implies $fd_begin */
void SPCDECL
prolog_fd_global_enqueue(Wam wam, SP_term_ref TermR)
{
  TAGGED term=RefTerm(TermR);
  TAGGED mutable;
  DECL_UPDATE_MUTABLE;

  SP_MANGLE(prolog_fd_begin)(wam);
  DerefNonvar(term);
  mutable = CTagToArg(term,3);
  FD_UPDATE_MUTABLE(RefMutable(mutable)|IStep(9),mutable); /* STATUS: enqueued, current */
  /*** done in Prolog
  fd_sync(wam);
  fd_enqueue(w, term, 0x3);
  ***/
}


/* Support for managing incumbents:
   '$fd_update_incumbent'(+Ref, +Value, +Vertex).
	store a new incumbent vertex and value
   '$fd_incumbent_bound'(+Ref, -Value).
	retrieve the current incumbent value.
*/

/* extract pointer from db_reference */
static struct instance *get_dbref(Wam wam, TAGGED ref)
{
  DerefNonvar(ref);
  (void) wam; /* TermToPointer() uses this, but only if DBG. */
  return (struct instance *)TermToPointer(CTagToArg(ref,1));
}


void SPCDECL
prolog_fd_update_incumbent(Wam wam,
				  SP_term_ref RefR,
				  SP_term_ref ValueR,
				  SP_term_ref VertexR)
{
  struct instance *ins = get_dbref(wam, RefTerm(RefR));
  int i;
  TAGGED vertex;
  
  DEREF(ins->code[1],RefTerm(ValueR));
  DEREF(vertex,RefTerm(VertexR));
  for (i=3; TagIsLST(vertex); i+=2) {
    DerefCar(ins->code[i],vertex);
    DerefCdr(vertex,vertex);
  }
}


void SPCDECL
prolog_fd_incumbent_bound(Wam wam,
				 SP_term_ref RefR,
				 SP_term_ref ValueR)
{
  struct instance *ins = get_dbref(wam, RefTerm(RefR));
  
  RefTerm(ValueR) = ins->code[1];
}


/* Support for Palloc/Pfree. */

void *fd_perm_alloc(Wam wam,
		    size_t nbytes,
		    TAGGED handle) /* HVA to bind */
{
  void *ptr;
  TAGGED tptr;
  TAGGED *h;
  TAGGED inner, outer;

  ptr = fd_malloc(wam, nbytes);
  tptr = PointerToTerm(ptr);
  FdMemRequireHeap1(4,handle,EVAL_ARITY);
  h = w->global_top;
  inner = MakeStructure(h);
  *h++ = functor_Dfree;
  *h++ = tptr;
  outer = MakeList(h);
  Load0HVA(h);
  *h++ = inner;
  w->global_top = h;
  TrailPushCheck(outer);
  BindHVA(handle,outer);
  return ptr;
}


void *fd_perm_data(Wam wam, TAGGED handle) /* [Flag | '$free'(Ptr)] */
{
  (void) wam; /* TermToPointer() uses this, but only if DBG. */
  handle = CTagToCdr(handle);
  handle = CTagToArg(handle,1);
  return TermToPointer(handle);
}

void fd_perm_free(Wam wam)
{
  struct node *nd;
  void (SPCDECL *destructor)(void *);
  ANYPOINTER frame;
  TAGGED handle;		/* [Flag | '$free'(Ptr)] */
  TAGGED flag;
  SP_BOOL committed;
  
  fd_static_output_state(wam,&handle,&committed);
  flag = CTagToCar(handle);
  if (committed) {
    BindHVA(flag,TaggedZero);	/* disable cleanup */
    frame = TermToPointer(CTagToArg(CTagToCdr(handle),1));
    destructor = *(void (SPCDECL **)(void*))frame;
    (*destructor)(frame);
    if (w->trail_top[-1]==handle)
      w->trail_top--;
  } else {
    BindHVA(flag,TaggedOne);	/* enable cleanup */
    for (nd = w->node;
	 (TAGGED *)nd<w->choice_start && !ChoiceptTestCleanup(nd);
	 nd = ChoiceptPrevious(nd))
      ChoiceptMarkCleanup(nd);
  }
}

/* sorry about the crock... no gc hazard here */
void
fd_common_done(Wam wam, int argno)
{
  TAGGED stash;

  stash = X(0);
  DerefArg(X(0),X(0),argno);	/* get state(...) */
  fd_perm_free(wam);
  X(0) = stash;
}

/* '$fd_trailed_Dfree'(-Item) :-
   If trail contains [Flag | '$free'(Ptr)] with unbound Flag,
   which is typically a memory leak, then Item = '$free'(Ptr), else Item = []. */
void SPCDECL
prolog_fd_trailed_Dfree(Wam wam, SP_term_ref Item)
{
  TAGGED *tr;
  for (tr=w->trail_top; tr>w->trail_start; ) {
    TAGGED item = *--tr;
    if (TagIsSmall(item)) {
      tr -= 2;
    } else if (TagIsLST(item)) {
      TAGGED car, cdr;
      DerefCar(car,item);
      DerefCdr(cdr,item);
      if (IsVar(car)) {
	RefTerm(Item) = cdr;
	return;
      }
    }
  }
}

static void
entail_global(Wam wam, TAGGED global)
{
  TAGGED statem = CTagToArg(global,1);
  TAGGED constraint = CTagToArg(global,2);
  TAGGED f = IsAtomic(constraint) ? constraint : TagToHeadfunctor(constraint);
  TAGGED ent;
  struct sw_on_key_node *hnode = incore_gethash(fd.dispatch,f);

  DerefArg(ent, global, 4);
  if (IsVar(ent)) {
    if (hnode->value.arities) {
      X(0) = RefMutable(statem);
      fd_perm_free(wam);
    }
  }
  BindHVA(ent,TaggedOne);
}


static void
entail_indexicals(Wam wam, TAGGED list)
{
  while (TagIsLST(list)) {
    TAGGED ix = CTagToCar(list);
    TAGGED ent;
    list = CTagToCdr(list);
    DerefArg(ent, ix, 4);
    if (IsVar(ent))
      BindHVA(ent,TaggedOne);
  }
}

static void
entail_daemons(Wam wam, TAGGED list)
{
  while (TagIsLST(list)) {
    TAGGED daemon = CTagToCar(list);
    TAGGED ent;
    list = CTagToCdr(list);
    DerefArg(ent, daemon, 4);
    if (IsVar(ent))
      entail_global(wam, CTagToArg(daemon,1));
  }
}

static void
entail_globals(Wam wam, TAGGED list)
{
  while (TagIsLST(list)) {
    TAGGED global = CTagToCar(list);
    list = CTagToCdr(list);
    entail_global(wam, global);
  }
}

static void
entail_disequations(Wam wam, TAGGED list)
{
  while (TagIsLST(list)) {
    TAGGED disequation = CTagToCar(list);
    TAGGED countm = CTagToArg(disequation,1);
    fd_update_mutable(wam, TaggedZero, countm);
    list = CTagToCdr(list);
  }
}

static void
entail_watchers(Wam wam, TAGGED list)
{
  while (TagIsLST(list)) {
    TAGGED watcher = CTagToCar(list);
    TAGGED ent;
    list = CTagToCdr(list);
    DerefArg(ent, watcher, 4);
    if (IsVar(ent))
      BindHVA(ent,TaggedOne);
  }
}


/* '$fd_purge'(+Var) :-
   where Var has FD attribute: force entailment of all attached constraints, and remove the attribute. */
void SPCDECL
prolog_fd_purge(Wam wam, SP_term_ref Var)
{
  TAGGED var, attr, mutable, susps, aliases, newaliases, *h, *tail;
  int n;

  DEREF(var,RefTerm(Var));
  attr = get_attributes(var,fd.fd_module);
  AttrToSuspM(attr, mutable);
  susps = RefMutable(mutable);
  AttrToAliasM(attr, mutable);
  aliases = RefMutable(mutable);
  entail_indexicals(wam, CTagToArg(susps,FD_LIST_DOM_IX));
  entail_daemons(wam, CTagToArg(susps,FD_LIST_DOM_DAEMON));
  entail_globals(wam, CTagToArg(susps,FD_LIST_DOM_GLOBAL));
  entail_indexicals(wam, CTagToArg(susps,FD_LIST_MIN_IX));
  entail_daemons(wam, CTagToArg(susps,FD_LIST_MIN_DAEMON));
  entail_globals(wam, CTagToArg(susps,FD_LIST_MIN_GLOBAL));
  entail_indexicals(wam, CTagToArg(susps,FD_LIST_MAX_IX));
  entail_daemons(wam, CTagToArg(susps,FD_LIST_MAX_DAEMON));
  entail_globals(wam, CTagToArg(susps,FD_LIST_MAX_GLOBAL));
  entail_indexicals(wam, CTagToArg(susps,FD_LIST_MINMAX_IX));
  entail_daemons(wam, CTagToArg(susps,FD_LIST_MINMAX_DAEMON));
  entail_globals(wam, CTagToArg(susps,FD_LIST_MINMAX_GLOBAL));
  // entail_implications(wam, CTagToArg(susps,FD_LIST_VAL_IMP0)); /* assume that the other var will be entailed anyway */
  // entail_implications(wam, CTagToArg(susps,FD_LIST_VAL_IMP1)); /* assume that the other var will be entailed anyway */
  entail_disequations(wam, CTagToArg(susps,FD_LIST_VAL_DISEQ));
  entail_watchers(wam, CTagToArg(susps,FD_LIST_VAL_WATCHER));
  entail_indexicals(wam, CTagToArg(susps,FD_LIST_VAL_IX));
  entail_daemons(wam, CTagToArg(susps,FD_LIST_VAL_DAEMON));
  entail_globals(wam, CTagToArg(susps,FD_LIST_VAL_GLOBAL));
  n = fd_list_length(aliases);
  FdMemRequireHeap2(2*n, var, attr, 0);
  AttrToAliasM(attr, mutable);
  aliases = RefMutable(mutable);
  h = w->global_top;
  tail = &newaliases;
  while (TagIsLST(aliases)) {
    TAGGED pair = CTagToCar(aliases);
    TAGGED other = CTagToArg(pair,2);
    aliases = CTagToCdr(aliases);
    if (other == attr) {
      break;
    } else {
      *tail = MakeList(h);
      h[0] = pair;
      tail = h+1;
      h += 2;
    }
  }
  *tail = aliases;
  w->global_top = h;
  fd_update_mutable(wam, newaliases, mutable);
  aliases = newaliases;
  while (TagIsLST(aliases)) {
    TAGGED pair = CTagToCar(aliases);
    TAGGED other = CTagToArg(pair,2);
    aliases = CTagToCdr(aliases);
    AttrToAliasM(other, mutable);
    fd_update_mutable(wam, newaliases, mutable);
  }
  delete_attributes(var, fd.fd_module, FALSE, 0);
}


TAGGED
fd_daemon_copy_state(Wam wam, TAGGED *global, SP_BOOL *buried)
{
  TAGGED *s, *h, tstate;
  int ar, i;
  
  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  s = TagToArg(tstate,0);
  if (s >= w->global_uncond) {
    *buried = FALSE;
    s[ar] += IStep(1);		/* increment stamp */
  } else {
    DECL_UPDATE_MUTABLE;
    *buried = TRUE;
    FdMemRequireHeap2(ar+1,*global,tstate,EVAL_ARITY);
    s = TagToArg(tstate,0);
    h = w->global_top;
    for (i=ar; i>=0; i--)
      h[i] = s[i];
    w->global_top = h+ar+1;
    h[ar] += IStep(1);		/* increment stamp */
    tstate = MakeStructure(h);
    FD_UPDATE_MUTABLE(tstate,CTagToArg(*global,1));
  }
  return tstate;
}

/* Most propagators have arguments (+State0, -State), ... where
   State0 = F(......,Handle,Stamp), is left dereferenced,
   State  = copy of State0 with Stamp incremented.
   Also, check if this execution step can be backtracked over or not.
*/
TAGGED fd_unify_output_state(Wam wam,
			     TAGGED *phandle,
			     SP_integer *pstamp,
			     SP_BOOL *pcommitted)
{
  TAGGED handle, t1, *s, *h;
  int ar, i;
  
  DerefNonvar(X(0));
  ar = Arity(TagToHeadfunctor(X(0)));
  DerefArg(handle,X(0),ar-1);
  DerefArg(t1,X(0),ar);
  *pstamp = GetSmall(t1);
  *pcommitted = (IsVar(handle) ? TRUE : TagToLST(handle) >= w->global_uncond);
  s = TagToArg(X(0),0);
  if (s >= w->global_uncond) {
    s[ar] += IStep(1);		/* increment stamp */
  } else {
    FdMemRequireHeap1(ar+1,handle,EVAL_ARITY);
    s = TagToArg(X(0),0);
    h = w->global_top;
    for (i=0; i<ar+1; i++)
      h[i] = s[i];
    w->global_top = h+ar+1;
    h[ar] += IStep(1);		/* increment stamp */
    X(0) = MakeStructure(h);
  }
  *phandle = handle;
  return X(0);
}

/* Some propagators have arguments (+State0, -State), ... where
   State0 = F(......,Handle), is left dereferenced,
   State  = State0,
   Also, check if this execution step can be backtracked over or not.
*/
TAGGED fd_static_output_state(Wam wam,
			      TAGGED *phandle,
			      SP_BOOL *pcommitted)
{
  TAGGED handle;
  int ar;
  
  DerefNonvar(X(0));
  ar = Arity(TagToHeadfunctor(X(0)));
  DerefArg(handle,X(0),ar-1);
  *phandle = handle;
  *pcommitted = (IsVar(handle) ? TRUE : TagToLST(handle) >= w->global_uncond);
  return X(0);
}

/* length(+List,-Length) */
int 
fd_list_length(TAGGED tvec)
{
  int nvars=0;
  
  while (TagIsLST(tvec)) {
    DerefCdr(tvec,tvec);
    nvars++;
  }
  return nvars;
}

/* store var & attr in global term refs */
void
fd_get_var_and_attr(TAGGED term, SP_globref ref)
{
  TAGGED t1;
  
  DerefArg(t1,term,1);	/* get domain var */
  RefGlob(ref+1) = t1;
  DerefArg(t1,term,2);	/* get attribute */
  RefGlob(ref) = t1;
}

/* access singleton value also in the context of extra attributes */
/* term is known to be dvar or integer */
TAGGED
fd_deref(Wam wam, TAGGED term)
{
  TAGGED attr, domain;
  DerefSwitch(term, {
      attr = get_attributes(term,fd.fd_module);
      DomFromAttr(domain,attr);
      if (DomainSize(domain)==TaggedOne)
	term = DomainMin(domain);
  });
  return term;
}


/* for qsorting by ascending SP_integer */
static int cmp_asc_long(Wam wam, SP_integer *l1, SP_integer *l2)
{
  SP_integer val1 = *l1;
  SP_integer val2 = *l2;

  (void)wam;
  return CMP(val1,val2);
}

#define QType SP_integer
#define QCmp  cmp_asc_long
#define QSort fd_qsort_asc_long1
#include "qsort.ic"

void
fd_qsort_asc_long(Wam wam, SP_integer *l1, int n)
{
  fd_qsort_asc_long1(wam, l1,n);
}



/* for qsorting by ascending TAGGED */
static int cmp_asc_tagged(Wam wam, TAGGED *t1, TAGGED *t2)
{
  (void)wam;
  return TCMP(*t1,*t2);
}

#define QType TAGGED
#define QCmp  cmp_asc_tagged
#define QSort fd_qsort_asc_tagged1
#include "qsort.ic"

void
fd_qsort_asc_tagged(Wam wam, TAGGED *l1, int n)
{
  fd_qsort_asc_tagged1(wam, l1,n);
}

void *
fd_malloc(Wam wam,
                 size_t size)
{
  void *p;
  
  /* [PM] 4.0 FIXME: update all callers to check the return value from Malloc/SP_malloc, then remove the lazy-check */
  LAZY_NULL_CHECK(p = SP_malloc(size));
  return p;
}

void *
fd_realloc(Wam wam,
		  void *p,
		  size_t size)
{
  void *tmp;
  LAZY_NULL_CHECK(tmp = SP_realloc(p, size));
  return tmp;
}

/* Confine local memory management to the numstack. */
void *
fd_temp_alloc(Wam wam, size_t nchars)
{
  void *p;
  int nlongs = (int)(((nchars-1)>>LogSizeOfWord)+1);
  
  NumstackAlloc(nlongs,p);
  return p;
}

/* Confine local memory management to a reused block. */
void *
fd_perm_alloc2(Wam wam,
		 size_t nchars,
		 struct size_mem *size_mem)
{
  if (size_mem->size==0) {
    size_mem->mem = SP_malloc(nchars);
  } else if (size_mem->size < nchars) {
    size_mem->size = nchars;
    size_mem->mem  = SP_realloc(size_mem->mem,nchars);
  }
  return size_mem->mem;
}

void
fd_perm_free2(Wam wam,
		void *ptr,
		struct size_mem *size_mem)
{
  if (size_mem->size==0)
    SP_free(ptr);
}

struct fd_state *
fd_state(Wam wam)
{
  return &fd;
}

void
fd_not_fixpoint(Wam wam)
{
  TAGGED mutable = CTagToArg(X(1),3); /* X(1) holds global */
  TAGGED flags = RefMutable(mutable);
  
  flags &= ~IStep(9);	       /* STATUS: not current, not enqueued */
  fd_update_mutable(wam, flags, mutable);
}

