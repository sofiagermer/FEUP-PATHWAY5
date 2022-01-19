/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

/* '$fd_unify(+X, +Y, -RC) */
SP_integer SPCDECL
prolog_fd_unify(Wam wam,
		SP_term_ref XR,
		SP_term_ref YR)
{
  int wc, refs, refs0;
  TAGGED *tr, *tw, *h=w->global_top;

  if (!cunify(w,RefTerm(XR),RefTerm(YR)))
    goto fail;
  tr = w->trail_top;
  wc = w->wake_count;
  while (wc>0) {
    TAGGED ref = *--tr;
    if (IsTrailedCVA(ref,h)) {
      wc--;
      if (!TagIsSmall(CTagToREF(ref)) || !pure_attributes(ref,fd.fd_module))
	return 0;	      /* use generic wakeup mechanism */	
    }
  }
  /* use accelerated CLFPD specific wakeup mechanism */
  refs0 = refs = SP_new_term_refs(2*w->wake_count); /* Foreign language Interface resets it */
  tw = tr;
  while (tr<w->trail_top) {
    TAGGED ref = *tr++;
    if (IsTrailedCVA(ref,h)) {
      TAGGED attr = get_attributes(ref,fd.fd_module);
      TAGGED value = CTagToREF(ref);
      
      RefTerm(refs++) = attr;
      RefTerm(refs++) = value;
      if (TagIsSTR(attr)) {
	if (!CondCVA(ref)) {
	  CTagToHeader(ref) = atom_nil; /* destruct into HVA and untrail  */
	} else {
	  *tw++ = ref;
	}
      } else {
	*tw++ = ref;
      }
    } else {
      *tw++ = ref;
    }
  }
  w->trail_top = tw;
  w->wake_count = 0;
  SP_MANGLE(prolog_fd_begin)(wam); /* calls fd_update_mutable(wam, ) -- must be AFTER the untrailing above */
  while (refs0<refs) {
    TAGGED attr = RefTerm(refs0++);
    TAGGED value = RefTerm(refs0++);
      
    if (TagIsSTR(attr)) {
      TAGGED var, domm, dom;
      AttrToDomM(attr, domm);
      dom = RefMutable(domm);
      if (!fd_member(value, DomainSet(dom))) {
	AttrAFC(attr) += IStep(1);
	goto fail;
      }

      AttrToVar(attr, var);
      X(EVAL_ARITY) = attr;
      X(EVAL_ARITY+1) = var;
      fd_tell_value(wam, value);
    }
  }
  return 1;
 fail:
  fd.failures++;
  SP_fail();
  return 0;
}

/******* Variable choice support. *******/

enum var_choice {
  min=0, max, ff, ffc, aff, occ, mreg, impact, dom_w_deg, leftmost
};

static SP_integer
var_degree(Wam wam, TAGGED attr) {
  TAGGED susp_mut, alias_mut, aliases, key1, key2;
  SP_integer degree = 0;
  (void)wam;

  AttrToVar(attr,key1);
  DEREF(key1,key1);
  AttrToAliasM(attr,alias_mut);
  aliases = RefMutable(alias_mut);
  while (TagIsLST(aliases)) {
    TAGGED off_attr = CTagToCar(aliases);
    attr = CTagToArg(off_attr,2);
    aliases = CTagToCdr(aliases);
    AttrToVar(attr,key2);
    DEREF(key2,key2);
    if (key1!=key2) {		/* an equation */
      degree++;
    } else {			/* a unified dvar */
      AttrToSuspM(attr,susp_mut);
      degree += GetSmall(CTagToArg(RefMutable(susp_mut),1));
    }
  }
  return degree;
}


static void
rank_var(Wam wam,
	 TAGGED var,
	 enum var_choice option,
	 SP_integer *key1, SP_integer *key2)
{
  TAGGED attr = fd_check_argument(wam, var,Inf,Sup,Sup,TRUE);
  TAGGED dom4;

  DomFromAttr(dom4,attr); /* get dom/4 term */

  switch (option) {
  case min:
  case leftmost:
    *key1 = GetSmall(DomainMin(dom4));
    break;
  case max:			/* maximize */
    *key1 = -GetSmall(DomainMax(dom4));
    break;
  case ffc:			/* maximize occurrence */
    *key2 = -var_degree(wam,attr);
    /* FALLTHROUGH */
  case ff:
    *key1 = DomainSizeAsInt(dom4);
    break;
  case aff:			/* maximize */
    *key1 = -DomainSizeAsInt(dom4);
    break;
  case occ:			/* maximize */
    *key1 = -var_degree(wam,attr);
    break;
  case mreg:			/* maximize */
    {
      TAGGED first = DomainMin(dom4);
      TAGGED set = DomainSet(dom4);
      TAGGED head = CTagToCar(set);
      if (RangeMax(head)!=first) { /* |first interval|>1 */
	*key1 = -1;
      } else {
	set = CTagToCdr(set);
	head = CTagToCar(set);
	*key1 = GetSmall(first) - GetSmall(RangeMin(head));
      }
    }
    break;
  case impact:			/* maximize */
    *key1 = -GetSmall(AttrAFC(attr));
    break;
  case dom_w_deg:		/* maximize AFC/domsize */
    {
      double fkey = ((double)GetSmall(AttrAFC(attr)) + 1) / (double)DomainSizeAsInt(dom4);
      *key1 = -(SP_integer)fkey;
      *key2 = -(SP_integer)((fkey + (double)*key1) * 1000000.0);
      break;
    }
  }
}

/* '$fd_delete'(+List, -Variable, +Option) */
/* Option in [min,max,ff,ffc,anti_first_fail,occurrence,max_regret,impact,dom_w_deg] */
void SPCDECL
prolog_fd_delete(Wam wam,
			SP_term_ref ListR,
			SP_term_ref VarR,
			SP_atom aoption)
{
  enum var_choice option;
  SP_integer bestk1=0, bestk2=0;
  SP_integer currk1=0, currk2=0;
  TAGGED bestvar, list, var;
  int i;
 
  for (i=0;; i++)
    if (fd.var_options[i]==aoption)
      break;
  option = i;
  list = RefTerm(ListR);	/* known to be list */
  DerefNonvar(list);
  DerefCar(var,list);		/* known to be var */
  DerefCdr(list,list);
  rank_var(wam, var, option, &bestk1, &bestk2);
  bestvar = var;
  while (TagIsLST(list) && !(option==ff && bestk1==2)) {
    DerefCar(var,list);
    DerefCdr(list,list);
    if (!IsVar(var))
      continue;
    rank_var(wam, var, option, &currk1, &currk2);
    if (currk1<bestk1 || (currk1==bestk1 && currk2<bestk2)) {
      bestk1 = currk1;
      bestk2 = currk2;
      bestvar = var;
    }
  }
  RefTerm(VarR) = bestvar;  
}

/******* Labeling support. *******/

#define X_Var      X(0)
#define X_Domain   X(1)
#define X_Vars     X(2)
#define X_Depth    X(3)
#define X_Encoding X(4)
#define X_Extra    X(5)
#define X_Param    X(6)
#define ARITY 7

#define Encoding_leftmost   0x000001
#define Encoding_min        0x000002
#define Encoding_max        0x000004
#define Encoding_ff         0x000008
#define Encoding_ffc        0x000010
#define Encoding_anti_ff    0x000020
#define Encoding_occurrence 0x000040
#define Encoding_max_regret 0x000080
#define Encoding_impact     0x000100
#define Encoding_dom_w_deg  0x000200
#define Encoding_enum       0x001000
#define Encoding_step       0x002000
#define Encoding_bisect     0x004000
#define Encoding_up         0x010000
#define Encoding_down       0x020000
#define Encoding_median     0x040000
#define Encoding_middle     0x080000
#define Encoding_minimize   0x100000
#define Encoding_maximize   0x200000

static TAGGED fd_get_pivot(TAGGED domain, int encoding) {
  if (encoding & Encoding_up)
    return fd_min(domain);
  else if (encoding & Encoding_down)
    return fd_max(domain);
  else if (encoding & Encoding_middle)
    return MakeSmall0(fd_middle(domain));
  else
    return MakeSmall0(fd_median(domain));
}

/* actually a copy of push_choicepoint() */
static void fd_push_choicepoint(Wam wam, struct try_node *alt) {
  int n = alt->node_offset;
  TAGGED *b0 = (TAGGED *)w->node;
  struct node *b = (struct node *)((char *)b0-n);

  w->node = b;
  b->trail_top = w->trail_top;
  b->global_top = w->global_top;
  b->next_alt = alt;
  b->frame = w->frame;
  b->next_insn = w->next_insn;
  b->local_top = w->local_top;
  n = OffsetToArity(n);
  X(ARITY) = atom_nil;		/* [MC] aligned choicepoints */
  while (n>0)
    *--b0 = X(--n);
  ChoiceCheck;
  NewShadowregs(w->global_top);
  SP_ASSERT(w->global_top <= w->heap_warn);
}

void fd_labeling(Wam wam, void *altptr) {
  int encoding;
  struct dvar dvar;
  SP_term_ref saveref = SP_new_term_refs(ARITY);
  enum var_choice var_choice = leftmost;
  int i;
  TAGGED rc, global, vars, depth, first_domain, local_domain;
  SP_integer rcmsg = 0;
  
  if (X_Domain == atom_nil) {	/* not on backtracking */
    DEREF(X_Vars,X_Vars);
    DEREF(X_Depth,X_Depth);
    DEREF(X_Encoding,X_Encoding);
    DEREF(X_Extra,X_Extra);
    DEREF(X_Param,X_Param);
  }
  encoding = GetSmall_int(X_Encoding);
  var_choice = (encoding & Encoding_min) ? min :
    (encoding & Encoding_max) ? max :
    (encoding & Encoding_ff) ? ff :
    (encoding & Encoding_ffc) ? ffc :
    (encoding & Encoding_anti_ff) ? aff :
    (encoding & Encoding_occurrence) ? occ :
    (encoding & Encoding_max_regret) ? mreg :
    (encoding & Encoding_impact) ? impact :
    (encoding & Encoding_dom_w_deg) ? dom_w_deg :
    leftmost;
  if (X_Domain != atom_nil) {	/* on backtracking */
    if (!fd_singleton(X_Domain) && (encoding & Encoding_enum)) {
      TAGGED pivot = fd_get_pivot(X_Domain, encoding);
      TAGGED *h;
      
      FdMemInit;
      X_Domain = fd_globalize(wam, fd_localize(wam, fd_delete(wam, X_Domain, pivot)), 4, ARITY);
      fd_push_choicepoint(wam, altptr);
      h = w->global_top;
      X_Domain = MakeList(h);
      h[0] = MakeList(h+2);
      h[1] = atom_nil;
      h[2] = pivot;
      h[3] = pivot;
      w->global_top = h+4;
    }
    for (i=0; i<ARITY; i++)
      RefTerm(saveref+i) = X(i);
    FdMemInit;
    SP_MANGLE(prolog_fd_begin)(wam);
    dvar_init_ix(&dvar, get_attributes(X_Var, fd.fd_module), X_Var);
    switch (dvar_fix_set(&dvar, fd_localize(wam, X_Domain))) {
    case -1:
      fd.failures++;
      rcmsg = -1;
      break;
    case 0:
      break;
    default:
      dvar_export_ix(wam, &dvar, FALSE); /* GC */
    }
    if (rcmsg==0 && (encoding & (Encoding_minimize|Encoding_maximize))) {
      TAGGED objective;
      TAGGED clauseref;
      TAGGED incumbent, lb, ub;
    
      X_Extra = RefTerm(saveref+5);
      DerefArg(objective, X_Extra, 1);
      DerefArg(clauseref, X_Extra, 2);
      /* Avoid clang warning about accessing code[1] that does not
	 exist in struct. (Also, [MC] says clauseref arg is known
	 deref here). */
      incumbent = (&(((struct instance *)TermToPointer(CTagToArg(clauseref,1)))->code[0]))[1];
      lb = (encoding & Encoding_minimize) ? Inf : incumbent+IStep(1);
      ub = (encoding & Encoding_maximize) ? Sup : incumbent-IStep(1);
      if (TagIsSmall(objective)) {
	if (!InInterval(objective,lb,ub)) {
	  fd.failures++;
	  rcmsg = -1;
	}
      } else {
	dvar_init_ix(&dvar, get_attributes(objective, fd.fd_module), objective);
	switch (dvar_fix_interval_t(&dvar, lb, ub)) {
	case -1:
	  fd.failures++;
	  rcmsg = -1;
	  break;
	case 0:
	  break;
	default:
	  dvar_export_ix(wam, &dvar, FALSE); /* GC */
	}
      }
    }
    if (rcmsg==0)
      rcmsg = prolog_fd_evaluate_indexical(wam, saveref+1);  
    for (i=0; i<ARITY; i++)
      X(i) = RefTerm(saveref+i);
    w->fli_stack_index = saveref+ARITY; /* SP_reset_term_refs(saveref+ARITY); */
  }

  /* backtracking or no backtracking, the main loop */
  
  while (rcmsg==0 && TagIsLST(X_Vars) && w->heap_warn_soft) {
    TAGGED head, tail;
    SP_integer bestk1=0, bestk2=0;
    SP_integer currk1=0, currk2=0;

    DerefCar(head, X_Vars);
    DerefCdr(tail, X_Vars);
    if (!IsVar(head)) {
      X_Vars = tail;
    } else {
      X_Var = head;
      if (var_choice != leftmost) {
	rank_var(wam, head, var_choice, &bestk1, &bestk2);
	while (TagIsLST(tail) && !(var_choice==ff && bestk1==2)) {
	  DerefCar(head,tail);
	  DerefCdr(tail,tail);
	  if (!IsVar(head))
	    continue;
	  rank_var(wam, head, var_choice, &currk1, &currk2);
	  if (currk1<bestk1 || (currk1==bestk1 && currk2<bestk2)) {
	    bestk1 = currk1;
	    bestk2 = currk2;
	    X_Var = head;
	  }
	}
      }
      FdMemInit;
      X_Depth += IStep(1);
      X_Domain = get_attributes(X_Var, fd.fd_module);
      AttrToDomM(X_Domain,X_Domain);
      local_domain = fd_localize(wam, DomainSet(RefMutable(X_Domain)));
      SP_ASSERT(!fd_singleton(local_domain));
      if (!(encoding & Encoding_bisect)) {
	TAGGED pivot = fd_get_pivot(local_domain, encoding);

	first_domain = fd_interval(wam, pivot, pivot);
      } else {
	SP_integer isum = GetSmall0(fd_min(local_domain)) + GetSmall0(fd_max(local_domain));
	SP_integer ipivot = isum%2 ? (isum-1)/2 : isum/2;
	TAGGED pivot = MakeSmall0(ipivot);
	TAGGED lb, ub;

	lb = (encoding & Encoding_up) ? Inf : pivot+IStep(1);
	ub = (encoding & Encoding_down) ? Sup : pivot;
	first_domain = fd_intersection_interval(wam, local_domain, lb, ub);
      }
      X_Domain = fd_globalize(wam, fd_subtract(wam, local_domain, first_domain), 0, ARITY);
      fd_push_choicepoint(wam, altptr);
      for (i=0; i<ARITY; i++)
	RefTerm(saveref+i) = X(i);
      FdMemInit;
      SP_MANGLE(prolog_fd_begin)(wam);
      dvar_init_ix(&dvar, get_attributes(X_Var, fd.fd_module), X_Var);
      switch (dvar_fix_set(&dvar, first_domain)) {
      case -1:
	fd.failures++;
	rcmsg = -1;
	break;
      case 0:
	break;
      default:
	dvar_export_ix(wam, &dvar, FALSE); /* GC */
      }
      if (rcmsg==0)
	rcmsg = prolog_fd_evaluate_indexical(wam, saveref+1);
      for (i=0; i<ARITY; i++)
	X(i) = RefTerm(saveref+i);
      w->fli_stack_index = saveref+ARITY; /* SP_reset_term_refs(saveref+ARITY); */
    }
  }
  DerefArg(rc, X_Param, 1);
  DerefArg(global, X_Param, 2);
  DerefArg(vars, X_Param, 3);
  DerefArg(depth, X_Param, 4);
  BindHVA(rc, MakeSmall(rcmsg));
  BindHVA(global, X_Domain);
  BindHVA(vars, X_Vars);
  BindHVA(depth, X_Depth);
  SP_reset_term_refs(saveref);
}
