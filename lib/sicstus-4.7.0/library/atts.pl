/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : atts.pl
%   Author     : Christian Holzbaur
%   Updated    : 3 September 1999
%   Purpose    : Expansions and library for programs using 
%              : attributed variables.

% This file has to be loaded at compile time in order to define
% expansions. It is also needed at runtime for expansion of dynamic calls to
% {get,put}_atts/2.

:- if(current_prolog_flag(dialect, spider)).

% Attach the package documentation to the module declaration, for SPIDER etc.
%@  @cindex attributed variables
%@  @cindex variables, attributed
%@  This package implements attributed variables.  It provides a means
%@  of associating with variables arbitrary attributes, i.e.@: named
%@  properties that can be used as storage locations as well as to extend
%@  the default unification algorithm when such variables are
%@  unified with other terms or with each other.  This facility
%@  was primarily designed as a clean interface between Prolog and
%@  constraint solvers, but has a number of other uses as well.  The basic
%@  idea is due to Christian Holzbaur and he was actively involved in the
%@  final design.  For background material, see the dissertation [Holzbaur 90].
%@  
%@  The package provides a means to declare and access named attributes of
%@  variables.  The attributes are compound terms whose
%@  arguments are the actual attribute values.  The attribute names
%@  are @emph{private} to the module in which they are defined.  They
%@  are defined with a declaration
%@  
%@  @cindex attribute declaration
%@  @cindex declaration, attribute
%@  @PLXindex {attribute/1 (declaration)}
%@  
%@  @example
%@  @group
%@  :- attribute @var{AttributeSpec}, @dots{}, @var{AttributeSpec}.
%@  @end group
%@  @end example
%@  
%@  @noindent
%@  where each @var{AttributeSpec} has the form (@var{Name}/@var{Arity}).
%@  There must be at most one such declaration in a module
%@  @var{Module}.
%@  At most 255 modules can declare attributes at the same time.
%@  
%@  Having declared some attribute names, these attributes can now be added,
%@  updated and deleted from unbound variables.  For each
%@  declared attribute name, any variable can have at most one such
%@  attribute (initially it has none).
%@  
%@  The declaration causes the following two access predicates
%@  to become defined by means of the @code{goal_expansion/5}
%@  mechanism.  They take a variable and an @var{AccessSpec} as
%@  arguments where an @var{AccessSpec} is either
%@  @code{+(@var{Attribute})}, @code{-(@var{Attribute})}, or a list of
%@  such.  The @samp{+} prefix may be dropped for convenience.  
%@  @var{Attribute} must be nonvariable at compile time.

%% [PM] 4.1.3 attribute/1 is not exported, instead it is special-cased by a term expansion. For SPIDER we instead fake an exported definition.

:- module(attributes, [put_atts/2, get_atts/2,
                       %% Add missing export for SPIDER
                       (attribute)/1]).

:- else.
:- module(attributes, [put_atts/2, get_atts/2]).
:- endif.

:- use_module(library(types), [
	illarg/4,
	must_be/4
        ]).

:- op(1150, fx, [attribute]).

%@  The meaning of the @samp{+}/@samp{-} prefix is documented below:
%@  


%@  @table @code
%@  @PLXindex {get_atts/2 (Module)}
%@  @item @var{Module}:get_atts(@var{-Var}, @var{?AccessSpec})
%@  Gets the attributes of @var{Var}, which must be a variable,
%@  according to @var{AccessSpec}.  If
%@  @var{AccessSpec} was unbound at compile time, it will be bound to a
%@  list of all present attributes of @var{Var}, otherwise
%@  the elements of @var{AccessSpec} have the following meaning:
%@  @table @code
%@  @item +(@var{Attribute})
%@  The corresponding actual attribute must be present and is unified with
%@  @var{Attribute}.
%@  @c Make the predicate documentation self-contained. SPRM-20484.
%@  The @samp{+} prefix may be dropped for convenience.
%@  
%@  @item -(@var{Attribute})
%@  The corresponding actual attribute must be absent.  The arguments
%@  of @var{Attribute} are ignored, only the name and arity are
%@  relevant.
%@  @end table
%@  
:- get_atts/2 is det.


%@  @PLXindex {put_atts/2 (Module)}
%@  @item @var{Module}:put_atts(@var{-Var}, @var{+AccessSpec})
%@  Sets the attributes of @var{Var}, which must be a variable, according to @var{AccessSpec}.
%@  The effects of @code{put_atts/2} are undone on backtracking.
%@  @table @code
%@  @item +(@var{Attribute})
%@  The corresponding actual attribute is set to @var{Attribute}.
%@  If the actual attribute was already present, it is simply replaced.
%@  @c Make the predicate documentation self-contained. SPRM-20484.
%@  The @samp{+} prefix may be dropped for convenience.
%@  
%@  @item -(@var{Attribute})
%@  The corresponding actual attribute is removed.
%@  If the actual attribute was already absent, nothing happens.
%@  @end table
%@  @end table
:- put_atts/2 is det.

%@  
%@  A module that contains an attribute declaration has an
%@  opportunity to extend the default unification algorithm by
%@  defining the following predicate:
%@  
%@  @table @code


%@  @PLXindex {verify_attributes/3 (Module)}
%@  @item @var{Module}:verify_attributes(@var{-Var}, @var{+Value}, @var{-Goals}) @hook{}
%@  This predicate is called whenever a variable @var{Var} that
%@  might have attributes in @var{Module} is about to be bound to
%@  @var{Value} (it might have none).  The unification resumes after
%@  the call to @code{verify_attributes/3}.  @var{Value} is a
%@  nonvariable, or another attributed variable.
%@  @var{Var} might have no attributes present in @var{Module}; the
%@  unification extension mechanism is not sophisticated enough to
%@  filter out exactly the variables that are relevant for
%@  @var{Module}.
%@  
%@  @code{verify_attributes/3} is called @emph{before} @var{Var} has
%@  actually been bound to @var{Value}.  If it fails, the
%@  unification is deemed to have failed.  It may succeed
%@  nondeterminately, in which case the unification might
%@  backtrack to give another answer.  It is expected to return, in
%@  @var{Goals}, a list of goals to be called @emph{after}
%@  @var{Var} has been bound to @var{Value}.  Finally, after calling
%@  @var{Goals},
%@  goals blocked on @var{Var} may have become unblocked, in which case they are called.
%@  
%@  @code{verify_attributes/3} may invoke arbitrary Prolog goals, but
%@  @var{Var} should @emph{not} be bound by it.  Binding
%@  @var{Var} will result in undefined behavior.
%@  
%@  If @var{Value} is a nonvariable,
%@  @code{verify_attributes/3} will typically inspect the attributes of
%@  @var{Var} and check that they are compatible with @var{Value} and fail
%@  otherwise.  If @var{Value} is another attributed variable,
%@  @code{verify_attributes/3} will typically copy the attributes of
%@  @var{Var} over to @var{Value}, or merge them with @var{Value}'s, in
%@  preparation for @var{Var} to be bound to @var{Value}.  In either
%@  case, @code{verify_attributes/3} may determine @var{Var}'s current
%@  attributes by calling @code{get_atts(@var{Var},@var{List})} with an
%@  unbound @var{List}.
%@  
%@  In the case when a single unification binds multiple attributed
%@  variables, first all such bindings are @emph{undone}, then 
%@  the following actions are carried out for each relevant variable:
%@  
%@  @enumerate
%@  @item
%@  For each relevant module @var{M}, @code{@var{M}:verify_attributes/3}
%@  is called, collecting a list of returned @var{Goals}.
%@  @item
%@  The variable binding is @emph{redone}.
%@  @item
%@  Any @var{Goals} are called.
%@  @item
%@  Any goals blocked on the variable, that has now become unblocked, are called.
%@  @end enumerate
%@  
%@  @end table
:- Module:verify_attributes/3 is hook.


%@  
%@  An important use for attributed variables is in implementing
%@  coroutining facilities as an alternative or complement to the
%@  built-in coroutining mechanisms.  In this context it might be
%@  useful to be able to interpret some of the attributes of a
%@  variable as a goal that is blocked on that
%@  variable.  Certain built-in predicates (@code{frozen/2},
%@  @code{copy_term/3}) and the Prolog top level need to access
%@  blocked goals, and so need a means of getting the goal
%@  interpretation of attributed variables by calling:
%@  
%@  @table @code


%@  @PLXindex {attribute_goal/2 (Module)}
%@  @item @var{Module}:attribute_goal(@var{-Var}, @var{-Goal}) @hook{}
%@  This predicate is called in each module that contains an
%@  attribute declaration, when an interpretation of the attributes as
%@  a goal is needed, in particular in @code{frozen/2},
%@  @code{copy_term/3} and the Prolog top level.
%@  It should unify @var{Goal} with the
%@  interpretation, or merely fail if no such interpretation is available.
%@  @end table
:- Module:attribute_goal/2 is hook.

%@  
%@  An important use for attributed variables is to provide an
%@  interface to constraint solvers.  An important function for a constraint
%@  solver in the constraint logic programming paradigm is to be able to
%@  perform projection of the residual constraints onto the variables
%@  that occurred in the top-level query.  A module that
%@  contains an attribute declaration has an opportunity to perform
%@  such projection of its residual constraints by defining the following
%@  predicate:
%@  


%@  @table @code
%@  @PLXindex {project_attributes/2 (Module)}
%@  @item @var{Module}:project_attributes(@var{+QueryVars}, @var{+AttrVars}) @hook{}
%@  This predicate is called by the Prolog top level in each module that
%@  contains an attribute declaration.  @var{QueryVars} is the
%@  list of variables occurring in the query, or in
%@  terms bound to such variables, and @var{AttrVars} is a
%@  list of possibly attributed variables created during the
%@  execution of the query.  The two lists of variables
%@  may or may not be disjoint.
:- Module:project_attributes/2 is hook.


%@  
%@  If the attributes on @var{AttrVars} can be interpreted as constraints,
%@  this predicate will typically ``project'' those constraints onto
%@  the relevant @var{QueryVars}.  Ideally, the residual constraints will be
%@  expressed entirely in terms of the @var{QueryVars}, treating all
%@  other variables as existentially quantified.  Operationally,
%@  @code{project_attributes/2} must remove all attributes from
%@  @var{AttrVars}, and add transformed attributes representing the
%@  projected constraints to some of the @var{QueryVars}.
%@  
%@  Projection has the following effect on the Prolog top level.  When the
%@  top-level query has succeeded, @code{project_attributes/2} is
%@  called first.  The top level then prints the answer substitution and
%@  residual constraints.  While doing so, it searches for attributed
%@  variables created during the execution of the query.  For
%@  each such variable, it calls @code{attribute_goal/2} to get a
%@  printable representation of the constraint encoded by the attribute.
%@  Thus, @code{project_attributes/2} is a mechanism for controlling how the
%@  residual constraints should be displayed at top level.
%@  
%@  The exact definition of @code{project_attributes/2} is constraint system
%@  dependent, but @pxref{Answer Constraints} and @pxref{CLPQR Projection}
%@  for details about projection in CLPFD and CLP(Q,R) respectively.
%@  @end table
%@  
%@  In the following example we sketch the implementation of a finite domain
%@  ``solver''.  Note that an industrial strength solver would have to
%@  provide a wider range of functionality and that it quite likely would
%@  utilize a more efficient representation for the domains proper.  The
%@  module exports a single predicate
%@  @code{domain(@var{-Var},@var{?Domain})}, which associates @var{Domain}
%@  (a list of terms) with @var{Var}.  A variable can be
%@  queried for its domain by leaving @var{Domain} unbound.
%@  
%@  We do not present here a 
%@  definition for @code{project_attributes/2}.  Projecting finite domain
%@  constraints happens to be difficult.
%@  
%@  
%@  @example
%@  @group
%@  @flushright
%@  @emph{% domain.pl}
%@  @end flushright
%@  :- module(domain, [domain/2]).
%@  
%@  :- use_module(library(atts)).
%@  :- use_module(library(ordsets), [
%@          ord_intersection/3,
%@          ord_intersect/2,
%@          list_to_ord_set/2
%@     ]).
%@  
%@  :- attribute dom/1.
%@  
%@  verify_attributes(Var, Other, Goals) :-
%@          get_atts(Var, dom(Da)), !,          % are we involved?
%@          (   var(Other) ->                   % must be attributed then
%@              (   get_atts(Other, dom(Db)) -> %   has a domain?
%@                  ord_intersection(Da, Db, Dc),
%@                  Dc = [El|Els],              % at least one element
%@                  (   Els = [] ->             % exactly one element
%@                      Goals = [Other=El]      % implied binding
%@                  ;   Goals = [],
%@                      put_atts(Other, dom(Dc))% rescue intersection
%@                  )
%@              ;   Goals = [],
%@                  put_atts(Other, dom(Da))    % rescue the domain
%@              )
%@          ;   Goals = [],
%@              ord_intersect([Other], Da)      % value in domain?
%@          ).
%@  verify_attributes(_, _, []).                % unification triggered
%@                                              % because of attributes
%@                                              % in other modules
%@  
%@  attribute_goal(Var, domain(Var,Dom)) :-     % interpretation as goal
%@          get_atts(Var, dom(Dom)).
%@  
%@  domain(X, Dom) :-
%@          var(Dom), !,
%@          get_atts(X, dom(Dom)).
%@  domain(X, List) :-
%@          list_to_ord_set(List, Set),
%@          Set = [El|Els],                     % at least one element
%@          (   Els = [] ->                     % exactly one element
%@              X = El                          % implied binding
%@          ;   put_atts(Fresh, dom(Set)),
%@              X = Fresh                       % may call
%@                                              % verify_attributes/3
%@          ).
%@  @end group
%@  @end example
%@  
%@  Note that the ``implied binding'' @code{Other=El} was deferred until
%@  after the completion of @code{verify_attribute/3}.  Otherwise, there
%@  might be a danger of recursively invoke @code{verify_attribute/3}, which
%@  might bind @code{Var}, which is not allowed inside the scope of
%@  @code{verify_attribute/3}.  Deferring unifications into the third
%@  argument of @code{verify_attribute/3} effectively serializes the
%@  calls to @code{verify_attribute/3}.
%@  
%@  Assuming that the code resides in the file @file{domain.pl}, we can
%@  load it via:
%@  
%@  @example
%@  @group
%@  | ?- @kbd{use_module(domain).}
%@  @end group
%@  @end example
%@  
%@  Let's test it:
%@  
%@  @example
%@  @group
%@  | ?- @kbd{domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]).}
%@  
%@  domain(X,[1,5,6,7]),
%@  domain(Y,[3,4,5,6]),
%@  domain(Z,[1,6,7,8])
%@  
%@  | ?- @kbd{domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]), }
%@       @kbd{X=Y.}
%@  
%@  Y = X,
%@  domain(X,[5,6]),
%@  domain(Z,[1,6,7,8])
%@  
%@  | ?- @kbd{domain(X,[5,6,7,1]), domain(Y,[3,4,5,6]), domain(Z,[1,6,7,8]),}
%@       @kbd{X=Y, Y=Z.}
%@  
%@  X = 6,
%@  Y = 6,
%@  Z = 6
%@  @end group
%@  @end example
%@  
%@  To demonstrate the use of the @var{Goals} argument of
%@  @code{verify_attributes/3}, we give an implementation of
%@  @code{freeze/2}.  We have to name it @code{myfreeze/2} in order to avoid
%@  a name clash with the built-in predicate of the same name.
%@  
%@  @example
%@  @group
%@  @flushright
%@  @emph{% myfreeze.pl}
%@  @end flushright

%@ :- module(myfreeze, [myfreeze/2]).
%@ 
%@ :- use_module(library(atts)).
%@ 
%@ :- meta_predicate myfreeze(*, 0).
%@ 
%@ :- attribute frozen/1.
%@ 
%@ verify_attributes(Var, Other, Goals) :-
%@         get_atts(Var, frozen(Fa)), !,            % are we involved?
%@         (   var(Other) ->                        % must be attributed then
%@             (   get_atts(Other, frozen(Fb))      % has a pending goal?
%@             ->
%@                 put_atts(Other, frozen((Fa,Fb))) % rescue conjunction
%@             ;   put_atts(Other, frozen(Fa))      % rescue the pending goal
%@             ),
%@             Goals = []
%@         ;   Goals = [Fa]                         % wake our frozen goal
%@         ).
%@ verify_attributes(_, _, []).
%@ 
%@ attribute_goal(Var, myfreeze(Var,Goal)) :- % interpretation as goal
%@         get_atts(Var, frozen(Goal)).
%@ 
%@ myfreeze(X, Goal) :-
%@         put_atts(Fresh, frozen(Goal)),
%@         Fresh = X.

%@  @end group
%@  @end example
%@  
%@  Assuming that this code lives in file @file{myfreeze.pl},
%@  we would use it via:
%@  
%@  @example
%@  @group
%@  | ?- @kbd{use_module(myfreeze).}
%@  | ?- @kbd{myfreeze(X,print(bound(x,X))), X=2.}
%@  
%@  bound(x,2)                      % side-effect
%@  X = 2                           % bindings
%@  @end group
%@  @end example
%@  
%@  The two solvers even work together:
%@  
%@  @example
%@  @group
%@  | ?- @kbd{myfreeze(X,print(bound(x,X))), domain(X,[1,2,3]),}
%@       @kbd{domain(Y,[2,10]), X=Y.}
%@  
%@  bound(x,2)                      % side-effect
%@  X = 2,                          % bindings
%@  Y = 2
%@  @end group
%@  @end example
%@  
%@  The two example solvers interact via bindings to shared attributed
%@  variables only.  More complicated interactions are likely to be
%@  found in more sophisticated solvers.  The corresponding
%@  @code{verify_attributes/3} predicates would typically refer to the
%@  attributes from other known solvers/modules via the module
%@  prefix in @code{@var{Module}:get_atts/2}.
:- if(current_prolog_flag(dialect, spider)).

:- (attribute)/1 is nondet.     % Avoid warning from SPIDER about missing determinacy declaration.


:- meta_predicate(attribute(:)).
% See the source for documentation.
attribute(_FA).                 % Dummy definition for SPIDER. (The strange comment above makes sense in SPIDER)
:- endif.
% -----------------------------------------------------------------------

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

:- multifile
	user:term_expansion/6.

user:term_expansion((:- attribute(Decls)), Lay0, Ids, Exp, Lay, [atts|Ids]) :-
	nonmember(atts, Ids), !,
	attribute_exp(Decls, Exp, []),
	condense_layout(Lay0, Lay).


goal_expansion(get_atts(Var,Spec), _Lay0, Module, Exp, Lay) :- !,
	Lay = [],                               % no layout
	expand_get_atts(Var, Spec, Module, Exp).
goal_expansion(put_atts(Var,Spec), _Lay0, Module, Exp, Lay) :- !,
	Lay = [],
	expand_put_atts(Var, Spec, Module, Exp).

% -----------------------------------------------------------------------

/*
   The attributes are representeted by a vector where the attributes are "flattened" and the first
   element of the vector is a bitmap that tells which attributes are present.

   So, in a module with three attributes

   :- attribute foo(A), bar(B,C), baz

   an attributed variable V has an associated vector v(Mask, A, B,C) where V has the attribute
   foo(A) if (Mask /\ 1<<0 =\= 0), it has the attribute bar(B,C) if (Mask /\ 1<<1 =\= 0),
   and it has the attribute baz if (Mask /\ 1<<2 =\= 0).

   '$get_attributes'/3 knows about this representation and returns a suitable Mask, either the
   first element of the attribute vector, or 0 (zero) if the variable has no attribute vector.

   Vector size overflow is not handled gracefully, currently (4.6.0).

   Information about the attributes in a module are represented in two generated predicates.
   The predicates are generated in the module that declares the attributes.

   One predicate for mapping from attribute to vector positions:

   '$t2v'(foo(A),     v(_M,  A, _B,_C), 1). % 1 is 1<<0
   '$t2v'(bar(B,C),   v(_M, _A,  B, C), 2). % 2 is 1<<1
   '$t2v'(baz,        v(_M, _A, _B,_C), 4). % 4 is 1<<2

   this predicate also has a clause for the bit-vector (always the first argument of the vector):

   '$t2v'('$bitv'(M), v( M, _A, _B,_C), 0). % it does not have a bit in the bitmask.

   There is also a predicate generated for mapping from vector to all attributes:

   '$v2l'(v(_M, A, B,C), [foo(A), bar(B,C), baz]). % '$bitv'(M) is not present in the list

   These predicates are used when goal-expanding put_atts/2 and get_atts/2 so they need to
   be present both at compile time and, to handle things like call(get_atts(...)), at runtime.
 */


attribute_exp(Decls) -->
	{ prolog_load_context(module, Module) },
	[(:-prolog:'$save_attribute_info'(Module))],
	[(:-use_module(attributes,library(atts),[put_atts/2,get_atts/2]))],
	{   conj_to_list_functor(Decls, Attrs, 1, Size),
	    functor(Vector, v, Size)
	},
	compute_att_mask(Attrs, Vector).

conj_to_list_functor((N/A,B), [H|Rest], I, K) :- !,
	functor(H, N, A),
	J is I+A,
	conj_to_list_functor(B, Rest, J, K).
conj_to_list_functor(N/A, [H], I, K) :- 
	functor(H, N, A),
	K is I+A.

% -----------------------------------------------------------------------
% Preprocessing of the set of attributes producing data used by
% the subsequent inline expansion of get/put_attr goals
% compute_att_mask(+Atts, +Vector)//
compute_att_mask(Atts, Vector) -->
	{ arg(1, Vector, Mask) },
	['$t2v'('$bitv'(Mask),Vector,0)],
	compute_att_mask(Atts, Vector, 1, 1),
	['$v2l'(Vector,Atts)].

% Generate '$t2v'/3 facts
compute_att_mask([], _, _, _) --> [].
compute_att_mask([Att|Atts], Vector, I, M) -->
	['$t2v'(Att,Vector,M)],
	{   functor(Att, _, A),
	    J is I+A,
	    equate_args(A, Att, J, Vector),
	    N is M<<1
	},
	compute_att_mask(Atts, Vector, J, N).

equate_args(0, _, _, _) :- !.
equate_args(A, T1, C, T2) :-
	arg(A, T1, X),
	arg(C, T2, X),
	B is A-1, D is C-1,
	equate_args(B, T1, D, T2).



% -----------------------------------------------------------------------
% Goal expansion of {get,put}_atts/2

expand_get_atts(Var, Spec, Module, Exp) :-
	nonvar(Spec), !,
	partition_spec(Spec, SpecP, [], SpecN, []),
	sanity(SpecP, get_atts(Var,Spec), Module),
	sanity(SpecN, get_atts(Var,Spec), Module),
	l2v(SpecP, Module, Vector, SpecPM),
	l2v(SpecN, Module, Vector, SpecNM),
	all_mask(Module, SpecAll),
	expand_get_atts(SpecPM, SpecNM, SpecAll, get_atts(Var,Spec), Module,
	                Vector, Exp0),
	Exp = Module:Exp0.
expand_get_atts(Var, Spec, Module, Exp) :- % var(Spec), !, % Get all attributes
	Exp = Module:Exp0,
	expand_get_atts_all(Var, Spec, Exp0).


expand_get_atts_all(Var, Spec, Exp0) :-
	Exp0 = (
		   '$get_attributes'(Var,Vector,Mask),
		   (var(Vector)->
		    Spec=[]
		   ;'$v2l'(Vector,Atts),
		    % Explicit module, so atts_subset/3 does not need to be exported/imported.
		    attributes:atts_subset(Atts,Mask,Spec)
		   )
	       ).


expand_get_atts(P, N, _, Goal, Module, _, _) :-
	P/\N =\= 0, !,                      % Pos and Neg the same attribute
	expand_exception(Goal, Module).
expand_get_atts(0, 0, _, Goal, _, _, Body) :- !, % Ignore all
	arg(1, Goal, Var),
	Body = (
		   '$get_attributes'(Var,_,_)
	       ).
expand_get_atts(0, All, All, Goal, _, _, Body) :- !, % Require all to be absent
	arg(1, Goal, Var),
	Body = (
		   '$get_attributes'(Var,_,0)
	       ).
expand_get_atts(0, N, _, Goal, _, _, Body) :- !, % Require some to be absent, ignore all present.
	arg(1, Goal, Var),
	Body = (
		   '$get_attributes'(Var,_,Mask),
		   Mask/\N=:=0
	       ).
expand_get_atts(P, N, All, Goal, _, Vector, Body) :- P\/N =:= All, !, % Require exact match of absents and presents
	arg(1, Goal, Var),
	Body = (
		   '$get_attributes'(Var,Vector0,Mask),
		   Mask=P,
		   Vector0=Vector
	       ).
expand_get_atts(P, N, _, Goal, _, Vector, Body) :- % Require some absent, some present.
	arg(1, Goal, Var),
	PN is P\/N,
	Body = (
		   '$get_attributes'(Var,Vector0,Mask),
		   Mask/\PN=:=P,
		   Vector0=Vector
	       ).



expand_put_atts(Var, Spec, Module, Module:Exp) :-
	Goal = put_atts(Var,Spec),
	must_be(Spec, nonvar, Goal, 2),
	partition_spec(Spec, SpecP, SpecN),
	sanity(SpecP, Goal, Module),
	sanity(SpecN, Goal, Module),
	expand_put_atts_1(Module, SpecP, SpecN, VOld, VNew, PM, NM),
	all_mask(Module, SpecAll),
	expand_put_atts(PM, NM, SpecAll, Goal, Module, VOld, VNew, Exp).


% expand_put_atts(+PosMask, +NegMask, +AllMask, +Goal, +Module, +VOld, +VNew, -Expansion).
expand_put_atts(P, N, _, Goal, Module, _, _, _) :-
	P/\N =\= 0,             % Add and delete the same attribute
	!,
	expand_exception(Goal, Module).
expand_put_atts(P, N, _, Goal, _, _, _, Body) :- P = 0, N = 0, !, % Add none, delete none.
	arg(1, Goal, Var),
	Body = (
		   % For argument validation only
		   '$get_attributes'(Var,_,_)
	       ).
expand_put_atts(P, N, All, Goal, _, _, _, Body) :- P = 0, N = All, !, % Add none, delete all
	arg(1, Goal, Var),
	Body = (
		   '$delete_attributes'(Var)
	       ).
expand_put_atts(P, N, _, Goal, _, VOld, VNew, Body) :- P = 0, !, % Add none, delete some
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	NN is \(N),
	Body = (
		   '$get_attributes'(Var,V,OldMask),
		   NewMask is OldMask/\NN,                       % Clear the deleted attribute bits, and set the VNew mask
		   NewMask=\=OldMask->                           % Only if anything changed
		   V=VOld,                                       % Propagate unchanged positions from V to VNew
		   '$put_attributes'(Var,VNew)
	       ;true
	       ).
expand_put_atts(P, N, All, Goal, _, _, VNew, Body) :- P\/N =:= All, !, % Replace all
	arg(1, Goal, Var),
	NewMask = P,                                                   % The new mask is just the set (P) attribute bits
	arg(1, VNew, NewMask),
	Body = (
		   '$put_attributes'(Var,VNew)
	       ).
expand_put_atts(P, N, _, Goal, _, VOld, VNew, Body) :- N = 0, !, % Add some, delete none.
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	Body = (
		   '$get_attributes'(Var,V,OldMask),
		   NewMask is OldMask\/P,
		   V=VOld,
		   '$put_attributes'(Var,VNew)
	       ).
expand_put_atts(P, N, _, Goal, _, VOld, VNew, Body) :- % Add some, delete some, keep some.
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	NN is \(N),
	Body = (
		   '$get_attributes'(Var,V,OldMask),
		   NewMask is(OldMask\/P)/\NN,
		   V=VOld,
		   '$put_attributes'(Var,VNew)
	       ).

% partition_spec(+AccessSpec, -Positive, -Negative)
% For all +(Attribute) in AccessSpec add Attribute to Positive
% For all -(Attribute) in AccessSpec add Attribute to Negative
% A variable Attribute will be put in the appropriate list for later steps to barf at.
partition_spec(Spec, SpecP, SpecN) :-
	partition_spec(Spec, SpecP, [], SpecN, []).


partition_spec(X, _, _, _, _) :- var(X), !, fail.
partition_spec(+(X), [X|P], P, N, N) :- !.
partition_spec(-(X), P, P, [Y|N], N) :-
	nonvar(X), !,
	functor(X, F, A),
	functor(Y, F, A).
partition_spec(-(X), P, P, [X|N], N) :- !.
partition_spec([], P0, P0, N0, N0) :- !.
partition_spec([S|Ss], P2, P0, N2, N0) :- !,
	partition_spec(S, P1, P0, N1, N0),
	partition_spec(Ss, P2, P1, N2, N1).
partition_spec(X, [X|P], P, N, N).

:- expand_exception/2 is throwing.
expand_exception(Goal, Module) :-
	arg(2, Goal, Spec),
	partition_spec(Spec, Pos, [], Neg, []),
	member(Culprit1, Pos),
	functor(Culprit1, N, A),
	functor(Culprit2, N, A),
	member(Culprit2, Neg),
	illarg(consistency(+Culprit1,-Culprit2,clash), Module:Goal, 2, Spec).

sanity(Ss, Goal, Module) :-
	(   foreach(S,Ss),
	    param([Goal,Module])
	do  (   nonvar(S),
		is_t2v(Module, '$t2v'(S,_,_)) -> true
	    ;   Module:'$v2l'(_,All),
		illarg(domain(term,one_of(All)), Module:Goal, 2, S)
	    )
	).

l2v(L, Module, Vector, Spec) :-
	(   foreach(T, L),
	    fromto(0, M0, M1, Spec),
	    param([Module,Vector])
	do
	    (  Module:'$t2v'(T, Vector, Mask) ->
	       M1 is M0\/Mask
	    )
	).

all_mask(Module, Mask) :-
	findall(Bits, Module:'$t2v'(_,_,Bits), All),
	(   foreach(Bits1,All),
	    fromto(0, Mask0, Mask1, Mask)
	do  Mask1 is Mask0\/Bits1
	).

% All the '$t2v'(T,Vec,Mask) facts for attributes
all_atts(Module, All) :-
	findall(T2V, is_t2v(Module,T2V), All).

% Can be used both for generating and for recognizing attributes.
% Determinate if T is nonvar.
:- is_t2v/2 is nondet.
is_t2v(Module, '$t2v'(T,Vect,Mask)) :-
	Module:'$t2v'(T, Vect, Mask),
	Mask =\= 0.             % not the bit-vector slot

% put_exp(+Module, +SpecPos, +SpecNeg, -VOld, -VNew, -PosMask, -NegMask).
% PosMask gets a bit for each attributes in SpecPos
% NegMask gets a bit for each attributes in SpecNeg
% VOld and VNew are v(...) vectors that share variables in the positions that are _not_ modified
% by the put_atts/2 call. That is, the put_attr(Var, Spec) should do something like:
% '$get_attributes'(Var, VOld, _), % get the old vector, binding unchanged attribute positions in VNew
% VNew = v(NewMask, ...New...),    % Bind the changed positions from Spec
% '$put_attributes'(Var, VNew)     % store the new vector, with any unchanged positions inherited from VOld.
%
expand_put_atts_1(Module, SpecP, SpecN, VOld, VNew, PM, NM) :-
	all_atts(Module, All),
	put_exp(All, SpecP, SpecN, VOld, VNew, 0, PM, 0, NM).


put_exp([], _, _, _, _, PM, PM, NM, NM).
put_exp(['$t2v'(T,Vec,Mask)|Ts], SpecP, SpecN, VOld, VNew, PM0, PM, NM0, NM) :-
	(   memberchk(T, SpecP) ->
	    PM1 is PM0\/Mask
	;   PM1 = PM0
	),
	(   memberchk(T, SpecN) ->
	    NM1 is NM0\/Mask
	;   NM1 = NM0
	),
	(   (PM1\/NM1)/\Mask =:= Mask -> % The T is attribute added or removed, i.e. its VOld slots should be discarded
	    true                         % do not propagate the attribute variables from VOld to VNew
	;   copy_term(T-Vec, T-VOld)     % The T attribute is kept, its variables will be shared between Vec,VNew,VOld
	),
	Vec = VNew,                      % VNew will get the T attribute variables from T
	put_exp(Ts, SpecP, SpecN, VOld, VNew, PM1, PM, NM1, NM).

% Called by expanded code.
:- public atts_subset/3.
:- atts_subset/3 is det.
atts_subset(Atts, Mask, Present) :-
	(   foreach(Att,Atts),
	    fromto(Present,S0,S,[]),
	    fromto(Mask,Mask1,Mask2,_)
	do  (Mask1/\1 =:= 1 -> S0 = [Att|S] ; S0 = S),
	    Mask2 is Mask1>>1
	).
