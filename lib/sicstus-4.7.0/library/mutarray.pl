:- module(mutarray, [
	is_mutarray/1,
	list_to_mutarray/2,
	new_mutarray/1,
	new_mutarray/2,
	mutarray_length/2,
	mutarray_set_length/2,
	mutarray_to_list/2,
	mutarray_last/2,
	mutarray_gen/3,
	mutarray_get/3,
	mutarray_put/3,
	mutarray_update/4,
	mutarray_append/2
   ]).

%@  Prolog does not have an array type. Instead, one must use lists
%@  or trees, with logarithmic access time to the elements at best.
%@  This package provides operations on dense, mutable arrays,
%@  of elements indexed from 1 and up.  It uses an array
%@  representation, which admits direct access to the elements and
%@  constant-time access to any element.  Update operations also take
%@  constant time, except under some circumstances when the entire
%@  array must be copied, in which case such operations take linear
%@  time.  Following are the exported predicates, where a
%@  @var{mutarray} is a @var{mutable} (@pxref{ref-lte-mut}), the
%@  value of which is accessed and modified by the operations of this package.
%@  
%@  @table @code
%@  @item is_mutarray(@var{+Array})
%@  @PLXindex {is_mutarray/1 (mutarray)}
%@  succeeds when @var{Array} is a @var{mutarray}.
:- is_mutarray/1 is semidet.
is_mutarray(MutArray) :-
	nonvar(MutArray),
	mutable(MutArray),
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2).

%@  @item list_to_mutarray(@var{+Items}, @var{-Array})
%@  @PLXindex {list_to_mutarray/2 (mutarray)}
%@  where @var{Items} should be a list of values.
%@  @var{Array} is unified with a new @var{mutarray} populated by the given values.
:- list_to_mutarray/2 is det.
list_to_mutarray(Items, MutArray) :-
	length(Items, N),
	prolog:'$array1d_new'(N, Array),
	arg(2, Array, Items),
	create_mutable(Array, MutArray).

%@  @item new_mutarray(@var{-Array})
%@  @PLXindex {new_mutarray/1 (mutarray)}
%@  unifies @var{Array} with a new @var{mutarray} of zero length.
:- new_mutarray/1 is det.
new_mutarray(MutArray) :-
	prolog:'$array1d_new'(0, Array),
	create_mutable(Array, MutArray).

%@  @item new_mutarray(@var{-Array}, @var{+Size})
%@  @PLXindex {new_mutarray/2 (mutarray)}
%@  where @var{Size} should be a nonnegative integer.
%@  @var{Array} is unified with a new @var{mutarray} of the given size populated by brand new variables.
:- new_mutarray/2 is det.
new_mutarray(MutArray, Items) :-
	integer(Items),
	prolog:'$array1d_new'(Items, Array),
	create_mutable(Array, MutArray).

%@  @item mutarray_length(@var{+MutArray}, @var{-Length})
%@  @PLXindex {mutarray_length/2 (mutarray)}
%@  @var{Length} is unified in constant time with the length of @var{MutArray}.
:- mutarray_length/2 is semidet.
mutarray_length(MutArray, Len) :-
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2),
	arg(1, Array, Len).

%@  @item mutarray_set_length(@var{+MutArray}, @var{+Length})
%@  @PLXindex {mutarray_set_length/2 (mutarray)}
%@  sets the new length of @var{mutarray} @var{MutArray} to @var{Length}.
%@  If the @var{MutArray} becomes shorter, this runs in constant time in the best case.
%@  Otherwise, new brand new variables are added to the end of the array in linear time.
:- mutarray_set_length/2 is det.
mutarray_set_length(MutArray, Len) :-
	get_mutable(Array, MutArray),	
	prolog:'$array1d_set_length'(Len, Array, NewArray),
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutArray)
	).

%@  @item mutarray_last(@var{+MutArray}, @var{-Last})
%@  @PLXindex {mutarray_last/2 (mutarray)}
%@  @var{Last} is unified in constant time with the last element of @var{MutArray}.
%@  Merely fails if @var{MutArray} is of zero length.
:- mutarray_last/2 is semidet.
mutarray_last(MutArray, Last) :-
	get_mutable(Array, MutArray),
	functor(Array, '$array1d', 2),
	arg(1, Array, Len),
	prolog:'$array1d_nth'(Array, Len, Output),
	Last = Output.

%@  @item mutarray_to_list(@var{+MutArray}, @var{-List})
%@  @PLXindex {mutarray_to_list/2 (mutarray)}
%@  @var{List} is unified with the list of elements of @var{MutArray}.
:- mutarray_to_list/2 is semidet.
mutarray_to_list(MutArray, List) :-
	nonvar(MutArray), !,
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2),
	arg(2, Array, Elts),
	(   foreach(X,Elts),	% build a separate list to protect from destructive updates
	    foreach(X,List)
	do  true
	).

%@  @item mutarray_gen(@var{+MutArray}, @var{-N}, @var{-Element})
%@  @PLXindex {mutarray_gen/3 (mutarray)}
%@  is true when @var{Element} is the @var{N}:th element of the given @var{MutArray}.
%@  Iterates over all elements by increasing @var{N}.
:- mutarray_gen/3 is nondet.
mutarray_gen(MutArray, Index, Element) :-
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2),
	arg(1, Array, N),
	between1(1, N, Index),
	prolog:'$array1d_nth'(Array, Index, Output),
	Element = Output.

%@  @item mutarray_get(@var{+MutArray}, @var{+N}, @var{-Element})
%@  @PLXindex {mutarray_get/3 (mutarray)}
%@  unifies @var{Element} in constant time with the @var{N}:th element of @var{MutArray}.
:- mutarray_get/3 is semidet.
mutarray_get(MutArray, Index, Element) :-
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2),
	integer(Index),
	prolog:'$array1d_nth'(Array, Index, Output),
	Element = Output.

%@  @item mutarray_put(@var{+MutArray}, @var{+N}, @var{+Element})
%@  @PLXindex {mutarray_put/3 (mutarray)}
%@  sets the @var{N}:th element of @var{MutArray} to @var{Element}.
:- mutarray_put/3 is det.
mutarray_put(MutArray, Index, Element) :-
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2),
	prolog:'$array1d_set_nth'(Array, Index, Element, NewArray),
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutArray)
	).

%@  @item mutarray_update(@var{+MutArray}, @var{+N}, @var{-OldElement}, @var{+NewElement})
%@  @PLXindex {mutarray_update/4 (mutarray)}
%@  unifies @var{OldElement} with the @var{N}:th element of @var{MutArray}, and sets that element to @var{NewElement}.
:- mutarray_update/4 is semidet.
mutarray_update(MutArray, Index, OldElement, NewElement) :-
	get_mutable(Array, MutArray),	
	functor(Array, '$array1d', 2),
	prolog:'$array1d_nth'(Array, Index, Output), % FIXME: merge C code
	prolog:'$array1d_set_nth'(Array, Index, NewElement, NewArray),
	OldElement = Output,
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutArray)
	).

%@  @item mutarray_append(@var{+MutArr}, @var{+MutArrOrValues})
%@  @PLXindex {mutarray_append/2 (mutarray)}
%@  where @var{MutArrOrValues} should be a @var{mutarray} or a list of values,
%@  which is appended to the end of @var{MutArr}.
:- mutarray_append/2 is det.
mutarray_append(MutArray, Arr) :-
	is_mutarray(Arr), !,
	get_mutable(Array, Arr),	
	functor(Array, '$array1d', 2),
	arg(2, Array, Values),
	mutarray_append(MutArray, Values).
mutarray_append(MutArray, Values) :-
	mutarray_length(MutArray, I),
	length(Values, J),
	K is I + J,
	I1 is I+1,
	mutarray_set_length(MutArray, K),
	get_mutable(Array1, MutArray),
	(   foreach(Value,Values),
	    count(Ix,I1,_),
	    fromto(Array1,Array2,Array3,Array4),
	    param(Flag)
	do  prolog:'$array1d_set_nth'(Array2, Ix, Value, NewArray),
	    (   NewArray = [] -> Array3 = Array2
	    ;   Array3 = NewArray, Flag = true
	    )
	),
	(   var(Flag) -> true
	;   update_mutable(Array4, MutArray)
	).
	

:- between1/3 is nondet.
between1(U, U, U) :- !.
between1(L, _, L).
between1(L, U, N) :-
	M is L+1,
	between1(M, U, N).

%@  @end table
