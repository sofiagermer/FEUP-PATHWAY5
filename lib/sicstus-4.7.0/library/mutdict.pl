:- module(mutdict, [
	is_mutdict/1,
	list_to_mutdict/2,
	new_mutdict/1,
	mutdict_empty/1,
	mutdict_size/2,
	mutdict_gen/3,
	mutdict_get/3,
	mutdict_put/3,
	mutdict_update/4,
	mutdict_delete/2,
	mutdict_clear/1,
	mutdict_keys/2,
	mutdict_values/2,
	mutdict_items/2,
	mutdict_append/2
   ]).

%@  Prolog does not have a dictionary type. Instead, one can use,
%@  e.g., AVL trees, with logarithmic access time.  This package
%@  provides operations on mutable dictionaries.  Such a
%@  @var{mutdict} is an unordered collection of items that consist of
%@  a key and a value.  The only restrictions on items is that the
%@  key must be ground.  The given @var{mutdict} can hold at most one
%@  item for any given key.  It uses a hash table representation,
%@  which admits table lookup in constant time, if there are no hash
%@  collisions.  Update operations also take constant time, except
%@  under some circumstances when the entire table must be copied, in
%@  which case such operations take linear time.  
%@  In addition, the hash function takes time linear in the size of the key,
%@  and so simple keys are more efficient than compound terms.
%@  A @var{mutdict} is a @var{mutable} (@pxref{ref-lte-mut}), the
%@  value of which is accessed and modified by the operations of this package.
%@  Following are the exported predicates:
%@  
%@  @table @code
%@  @item is_mutdict(@var{+Dict})
%@  @PLXindex {is_mutdict/1 (mutdict)}
%@  succeeds when @var{Dict} is a @var{mutdict}.
:- is_mutdict/1 is semidet.
is_mutdict(MutDict) :-
	mutdict_size(MutDict, _).

%@  @item list_to_mutdict(@var{+Items}, @var{-Dict})
%@  @PLXindex {list_to_mutdict/2 (mutdict)}
%@  unifies @var{Dict} with a new @var{mutdict} populated by @var{Items},
%@  each of which should be of the form @code{@var{Key}-@var{Value}}.
:- list_to_mutdict/2 is det.
list_to_mutdict(Items, MutDict) :-
	prolog:'$array1d_dict_new'(Array),
	create_mutable(Array, MutDict),
	mutdict_append(MutDict, Items).

%@  @item new_mutdict(@var{-Dict})
%@  @PLXindex {new_mutdict/1 (mutdict)}
%@  unifies @var{Dict} with a new @var{mutdict}.
:- new_mutdict/1 is det.
new_mutdict(MutDict) :-
	prolog:'$array1d_dict_new'(Array),
	create_mutable(Array, MutDict).

%@  @item mutdict_empty(@var{+Dict})
%@  @PLXindex {mutdict_empty/1 (mutdict)}
%@  succeeds when @var{Dict} is an empty @var{mutdict}.
:- mutdict_empty/1 is semidet.
mutdict_empty(MutDict) :-
	mutdict_size(MutDict, 0).

%@  @item mutdict_size(@var{+Dict}, @var{-Size})
%@  @PLXindex {mutdict_size/1 (mutdict)}
%@  unifies @var{Size} with the number of elements of the @var{mutdict} @var{Dict}.
:- mutdict_size/1 is semidet.
mutdict_size(MutDict, Size) :-
	nonvar(MutDict),
	mutable(MutDict),
	get_mutable(Array, MutDict),	
	functor(Array, '$array1d', 2),
	arg(2, Array, List),
	List = ['$dict', _, Size|_].

%@  @item mutdict_gen(@var{+Dict}, @var{-Key}, @var{-Value})
%@  @PLXindex {mutdict_gen/3 (mutdict)}
%@  succeeds when @var{Dict} is a @var{mutdict} where one item consists of @var{Key} and @var{Value}.
%@  Iterates over all items, but in no specific order.
:- mutdict_gen/3 is nondet.
mutdict_gen(MutDict, Key, Value) :-
	mutdict_items(MutDict, Items),
	member(Key-Value, Items).

%@  @item mutdict_get(@var{+Dict}, @var{+Key}, @var{-Value})
%@  @PLXindex {mutdict_get/3 (mutdict)}
%@  If there is an item of the @var{mutdict} with key @var{Key}, then its value is unified with @var{Value}.
%@  Otherwise, the goal fails.
:- mutdict_get/3 is semidet.
mutdict_get(MutDict, Key, Value) :-
	get_mutable(Array, MutDict),
	ground(Key),
	prolog:'$array1d_dict_get'(Array, Key, Output),
	Output = Value.

%@  @item mutdict_put(@var{+Dict}, @var{+Key}, @var{+Value})
%@  @PLXindex {mutdict_put/3 (mutdict)}
%@  If there is an item of the @var{mutdict} with key @var{Key}, then its value is simply replaced by @var{Value};
%@  otherwise, adds the item consisting of @var{Key} and @var{Value} to the given @var{mutdict}.
:- mutdict_put/3 is det.
mutdict_put(MutDict, Key, Value) :-
	get_mutable(Array, MutDict),
	ground(Key),
	prolog:'$array1d_dict_put'(Array, Key, Value, NewArray),
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutDict)
	).

%@  @item mutdict_update(@var{+Dict}, @var{+Key}, @var{-OldValue}, @var{+NewValue})
%@  @PLXindex {mutdict_update/4 (mutdict)}
%@  If there is an item of the @var{mutdict} with key @var{Key}, then its value is unified with @var{OldValue}
%@  and replaced by @var{NewValue}.  Otherwise, the goal fails.
:- mutdict_update/4 is semidet.
mutdict_update(MutDict, Key, OldValue, NewValue) :-
	get_mutable(Array, MutDict),
	ground(Key),
	prolog:'$array1d_dict_get'(Array, Key, Output), % FIXME: merge C code
	prolog:'$array1d_dict_put'(Array, Key, NewValue, NewArray),
	OldValue = Output,
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutDict)
	).

%@  @item mutdict_delete(@var{+Dict}, @var{+Key})
%@  @PLXindex {mutdict_delete/2 (mutdict)}
%@  If there is an item of the @var{mutdict} with key @var{Key}, then it is deleted;
%@  otherwise, the goal merely succeeds.
:- mutdict_delete/2 is det.
mutdict_delete(MutDict, Key) :-
	get_mutable(Array, MutDict),
	ground(Key),
	prolog:'$array1d_dict_delete'(Array, Key, NewArray),
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutDict)
	).

%@  @item mutdict_clear(@var{+Dict})
%@  @PLXindex {mutdict_clear/1 (mutdict)}
%@  All items are deleted from the @var{mutdict}.
:- mutdict_clear/1 is det.
mutdict_clear(MutDict) :-
	get_mutable(Array, MutDict),
	prolog:'$array1d_dict_clear'(Array, NewArray),
	(   NewArray = [] -> true
	;   update_mutable(NewArray, MutDict)
	).

%@  @item mutdict_keys(@var{+Dict}, @var{-Keys})
%@  @PLXindex {mutdict_keys/2 (mutdict)}
%@  @var{Keys} is unified with the list of keys of @var{mutdict}.
%@  The list comes in no specific order, and is free of duplicates.
:- mutdict_keys/2 is semidet.
mutdict_keys(MutDict, Keys) :-
	get_mutable(Array, MutDict),
	prolog:'$array1d_dict_items'(Array, Items),
	(   foreach(Key-_,Items),
	    foreach(Key,Keys)
	do  true
	).

%@  @item mutdict_values(@var{+Dict}, @var{-Values})
%@  @PLXindex {mutdict_values/2 (mutdict)}
%@  @var{Values} is unified with the list of values of @var{mutdict}.
%@  The list comes in no specific order, and can contain duplicates.
:- mutdict_values/2 is semidet.
mutdict_values(MutDict, Values) :-
	get_mutable(Array, MutDict),
	prolog:'$array1d_dict_items'(Array, Items),
	(   foreach(_-Value,Items),
	    foreach(Value,Values)
	do  true
	).

%@  @item mutdict_items(@var{+Dict}, @var{-Items})
%@  @PLXindex {mutdict_items/2 (mutdict)}
%@  @var{Items} is unified with the list of items of @var{mutdict}.
%@  Each item is of the form @code{@var{Key}-@var{Value}}.
%@  The list comes in no specific order, and is free of duplicates.
:- mutdict_items/2 is semidet.
mutdict_items(MutDict, Items) :-
	get_mutable(Array, MutDict),
	prolog:'$array1d_dict_items'(Array, Output),
	Items = Output.

%@  @item mutdict_append(@var{+Dict}, @var{+DictOrItems})
%@  @PLXindex {mutdict_append/2 (mutdict)}
%@  where @var{DictOrItems} should be a @var{mutdict} or a list of items.
%@  For each item of @var{DictOrItems} of the form @code{@var{Key}-@var{Value}},
%@  if there is an item of the @var{mutdict} with key @var{Key}, then its value is simply replaced by @var{Value};
%@  otherwise, the item is added to the given @var{mutdict}.
:- mutdict_append/2 is det.
mutdict_append(MutDict, Dict) :-
	is_mutdict(Dict), !,
	mutdict_items(Dict, Items), % FIXME: room for optimization
	mutdict_append(MutDict, Items).
mutdict_append(MutDict, Items) :-
	get_mutable(Array1, MutDict),
	(   foreach(Key-Value,Items),
	    fromto(Array1,Array2,Array3,Array4),
	    param(Flag)
	do  ground(Key),
	    prolog:'$array1d_dict_put'(Array2, Key, Value, NewArray),
	    (   NewArray = [] -> Array3 = Array2
	    ;   Array3 = NewArray, Flag = true
	    )
	),
	(   var(Flag) -> true
	;   update_mutable(Array4, MutDict)
	).
	
				
%@  @end table
