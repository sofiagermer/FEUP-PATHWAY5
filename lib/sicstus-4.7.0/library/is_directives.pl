/* -*- Mode:Prolog; coding:utf-8; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

%@  This library module gives access to the information declared by
%@  @code{is/2} directives.

%@  The @code{is/2} declarations can be used for declaring arbitrary
%@  predicate attributes, but the main application is for declaring
%@  determinacy information, and this is what is described here.


%@  @subsection Introduction
%@
%@  Determinacy, i.e.@: whether a call to a predicate can produce more
%@  than one solution on backtracking, is an important property for
%@  understanding the predicate and the code that uses it. For this reason
%@  it is desirable to describe the behavior in the documentation that
%@  accompanies a predicate.
%@
%@  The determinacy properties of a predicate can also be used by
%@  various tools, and for this reason it is possible to declare the
%@  determinacy of a predicate in a way that can be automatically
%@  processed by such tools. One example is the determinacy analyzer
%@  in the SPIDER IDE (@pxref{SPIDER}), which uses the declared
%@  determinacy of a called predicate to improve the precision of the
%@  analysis of the caller in the common case that the caller does not
%@  have any determinacy declaration of its own.
%@
%@  The SICStus libraries contain determinacy declarations for most
%@  exported predicates. These can serve as useful examples.
%@
%@  Determinacy declarations are @emph{hints}, i.e.@: they are meant to
%@  convey the @emph{expected} behavior. Due to the dynamic nature of
%@  Prolog, almost any call can fail, throw an exception, or succeed more
%@  than once for reasons not directly related to the called
%@  predicate. Examples where these things can happen are timeouts
%@  (@code{library(timeout)}) and functionality that can cause goals to
%@  run when a variable is bound, e.g.@: @code{freeze/2}. For this reason,
%@  anything that uses determinacy declarations must be prepared to handle
%@  any run-time behavior, not just the behavior specified by the
%@  declarations.
%@
%@  @subsection Available Determinacy Annotations
%@
%@  The following determinacy attributes are available:
%@
%@  @table @code
%@  @item det
%@  A call will always succeed exactly once. I.e.@: the number of solutions
%@  is expected to be one. This is probably the most common behavior.
%@
%@  @item semidet
%@  A call will fail, or succeed exactly once. I.e.@: the number of
%@  solutions is expected to be zero or one. This can be used for for
%@  describing tests, e.g. @code{X>Y}.
%@
%@  @item multi
%@  A call will succeed more than once. I.e.@: the number of solutions is
%@  expected to be one or more. This is uncommon, but could be used to
%@  describe the builtin @code{repeat/0}.
%@
%@  @item nondet
%@  A call may fail or succeed any number of times. I.e.@: the number of
%@  solutions is expected to be zero or more. This is the most general
%@  determinacy information and is what must be assumed unless no other
%@  information is available.
%@
%@  @item failing
%@  A call will fail. I.e.@: the number of solutions is expected to be
%@  zero. This is uncommon, but could be used to describe the builtin
%@  @code{false/0}.
%@
%@  @item throwing
%@  A call is expected to throw an exception. This is similar to
%@  @code{failing} in that the number of solutions is expected to be zero
%@  but differs in situations where failure is treated specially, like
%@  if-then-else. This is uncommon, but could be used to describe the
%@  builtin @code{throw/1} and some of the error reporting predicates in
%@  @code{library(types)}.
%@
%@  @end table
%@
%@  @subsection Syntax of Determinacy Declarations
%@
%@  Determinacy is declared using directives that use @code{is/2}. Note
%@  that in this usage, @code{is/2} has nothing to do with the arithmetic
%@  predicate of the same name.
%@
%@  The format of a determinacy directive is:
%@  
%@  @example
%@  :- @var{SPEC} is @var{ANNOTATION}.
%@  @end example
%@  
%@  where @var{ANNOTATION} is one of the atoms used for
%@  determinacy annotation. The @var{SPEC} describes the predicate and can
%@  be either a @use{predicate specification} (like those used by the
%@  @code{abolish/1} predicate) or a @use{skeletal goal} (like those used
%@  by the @code{meta_predicate/1} directive).
%@
%@  Example:
%@
%@  @example
%@  @group
%@  % foo/2 is expected to always succeed, once, for any argument.
%@  :- foo/2 is det.
%@
%@  foo(X, Y) :-
%@          Y = hello(X).
%@
%@  % bar/2, when called with a non-variable first argument,
%@  % is expected to succeed at most once.
%@  :- bar(+, ?) is semidet.
%@
%@  % bar/2, when called with a variable first argument,
%@  % is expected to succeed any number of times.
%@  :- bar(-, ?) is nondet.
%@
%@  bar('a', lowercase).
%@  bar('A', uppercase).
%@  @dots{}
%@  bar('z', lowercase).
%@  bar('Z', uppercase).
%@  @end group
%@  @end example
%@
%@  The @var{SPEC} tells which predicate is being annotated and, if it is
%@  a @use{skeletal goal}, it can restrict the declaration to only apply
%@  for the specified instantiation of the arguments. It can have one of
%@  the following forms:
%@
%@  @table @code
%@  @item @var{Module}:@var{Name}/@var{Arity}
%@  @itemx @var{Name}/@var{Arity}
%@  @var{Module} and @var{Name} should be atoms, @var{Arity} should be a non-negative
%@  integer. This @use{predicate specification} denotes the predicate @var{Name} with arity
%@  @var{Arity} in module @var{Module}, where @var{Module} defaults to the source
%@  module.
%@
%@  @item @var{Module}:@var{Name}(ARG1, ARG2, @dots{}, ARGN)
%@  @itemx @var{Name}(ARG1, ARG2, @dots{}, ARGN)
%@
%@  This @use{skeletal goal} denotes the predicate @var{Name} with arity @var{N} in the
%@  module @var{Module}, where @var{Module} defaults to the source module. The
%@  argument positions is typically used to indicate an instantiation
%@  pattern.
%@
%@  @end table
%@  @c For now, do not document the DCG-style M:F//N and //(F(...)).
%@
%@  @subsubsection Specifying Instantiation Patterns
%@
%@  Many predicates have different determinacy depending on how their
%@  arguments are instantiated. This can be indicated using the skeleton goal
%@  form of specification, with each argument of the skeleton goal being one of
%@  the following:
%@
%@  @table @code
%@  @item +
%@  Indicates that the argument is instantiated, i.e.@: it is not a
%@  variable when the predicate is called.
%@  @item -
%@  Indicates that the argument is uninstantiated, i.e.@: it is a variable
%@  when the predicate is called.
%@  @item ?
%@  @itemx *
%@  Indicates that the argument can be anything, i.e.@: the declared
%@  determinism is not affected by the instantiation of this argument
%@  @end table
%@  @noindent if the argument is a compound term with one argument
%@  (like @code{+ hello}, or @code{- Bar}), only the functor is used when
%@  interpreting the argument. This makes it possible to use variables as
%@  descriptive names for the arguments, e.g.@:
%@  
%@  @example
%@  :- parent_of(+Parent, -Child) is nondet.
%@  @end example
%@  
%@  Not only variables can be used as descriptions in this way, any term
%@  is accepted.
%@
%@  When declaring determinism, the skeleton argument only specifies
%@  whether an argument is a variable or not. This is different from
%@  whether the argument should be considered input or output
%@  (@code{var(X)} has only an input argument, but will often be called
%@  with a variable as input argument).
%@
%@  @example
%@  @group
%@  @end group
%@  @end example
%@
%@
%@  @subsubsection Declaring Meta Predicate Determinacy
%@
%@  The declarations above is sufficient for most predicates. However,
%@  they do not suffice for predicates whose determinism depends on an
%@  argument goal, like @code{lists:maplist/2}.
%@
%@  For such predicates, i.e.@: meta predicates that take a single
%@  @use{closure} (goal) argument, it is possible to specify different
%@  determinacy for each of the possible determinacies of the closure
%@  argument, as in the following example:
%@
%@  @example
%@  @group
%@  % The last argument of dolist/3 is a closure with two
%@  % suppressed arguments that will be supplied using call/3.
%@  :- meta_predicate dolist(*, *, 2).
%@
%@  % This is the expected mode, a determinate producer. In this
%@  % case dolist/3 will also succeed exactly once.
%@  :- dolist(+, -, det) is det.
%@
%@  % If the closure can fail, then dolist/2 can also fail.
%@  :- dolist(+, -, semidet) is semidet.
%@
%@  % If the closure succeeds more than once, then so will dolist/2.
%@  :- dolist(+, -, multi) is multi.
%@
%@  % If the closure always fails (a strange usage, indeed) then dolist/3
%@  % can nevertheless succeed, when the input is an empty list.
%@  :- dolist(+, -, failing) is semidet.
%@
%@  % In general, dolist/3 will be nondeterminate if the closure is
%@  % nondeterminate.
%@  % This declaration may seem redundant, but it may not be for some tools.
%@  :- dolist(+, -, nondet) is nondet.
%@
%@  % dolist/3 calls the argument closure, which expects two extra
%@  % arguments, on each pair of corresponding list elements
%@  dolist([], [], _G_2).           % The closure is ignored here
%@  dolist([X|Xs], [Y|Ys], G_2) :-
%@          call(G_2, X, Y),
%@          dolist(Xs, Ys, G_2).
%@
%@  :- square/2 is det. % always expected to succeed (once)
%@  square(X, XX) :-
%@          XX is X*X.
%@
%@  %  Example use:
%@  %  | ?- square_list([1,2,3], Squares).
%@  %  Squares = [1,4,9] ?
%@  %
%@  % It can be inferred that this is expected to succeed exactly once.
%@  square_list(Numbers, Squares) :-
%@          % Calls square(X,Y) on each X in Numbers and Y in Squares.
%@          % Since square/2 is 'det', the call do dolist/3 will
%@          % be considered 'det' as well.
%@          dolist(Numbers, Squares, square).
%@  @end group
%@  @end example
%@
%@  @subsection Using Determinacy Declarations
%@
%@  Since determinacy declarations by necessity are only @emph{hints}, it
%@  is often better to focus on the expected behavior rather than the exact
%@  behavior when declaring determinism for a predicate.
%@
%@  Also, only very simple instantiation patterns can be specified, so it
%@  may be useful to pretend they indicate more than they actually do.
%@
%@  As an example, consider how to declare the determinacy of the builtin
%@  @code{length(List, Length)}:
%@  @enumerate
%@  @item
%@  It will succeed at most once if the second argument is instantiated
%@  (It will succeed once if the first argument can be unified with a list
%@  of the specified length, otherwise it will fail), regardless of the
%@  instantiation of the first argument. This corresponds to the attribute
%@  @code{semidet}.
%@
%@  @item
%@  It will succeed exactly once if the first argument is a @use{proper
%@  list} and the second argument is a variable. This corresponds to the
%@  attribute @code{det}.
%@
%@  @item
%@  It will succeed more than once if the first argument is a @use{partial
%@  list} (i.e.@: is a variable or has a variable tail). This corresponds
%@  to the attribute @code{multi}.
%@
%@  @item
%@  Finally, it will throw an exception for some invalid inputs, but this
%@  is not specified with determinacy declarations (@code{@dots{} is
%@  throwing} is only meant for predicates expected to @emph{always} throw
%@  an exception).
%@  @end enumerate
%@
%@  However, it is not possible to specify ``proper list'', ``partial
%@  list`` and non-list as instantiation patterns. On the other hand, it
%@  would be unfortunate to not be able to say anything about the
%@  determinacy of @code{length/2}.
%@
%@  In such cases it may make sense to pretend that @samp{+}, the
%@  non-variable argument instantiation, means ``a properly/fully
%@  instantiated input'', i.e.@: a ``proper list'' in the case of
%@  @code{length/2}. Similarly, it may make sense to pretend that
%@  @samp{?}, any instantiation, means ``a partially instantiated input'',
%@  i.e.@: a ``partial list'' in the case of @code{length/2}.
%@
%@  So, for the builtin @code{length/2} it would make sense to specify the
%@  following determinacy declarations:
%@  @example
%@  @group
%@  :- length(*, +) is semidet.     % this is precise
%@  :- length(+, -) is det.         % pretend '+' means proper list
%@  :- length(?, -) is multi.       % pretend '?' means partial list
%@  @end group
%@  @end example
%@  It is up to the documentation accompanying the predicate,
%@  and any tools that use these declarations, to handle this appropriately.
%@
%@  @subsection Accessing Determinacy Declarations at Runtime
%@
%@  The determinacy declarations are saved when code is compiled or
%@  consulted and can be accessed when the code has been loaded. This
%@  could be used by documentation generators, smart debuggers, and many
%@  other purposes, not
%@  all of which documented.  New uses may be added without notice,
%@  so you should ignore any recorded @code{is/2} directive that you
%@  do not understand.
%@
%@  The loaded @code{is/2} directives can be accessed using the following,
%@  non-determinate, predicate:
%@  @example
%@  @group
%@  current_is_directive(Skel, M, Annotation, Spec, Directive, Context).
%@  @end group
%@  @end example
%@
%@  @table @var
%@  @item Skel
%@  This is a compound term with the same name as the predicate and one
%@  anonymous variable in each argument position.
%@
%@  @item M
%@  This is the module of the predicate specification. Typically the same as the source module.
%@
%@  @item Annotation
%@  This is the second argument of the @code{is/2} directive.
%@
%@  @item Spec
%@  This is the first argument of the @code{is/2} directive, with module prefixes peeled off.
%@
%@
%@  @item Directive
%@  This is the entire @code{is/2} directive (without the surrounding @code{:- @enddots{}}).
%@  @item Context
%@  this is the source module.
%@  @end table
%@
%@  Consider the following code:
%@  @example
%@  @group
%@  :- module(example, [p1/3]).
%@
%@  :- p1/3 is det.
%@  :- user:bar(+, +) is semidet.
%@
%@  @end group
%@  @end example
%@
%@  This corresponds to the following two clauses of @code{current_is_directive/6}:
%@  @example
%@  @group
%@  current_is_directive(p1(_,_,_),
%@                       example,
%@                       det,
%@                       p1/3,
%@                       (p1/3 is det),
%@                       example).
%@  current_is_directive(bar(_,_),
%@                       user,
%@                       semidet,
%@                       bar(+,+),
%@                       (user:bar(+,+) is semidet),
%@                       example).
%@  @end group
%@  @end example
%@
%@

:- module(is_directive, [current_is_directive/3, current_is_directive/6]).
:- meta_predicate current_is_directive(:, -, -).

%@  Exported predicates:
%@
%@  @table @code

%@  @item current_is_directive(@var{Skel}, @var{M}, @var{Annotation}, @var{Spec}, @var{Directive}, @var{Context}).
%@  Low-level access to the information recorded by @code{is/2}
%@  directives. See the library documentation for details.
%@
:- current_is_directive/6 is nondet.
current_is_directive(Skel, M, Annotation, Spec, Directive, Context) :-
        prolog:current_is_directive(Skel, M, Annotation, Spec, Directive, Context).

%@  @item current_is_directive(@var{:MSkel}, @var{Annotation}, @var{Spec}).
%@  Like @code{current_is_directive(@var{Skel}, @var{M},
%@  @var{Annotation}, @var{Spec}, _, _)} where @var{M} and @var{Skel}
%@  are the parts of the meta argument @var{MSkel}.
%@
:- current_is_directive/3 is nondet.
current_is_directive(M:Skel, Spec, Annotation) :-
        prolog:current_is_directive(Skel, M, Annotation, Spec, _Directive, _Context).

%@  @end table @c Exported Predicates
