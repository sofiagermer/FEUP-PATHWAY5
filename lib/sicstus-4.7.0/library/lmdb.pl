/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
% Copyright (C) 2021, RISE Research Institutes of Sweden AB.

%@  This library module handles storage and retrieval of terms
%@  on files.  By using indexing, the store/retrieve operations are
%@  efficient also for large data sets.  The package is an interface to the
%@  Lightning Memory-Mapped Database Manager (LMDB).  The API is quite similar
%@  to the deprecated @code{library(bdb)}, and provides essentially the same functionality,
%@  although some details differ.
%@  The library module is part of SICStus Prolog since release 4.7.0.
%@
%@  @menu
%@  * LMDB Basics:: Basics
%@  * LMDB Current Limitations:: Current Limitations
%@  * Lightning Memory-Mapped Database Manager:: Lightning Memory-Mapped Database Manager
%@  * LMDB DB-Spec Intro:: DB-Spec---Introduction
%@  * LMDB Predicates:: Predicates
%@  * LMDB Example Session:: Example Session
%@  * LMDB DB-Spec Details:: DB-Spec---Details
%@  * LMDB Export-Import:: Exporting and Importing a Database
%@  @end menu
%@
%@  @node LMDB Basics
%@  @subsection Basics
%@
%@  The idea is to get a behavior similar to @code{assert/1},
%@  @code{retract/1} and @code{clause/2}, but the terms are stored on
%@  file instead of in primary memory.  Like the Prolog database,
%@  LMDB will store variables with attributes or with blocked
%@  goals as ordinary variables.
%@
%@  The differences compared with the Prolog database are:
%@
%@  @itemize @bullet
%@  @item
%@  @cindex database
%@  A @dfn{database} must be explicitly created, and occupies a file system directory.
%@  A database must be opened before any access and closed after the
%@  last access.
%@
%@  @item
%@  The functors and the indexing specifications of the terms to
%@  be stored have to be given when the database is
%@  created. It is possible to index on other parts of the term than just the
%@  functor and first argument. (@pxref{LMDB DB-Spec Details}).
%@
%@  @item
%@  Changes are not guaranteed to affect the database until it is explicitly @dfn{synced}.
%@  @end itemize
%@
%@  @node LMDB Current Limitations
%@  @subsection Current Limitations
%@
%@  @itemize @bullet
%@  @item
%@  The terms are not necessarily fetched in the same order as they
%@  were stored.
%@
%@  @item
%@  The number of terms ever inserted in a database cannot exceed 2^32-1.
%@
%@  @item
%@  A size limit must be set at creation time. An LMDB database cannot grow beyond that limit.
%@  @end itemize
%@
%@  @node Lightning Memory-Mapped Database Manager
%@  @subsection Lightning Memory-Mapped Database Manager (LMDB)
%@
%@  LMDB is a software library that provides a transactional database in
%@  the form of a key-value store. LMDB stores arbitrary key/data pairs as
%@  byte arrays, has a range-based search capability, and supports
%@  multiple data items for a single key.
%@  LMDB is fully thread-aware and supports concurrent read/write
%@  access from multiple processes and threads.
%@
%@  For details on the underlying technology, we refer to the OpenLDAP
%@  home page @uref{https://www.openldap.org/}.
%@
%@  @node LMDB DB-Spec Intro
%@  @subsection The DB-Spec---Informal Description
%@
%@  @cindex db-spec
%@  The @dfn{db-spec} defines which functors are allowed and which
%@  parts of a term are used for indexing in a database.  The
%@  syntax of a db-spec is a skeletal goal with no module.  The
%@  db-spec is a list of atoms and compound terms where
%@  the arguments are either @code{+} or @code{-}.  A term can
%@  be inserted in the database if there is a spec in the spec
%@  list with the same functor.
%@
%@  Multilevel indexing is not supported, terms have to be
%@  ``flattened''.
%@
%@  @cindex indexed term
%@  @cindex term, indexed
%@  Every spec with the functor of the @dfn{indexed term} specifies an
%@  indexing.  Every argument where there is a @code{+} in the spec is
%@  indexed on.
%@
%@  The idea of the db-spec is illustrated with a few examples.  (A section
%@  further down explains the db-spec in a more formal way).
%@
%@  Given a spec of @code{[f(+,-), .(+,-), g, f(-,+)]} the indexing works
%@  as follows.  (The parts with indexing are underlined.)
%@
%@  @c The ugly "@ " are so that multiple spaces are not collapsed into one.
%@  @multitable { mmmmmmmm } { domain error mmmmmmm } { instantiation error }
%@  @item @var{Term}               @tab @var{Store}                      @tab @var{Fetch}
%@  @item
%@  @item @code{g(x,y)}              @tab domain error              @tab domain error
%@  @item
%@  @item @code{f(A,B)}       @tab @code{f(A,B)}                    @tab instantiation error
%@  @item                     @tab @code{-}
%@  @item
%@  @item @code{f(a,b)}       @tab @code{f(a,b)@ @ f(a,b)}            @tab @code{f(a,b)}
%@  @item                     @tab @code{-@ -@ @ @ @ @ -@ @ @ -}             @tab @code{-@ -}
%@  @item
%@  @item @code{[a,b]}        @tab @code{.(a,.(b,[]))}              @tab @code{.(a,.(b,[]))}
%@  @item                     @tab @code{-@ -}                       @tab @code{-@ -}
%@  @item
%@  @item @code{g}            @tab @code{g}                         @tab @code{g}
%@  @item                     @tab @code{-}                         @tab @code{-}
%@  @end multitable
%@
%@  The specification @code{[f(+,-), f(-,+)]} is different from
%@  @code{[f(+,+)]}.  The first specifies that two indices are to be made
%@  whereas the second specifies that only one index is to be made on both
%@  arguments of the term.
%@
%@  @node LMDB Predicates
%@  @subsection Predicates
%@
%@  @menu
%@  * LMDB Conventions:: Conventions
%@  * LMDB Memory Leaks:: Memory Leaks
%@  * LMDB The Predicates:: The Predicates
%@  @end menu
%@
%@
%@  @node LMDB Conventions
%@  @subsubsection Conventions
%@
%@  The following conventions are used in the predicate descriptions below.
%@  @itemize @bullet
%@  @item
%@  The predicates that create and open databases can be given a list of options.
%@  An @var{Option} is one of:
%@  @table @code
%@  @item mapsize(@var{S})
%@  @findex mapsize/1 (lmdb option)
%@  The mapsize, or size limit, of the database being created, in MiB.
%@  The default is 10MiB.
%@
%@  @item permission(@var{S})
%@  @findex permission/1 (lmdb option)
%@  The UNIX permission for files of the database being created.  Ignored for Windows.
%@  The default is octal 664.
%@
%@  @item mode(@var{S})
%@  @findex mode/1 (lmdb option)
%@  The access mode for the database being opened. One of the atoms @code{read} and @code{write}.
%@  If @code{read} (the default), no updates are allowed.
%@  @end table
%@
%@  @item
%@  @var{DBRef} is a reference to an open database.
%@  It is returned when the database is opened.  The reference becomes
%@  invalid after the database has been closed.
%@  The reference corresponds to the LMDB environment notion.
%@  An environment
%@  physically consists of a directory containing the files needed to enable
%@  sharing databases between processes.
%@
%@  @item
%@  @var{TermRef} is a reference to a term in a given database.
%@  The reference is returned when a term is stored.  The reference
%@  stays valid even after the database has been closed and hence can
%@  be stored permanently as part of another term.  However, if such
%@  references are stored in the database, automatic compression of
%@  the database (using @code{lmdb_compress/[2,3]}) is not possible, in
%@  that case the user has to write their own compressing predicate.
%@
%@  @item
%@  @var{SpecList} is a description of the indexing scheme;
%@  @pxref{The DB-Spec}.
%@
%@  @item
%@  @var{Term} is any Prolog term.
%@
%@  @item
%@  @var{Iterator} is a non-backtrackable mutable object.  It can be
%@  used to iterate through a set of terms stored in a database.
%@  The iterators are unidirectional.
%@  @end itemize
%@
%@  @node LMDB Memory Leaks
%@  @subsubsection Memory Leaks
%@  In order to avoid memory leaks, databases and
%@  iterators should always be closed explicitly.  Consider using
%@  @code{lmdb_with_db/[2,3]} and @code{lmdb_with_iterator/3} to
%@  automate the closing/deallocation of these objects.
%@
%@  @quotation
%@  @strong{Please note}: a database must not be closed while there are outstanding
%@  choices for some @code{lmdb_fetch/3} or @code{lmdb_enumerate/3} goal that refers to that
%@  database.  Outstanding choices can be removed with a cut
%@  (@code{!}).
%@  @end quotation
:- module(lmdb, [
                    lmdb_create/2,
                    lmdb_create/3,
                    lmdb_open/2,
                    lmdb_open/3,
                    lmdb_close/1,
                    lmdb_store/3,
                    lmdb_fetch/3,
                    lmdb_erase/2,
                    lmdb_enumerate/3,
                    lmdb_findall/5,
                    lmdb_compress/2,
                    lmdb_sync/1,
                    lmdb_make_iterator/3,
                    lmdb_iterator_next/3,
                    lmdb_iterator_done/1,
                    lmdb_property/2,
                    lmdb_export/2,
                    lmdb_import/2,
                    lmdb_with_db/2,
                    lmdb_with_db/3,
                    lmdb_with_iterator/3
                ]).

:- use_module(library(types), [
                                  must_be/4,
                                  illarg/3,
                                  illarg/4
                              ]).

:- use_module(library(fastrw), [
                                   fast_buf_read/2,
                                   fast_buf_write/3
                               ]).

:- use_module(library(file_systems), [
                                         directory_exists/1,
                                         file_exists/1,
                                         make_directory/1
                                     ]).

:- meta_predicate
           lmdb_with_db(+, 1),
           lmdb_with_db(+, 1, +),
           lmdb_with_iterator(+, +, 1),
           lmdb_findall(+, +, +, 0, -).

:- meta_predicate
           db_findall_cont(+, +, 0, -, +),
           unify_call(+, +, 0).

:- dynamic
           '$db_specs'/4.

:- volatile
           '$db_specs'/4.

%@  @node LMDB The Predicates
%@  @subsubsection The Predicates
%@  @table @code
%@  @item lmdb_create(@var{+Path}, @var{+SpecList})
%@  @itemx lmdb_create(@var{+Path}, @var{+SpecList}, @var{+Options})
%@  @PLXindex {lmdb_create/[2,3] (lmdb)}
%@  Creates a brand new LMDB database with the given spec list.
%@  @var{Path} should be a file system path where a new directory can be created.
%@  If some file or directory already exists there, an exception is raised.
:- lmdb_create/2 is det.
lmdb_create(Path, SpecList) :-
        lmdb_create(Path, SpecList, []).

:- lmdb_create/3 is documented_as(lmdb_create/2).
:- lmdb_create/3 is det.
lmdb_create(Path, SpecList, Options) :-
        ErrGoal = lmdb_create(Path,SpecList,Options),
        must_be(Options, proper_list, ErrGoal, 3),
        db_open_options(Options, opt(10,0o664,read), opt(MapSize,Permission,_Mode), ErrGoal, 3),
        prolog:absolute_file_name(Path, AbsPath, [file_type(directory),access([]),file_errors(error)], ErrGoal, 1),
        (   \+ file_exists(AbsPath),
            \+ directory_exists(AbsPath)
               ->
            make_directory(AbsPath)
        ;   illarg(permission('create new',directory,0), ErrGoal, 1)
        ),
        db_create_int(AbsPath, MapSize, SpecList, Permission, ErrGoal).

db_create_int(_, _MapSize, SpecList0, _, ErrGoal) :-
        var(SpecList0), !,
        illarg(var, ErrGoal, 2).
db_create_int(AbsPath, MapSize, SpecList0, Permission, ErrGoal) :-
        must_be_valid_spec_list(SpecList0, ErrGoal, 2),
        normalise_spec_list(SpecList0, SpecList),
        fast_buf_write(SpecList, SpecSize, SpecAddr),
        c_create_db(AbsPath, MapSize, SpecAddr, SpecSize, Permission, R),
        db_error_chk(R, lmdb_create_db).

%@  @item lmdb_open(@var{+Path}, @var{-DBRef})
%@  @itemx lmdb_open(@var{+Path}, @var{-DBRef}, @var{+Options})
%@  @PLXindex {lmdb_open/[2,3] (lmdb)}
%@  Opens an existing database and unifies @var{DBRef} with its reference.
%@  @var{Path} should be a file system path to the directory that holds the database.
%@
%@  @strong{Please note:} Only one @var{DBRef} may be connected to the same @var{Path} at
%@  the same time in the same process. I.e., you must not call @code{lmdb_open/2} twice with
%@  the same @var{Path} without closing the @var{DBRef} before the second call.
lmdb_open(Path, DBRef) :-
        lmdb_open(Path, DBRef, []).

:- lmdb_open/3 is documented_as(lmdb_open/2).
:- lmdb_open/3 is det.
lmdb_open(Path, DBRef, Options) :-
        DBRef = '$lmdb'(DBAddr),
        ErrGoal = lmdb_open(Path,DBRef,Options),
        must_be(Options, proper_list, ErrGoal, 3),
        db_open_options(Options, opt(10,0o664,read), opt(MapSize,Permission,Mode), ErrGoal, 3),
        prolog:absolute_file_name(Path, AbsPath, [file_type(directory),access(exist),file_errors(error)], ErrGoal, 1),
        c_open_db(AbsPath, MapSize, Permission, Mode, DBAddr, R),
        db_error_chk(R, open_db),
        db_get_spec(DBAddr, SpecList),
        assert_applicable_keys(SpecList, DBAddr, 1).

db_get_spec(DBAddr, SpecList) :-
        c_read_spec(DBAddr, RealSpecAddr, R1),
        db_error_chk(R1, read_spec),
        fast_buf_read(SpecList, RealSpecAddr).

db_open_options([], Opt, Opt, _, _).
db_open_options([X|L], Opt0, Opt, Goal, ArgNo) :-
        (   nonvar(X),
            db_open_option(X, Opt0, Opt1) ->
            true
        ;   illarg(domain(term,lmdb_open_option), Goal, ArgNo, X)
        ),
        db_open_options(L, Opt1, Opt, Goal, ArgNo).

db_open_option(mapsize(Si), opt(_Si,Pe,Mo), opt(Si,Pe,Mo)) :-
        integer(Si),
        Si > 0.
db_open_option(permission(Pe), opt(Si,_Pe,Mo), opt(Si,Pe,Mo)) :-
        integer(Pe),
        Pe >= 0.
db_open_option(mode(Mo), opt(Si,Pe,_Mo), opt(Si,Pe,Mo)) :-
        atom(Mo),
        memberchk(Mo, [read,write]).

valid_spec_list(SL) :- var(SL), !.
valid_spec_list(SL) :-
        SL = [_|_],
        (   foreach(S, SL)
        do
            valid_spec(S)
        ).

valid_spec(S) :-
        nonvar(S),
        functor(S, N, _),
        atom(N),
        (   foreacharg(A, S)
        do
            valid_argspec(A)
        ).

valid_argspec(?) :- !,
        fail.
valid_argspec(+).
valid_argspec(-).

normalise_spec_list(SpecList, NSpecList) :-
        (   foreach(S, SpecList),
            foreach(K-S, KSpecList)
        do
            spec_key(S, K)
        ),
        keysort(KSpecList, KSSpecList),
        (   foreach(_-X, KSSpecList),
            foreach(X, NSpecList)
        do
            true
        ).

spec_key(S, K) :-
        functor(S, F, A),
        minus_args(S, 1, A, 0, P),
        functor(K, F, 2),
        arg(1, K, A),
        arg(2, K, P).

minus_args(S, I, A, P0, P) :-
        (   I =< A
        ->
            arg(I, S, X),
            (   X == (-)
            ->
                P1 is P0+1
            ;   P1 = P0
            ),
            I1 is I+1,
            minus_args(S, I1, A, P1, P)
        ;   P = P0
        ).

assert_applicable_keys([], _, _).
assert_applicable_keys([Spec|SpecList], DBAddr, N) :-
        functor(Spec, F, A),
        functor(SpecPattern, F, A),
        same_functor_prefix(SpecList, F, A, SameFs, Rest, N, N1),
        assert('$db_specs'(SpecPattern, DBAddr, N, [Spec|SameFs])),
        assert_applicable_keys(Rest, DBAddr, N1).

same_functor_prefix([], _F, _A, [], [], N, N).
same_functor_prefix(SpecList, F, A, SameFs, Rest, N0, N) :-
        SpecList = [S|Spec],
        functor(S, F1, A1),
        (   F == F1,
            A == A1
        ->
            SameFs = [S|SameFs1],
            N1 is N0+1,
            same_functor_prefix(Spec, F, A, SameFs1, Rest, N1, N)
        ;   SameFs = [],
            Rest = SpecList,
            N = N0
        ).

%@  @item lmdb_with_db(@var{+Path}, @var{:Goal})
%@  @itemx lmdb_with_db(@var{+Path}, @var{:Goal}, @var{+Options})
%@  @PLXindex {lmdb_with_db/[2,3] (lmdb)}
%@  Opens an existing database, calls @var{Goal} with its reference, as if by @code{call(Goal, DBRef)}, and ensures that
%@  the database is closed afterwards.
%@  @var{Goal} may backtrack and succeed multiple times.
%@  @var{Path} should be a file system path to the directory that holds the database.
%@
%@  @strong{Please note:} see @code{lmdb_open/2} for restrictions on
%@  opening the same @var{Path} more than once at the same time.
:- lmdb_with_db/2 is nondet.
lmdb_with_db(Path, Goal) :-
        lmdb_with_db(Path, Goal, []).

:- lmdb_with_db/3 is documented_as(lmdb_with_db/2).
:- lmdb_with_db/3 is nondet.
lmdb_with_db(Path, Goal, Options) :-
        lmdb_open(Path, DBRef, Options),
        call_cleanup(call(Goal, DBRef), lmdb_close(DBRef)).

%@  @item lmdb_close(@var{+DBRef})
%@  @PLXindex {lmdb_close/1 (lmdb)}
%@  Closes the database referenced by @var{DBRef}.
:- lmdb_close/1 is det.
lmdb_close(DBRef) :-
        ErrGoal = lmdb_close(DBRef),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        db_close_int(DBAddr).

db_close_int(DBAddr) :-
        c_close_db(DBAddr, R),
        db_error_chk(R, close_db),
        retractall('$db_specs'(_, DBAddr, _, _)).

%@  @item lmdb_store(@var{+DBRef}, @var{+Term}, @var{-TermRef})
%@  @PLXindex {lmdb_store/3 (lmdb)}
%@  Stores @var{Term} in the database @var{DBRef}.  @var{TermRef} is
%@  unified with a corresponding term reference.  The
%@  functor of @var{Term} must match the functor of a spec in
%@  the db-spec associated with @var{DBRef}.
:- lmdb_store/3 is det.
lmdb_store(DBRef, Term, TermRef) :-
        ErrGoal = lmdb_store(DBRef,Term,TermRef),
        TermRef = '$db_termref'(ITermRef),
        must_be(Term, callable, ErrGoal, 2),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        db_store_int(DBAddr, Term, ITermRef, ErrGoal).

db_store_int(DBAddr, Term, ITermRef, ErrGoal) :-
        index_keys(Term, DBAddr, Keys),
        (   Keys = []
        ->
            illarg(domain(term,'valid term'), ErrGoal, 2, Term)
        ;   true
        ),
        hash_key_set(Keys, HCs),
        c_next_termref(DBAddr, ITermRef, R1),
        db_error_chk(R1, next_termref),
        (   foreach(H, HCs),
            param(DBAddr, ITermRef)
        do
            c_store_termref(DBAddr, H, ITermRef, R2),
            db_error_chk(R2, store_termref)
        ),
        fast_buf_write(Term, TermSize, TermAddr),
        c_store_term(DBAddr, ITermRef, TermAddr, TermSize, R3),
        db_error_chk(R3, store_term).

hash_key_set(Ks, HCs) :-
        (   foreach(K, Ks),
            foreach(H, HCs0)
        do
            c_term_hash(K, H)
        ),
        sort(HCs0, HCs).

%@  @item lmdb_fetch(@var{+DBRef}, @var{?Term}, @var{?TermRef})
%@  @PLXindex {lmdb_fetch/3 (lmdb)}
%@  Unifies @var{Term} with a term from the database
%@  @var{DBRef}.  At the same time, @var{TermRef} is unified with a
%@  corresponding term reference.  Backtracking over the
%@  predicate unifies with all terms matching @var{Term}.
%@
%@  If @var{TermRef} is not instantiated, both the functor
%@  and the instantiatedness of @var{Term} must match a spec in the
%@  db-spec associated with @var{DBRef}.
%@
%@  If @var{TermRef} is instantiated, the referenced term is
%@  read and unified with @var{Term}.
%@
%@  If you simply want to find all matching terms, it is more
%@  efficient to use @code{lmdb_findall/5} or @code{lmdb_enumerate/3}.
:- lmdb_fetch(+, -, +) is semidet.
:- lmdb_fetch(+, ?, -) is nondet.
lmdb_fetch(DBRef, Term, TermRef) :-
        nonvar(TermRef), !,
        ErrGoal = lmdb_fetch(DBRef,Term,TermRef),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        must_be_termref(TermRef, ITermRef, ErrGoal, 3),
        fetch_ref(DBAddr, Term, ITermRef).
lmdb_fetch(DBRef, Term, TermRef) :-
        TermRef = '$db_termref'(ITermRef),
        ErrGoal = lmdb_fetch(DBRef,Term,TermRef),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        must_be(Term, callable, ErrGoal, 2),
        query_keys(Term, DBAddr, Keys, ErrGoal),
        hash_key_set(Keys, HCs),
        c_term_iterator(DBAddr, HCs, ItAddr, R),
        db_error_chk(R, term_iterator),
        call_cleanup(tfetch(DBAddr, ItAddr, Term, ITermRef), tfetch_cleanup(DBAddr, ItAddr)).

fetch_ref(DBAddr, Term, ITermRef) :-
        c_fetch_term(DBAddr, ITermRef, TermAddr, R),
        db_error_chk(R, fetch_term),
        fast_buf_read(Term, TermAddr).

:- tfetch/4 is nondet.
tfetch(DBAddr, ItAddr, Term, ITermRef) :-
        repeat,
        c_term_iterator_next(DBAddr, ItAddr, TermAddr, ITermRef, R),
        db_error_chk(R, term_iterator_next),
        (   TermAddr =:= 0 ->
            !,
            fail
        ;   fast_buf_read(Term, TermAddr)
        ).

tfetch_cleanup(DBAddr, ItAddr) :-
        c_term_iterator_done(DBAddr, ItAddr, R),
        db_error_chk(R, term_iterator_done).

%@  @item lmdb_erase(@var{+DBRef}, @var{+TermRef})
%@  @PLXindex {lmdb_erase/2 (lmdb)}
%@  Deletes the term from the database @var{DBRef} that is
%@  referenced by @var{TermRef}.
:- lmdb_erase/2 is det.
lmdb_erase(DBRef, TermRef) :-
        ErrGoal = lmdb_erase(DBRef,TermRef),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        must_be_termref(TermRef, ITermRef, ErrGoal, 2),
        fetch_ref(DBAddr, Term, ITermRef),
        index_keys(Term, DBAddr, Keys),
        hash_key_set(Keys, HCs),
        c_delete_term(DBAddr, ITermRef, R),
        db_error_chk(R, delete_term),
        (   foreach(H, HCs),
            param(DBAddr, ITermRef)
        do
            c_delete_termref(DBAddr, H, ITermRef, S),
            db_error_chk(S, delete_termref)
        ).

%@  @item lmdb_enumerate(@var{+DBRef}, @var{?Term}, @var{-TermRef})
%@  @PLXindex {lmdb_enumerate/3 (lmdb)}
%@
%@  Unifies @var{Term} with a term from the database
%@  @var{DBRef}.  At the same time, @var{TermRef} is unified with a
%@  corresponding term reference.  Backtracking over the
%@  predicate unifies with all terms matching @var{Term}.
%@
%@  Implemented by linear search---the db-spec associated with @var{DBRef}
%@  is ignored.
:- lmdb_enumerate/3 is nondet.
lmdb_enumerate(DBRef, Term, TermRef) :-
        TermRef = '$db_termref'(ITermRef),
        ErrGoal = lmdb_enumerate(DBRef,Term,TermRef),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        c_global_iterator(DBAddr, ItAddr, R),
        db_error_chk(R, global_iterator),
        call_cleanup(gfetch(DBAddr, ItAddr, Term, ITermRef), gfetch_cleanup(DBAddr, ItAddr)).

:- gfetch/4 is nondet.
gfetch(DBAddr, ItAddr, Term, ITermRef) :-
        repeat,
        c_global_iterator_next(DBAddr, ItAddr, TermAddr, ITermRef, R),
        db_error_chk(R, global_iterator_next),
        (   TermAddr =:= 0 ->
            !,
            fail
        ;   fast_buf_read(Term, TermAddr)
        ).

gfetch_cleanup(DBAddr, ItAddr) :-
        c_global_iterator_done(DBAddr, ItAddr, R),
        db_error_chk(R, global_iterator_done).

%@  @item lmdb_findall(@var{+DBRef}, @var{+Template}, @var{+Term}, @var{:Goal}, @var{-Bag})
%@  @PLXindex {lmdb_findall/3 (lmdb)}
%@  Unifies @var{Bag} with the list of instances of
%@  @var{Template} in all proofs of @var{Goal} found when @var{Term} is
%@  unified with a matching term from the database
%@  @var{DBRef}.  Both the functor and the instantiatedness of
%@  @var{Term} must match a spec in the db-spec associated with @var{DBRef}.
%@  Conceptually, this predicate is equivalent to
%@  @code{findall(@var{Template}, (lmdb_fetch(@var{DBRef}, @var{Term}, _), @var{Goal}), @var{Bag})}.
:- lmdb_findall(+, +, +, 0, -) is det.
lmdb_findall(DBRef, Template, Term, Goal, Bag) :-
        lmdb_with_iterator(DBRef, Term, db_findall_cont(Template, Term, Goal, Bag)).

db_findall_cont(Template, Term, Goal, Bag, Iterator) :-
        db_iterator_next_int(Iterator, Term1, _, R),
        (   R =\= 0 ->
            Bag = []            % no more terms
        ;   findall(Template, unify_call(Term, Term1, Goal), Bag, BagT),
            db_findall_cont(Template, Term, Goal, BagT, Iterator)
        ).

:- unify_call/3 is nondet.
unify_call(T, T, G) :-
        call(G).

%@  @item lmdb_compress(@var{+DBRefFrom}, @var{+DBRefTo})
%@  @PLXindex {lmdb_compress/2 (lmdb)}
%@  Copies the database given by @var{DBRefFrom} to a new database
%@  given by @var{DBRefTo}.
%@  The new database will be a compressed
%@  version of the first one in the sense that it will not have ``holes''
%@  resulting from deletion of terms.  Deleted term references
%@  will also be reused, which implies that references that refer to
%@  terms in the old database will be invalid in the new one.
%@
%@  The terms of @var{DBRefFrom} will be appended to any existing terms of @var{DBRefTo}.  Of course, @var{DBRefTo} must have
%@  an indexing specification that enables the terms in @var{DBRefFrom} to be inserted into it.
:- lmdb_compress/2 is det.
lmdb_compress(DBRefFrom, DBRefTo) :-
        ErrGoal = lmdb_compress(DBRefFrom,DBRefTo),
        must_be_db(DBRefFrom, _DBAddrFrom, ErrGoal, 1),
        must_be_db(DBRefTo, DBAddrTo, ErrGoal, 2),
        lmdb_make_iterator(DBRefFrom, _, Iterator),
        call_cleanup(copy_db(Iterator, DBAddrTo), db_iterator_done_int(Iterator)).

copy_db(Iterator, DBAddr) :-
        db_iterator_next_int(Iterator, Term, _, R),
        (   R =\= 0 ->
            true                % no more terms
        ;   db_store_int(DBAddr, Term, _, 0),
            copy_db(Iterator, DBAddr)
        ).

%@  @item lmdb_sync(@var{+DBRef})
%@  @PLXindex {lmdb_sync/1 (lmdb)}
%@  Flushes any cached information from the database referenced by @var{DBRef} to stable storage.
:- lmdb_sync/1 is det.
lmdb_sync(DBRef) :-
        ErrGoal = lmdb_sync(DBRef),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        c_sync_db(DBAddr, R),
        db_error_chk(R, sync_db).

%@  @item lmdb_make_iterator(@var{+DBRef}, @var{+Term}, @var{-Iterator})
%@  @PLXindex {lmdb_make_iterator/3 (lmdb)}
%@  Creates a new iterator and unifies it with @var{Iterator}.
%@  The iterator iterates through the terms that would be found by
%@  @code{lmdb_fetch(@var{DBRef}, @var{Term}, _)}.
%@
%@  Every iterator created by @code{lmdb_make_iterator/3} must be
%@  destroyed with @code{lmdb_iterator_done/1}.
:- lmdb_make_iterator/3 is det.
lmdb_make_iterator(DBRef, Term, Iterator) :-
        ErrGoal = lmdb_make_iterator(DBRef,Term,Iterator),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        (   var(Term)
        ->
            Iterator = '$db_global_it'(DBAddr,ItAddr),
            c_global_iterator(DBAddr, ItAddr, R),
            db_error_chk(R, global_iterator)
        ;   must_be(Term, callable, ErrGoal, 2),
            Iterator = '$db_term_it'(DBAddr,ItAddr,Term),
            query_keys(Term, DBAddr, Keys, ErrGoal),
            hash_key_set(Keys, HCs),
            c_term_iterator(DBAddr, HCs, ItAddr, R),
            db_error_chk(R, term_iterator)
        ).

%@  @item lmdb_with_iterator(@var{+DBRef}, @var{+Term}, @var{:Goal})
%@  @PLXindex {lmdb_with_iterator/3 (lmdb)}
%@  Creates a new iterator, calls @var{Goal} with it, as if by @code{call(Goal, Iterator)}, and ensures that
%@  the iterator is destroyed afterwards.
%@  @var{Goal} may backtrack and succeed multiple times.
%@  See the documentation of @var{lmdb_make_iterator/3}.
:- lmdb_with_iterator/3 is nondet.
lmdb_with_iterator(DBRef, Term, Goal) :-
        lmdb_make_iterator(DBRef, Term, Iterator),
        call_cleanup(call(Goal, Iterator), db_iterator_done_int(Iterator)).

%@  @item lmdb_iterator_next(@var{+Iterator}, @var{-Term}, @var{-TermRef})
%@  @PLXindex {lmdb_iterator_next/3 (lmdb)}
%@  @var{Iterator} advances to the next term, and @var{Term} and
%@  @var{TermRef} are unified with the term and its reference
%@  pointed to by @var{Iterator}.  If there is no next term, the
%@  predicate fails.
:- lmdb_iterator_next(+Iterator, -Term, -TermRef) is semidet.
lmdb_iterator_next(Iterator, Term, TermRef) :-
        db_iterator_next_int(Iterator, Term, TermRef, 0).

db_iterator_next_int('$db_global_it'(DBAddr,ItAddr), Term, '$db_termref'(ITermRef), Code) :-
        c_global_iterator_next(DBAddr, ItAddr, TermAddr, ITermRef, R),
        db_error_chk(R, global_iterator_next),
        (   TermAddr =:= 0 ->
            Code = 1            % no more terms
        ;   Code = 0,
            fast_buf_read(Term, TermAddr)
        ).
db_iterator_next_int('$db_term_it'(DBAddr,ItAddr,Orig), Term, TermRef, Code) :-
        copy_term(Orig, Copy),
        term_iterator_next(DBAddr, ItAddr, Copy, Term, TermRef, Code).

term_iterator_next(DBAddr, ItAddr, Copy, Term, TermRef, Code) :-
        c_term_iterator_next(DBAddr, ItAddr, TermAddr, ITermRef, R),
        db_error_chk(R, term_iterator_next),
        (   TermAddr =:= 0 ->
            Code = 1            % no more terms
        ;   fast_buf_read(TermR, TermAddr),
            (   Copy = TermR ->
                Term = TermR,
                TermRef = '$db_termref'(ITermRef),
                Code = 0
            ;   term_iterator_next(DBAddr, ItAddr, Copy, Term, TermRef, Code)
            )
        ).

%@  @item lmdb_iterator_done(@var{+Iterator})
%@  @PLXindex {lmdb_iterator_done/1 (lmdb)}
%@  Deallocates @var{Iterator}, which must not be in use anymore.
:- lmdb_iterator_done/1 is det.
lmdb_iterator_done(Iterator) :-
        db_iterator_done_int(Iterator).

db_iterator_done_int('$db_global_it'(DBAddr,ItAddr)) :-
        c_global_iterator_done(DBAddr, ItAddr, R),
        db_error_chk(R, global_iterator_done).
db_iterator_done_int('$db_term_it'(DBAddr,ItAddr,_)) :-
        c_term_iterator_done(DBAddr, ItAddr, R),
        db_error_chk(R, term_iterator_done).

% Syntax of specs
% ---------------
%
% speclist  = [spec1, ..., specM]
% spec      = FUNCTOR(argspec1, ..., argspecN)
% argspec   = + | -
query_keys(Term, DB, Keys, ErrGoal) :-
        '$db_specs'(Term, DB, I0, Spec), !,
        keylist(Spec, I0, Term, Key),
        (   Key \== none ->
            Key = [I|M]
        ;   illarg(var, ErrGoal, 2, Term)
        ),
        (   M = [] ->
            Keys = [[I]]
        ;   var_code(VAR),
            Keys = [Key,[I,VAR]]
        ).
query_keys(Term, _, _, ErrGoal) :-
        illarg(domain(term,'valid term'), ErrGoal, 2, Term).

keylist([], _, _, none).
keylist([S|Spec], I, T, Key) :-
        c_index_keys(S, T, K, C),
        (   C < 0 ->
            I1 is I+1,
            keylist(Spec, I1, T, Key)
        ;   Key = [I|K]
        ).

index_keys(Term, DB, Keys) :-
        '$db_specs'(Term, DB, I0, Spec), !,
        index_keys(Spec, I0, Term, Keys).
index_keys(_, _, []).

index_keys([], _, _, []).
index_keys([S|Spec], N, T, Keys) :-
        c_index_keys(S, T, K, C),
        (   C =:= -1 ->
            Keys = Keys1
        ;   C =:= -2 ->
            var_code(VAR),
            Keys = [[N,VAR]|Keys1]
        ;   Keys = [[N|K]|Keys1]
        ),
        N1 is N+1,
        index_keys(Spec, N1, T, Keys1).

var_code(-0x1227F5A).

% decode the type of error and throw it along with the context of error
% In most cases it is _not_ expected to throw, so do not declare it as "db_error_chk/2 is throwing."
db_error_chk(0, _) :- !.
db_error_chk(ErrorCode, Ctxt) :-
        c_decode_error(ErrorCode, Err),
        Goal = lmdb,                                % FIXME: proper goal
        illarg(system(lmdb(Ctxt,Err)), Goal, 0, 0). % TODO: msgs.pl

must_be_valid_spec_list(SpecList, ErrGoal, ArgNo) :-
        (   valid_spec_list(SpecList) ->
            true
        ;   illarg(domain(list,'database spec'), ErrGoal, ArgNo, SpecList)
        ).

must_be_db(DBRef, DBAddr, ErrGoal, ArgNo) :-
        (   DBRef = '$lmdb'(DBAddr),
            integer(DBAddr),
            '$db_specs'(_, DBAddr, _, _) ->
            true
        ;   illarg(domain(ground,'database reference'), ErrGoal, ArgNo, DBRef)
        ).

must_be_termref(TermRef, ITermRef, ErrGoal, ArgNo) :-
        (   TermRef = '$db_termref'(ITermRef),
            integer(ITermRef) ->
            true
        ;   illarg(domain(ground,'term reference'), ErrGoal, ArgNo, TermRef)
        ).

%@  @item lmdb_property(@var{+DBRef}, @var{-Property})
%@  @PLXindex {lmdb_property/2 (lmdb)}
%@  Unifies @var{Property} with one of the properties of the database given by @var{DBRef}, which are:
%@
%@  @table @code
%@  @item path(@var{P})
%@  @findex path/1 (lmdb property)
%@  The file system path of the database.
%@
%@  @item speclist(@var{L})
%@  @findex speclist/1 (lmdb property)
%@  The @var{SpecList} with which the database was created.
%@
%@  @item mapsize(@var{S})
%@  @findex mapsize/1 (lmdb property)
%@  The mapsize, or size limit, of the database.
%@
%@  @item mode(@var{S})
%@  @findex mode/1 (lmdb option)
%@  The access mode with which the database was opened. One of the atoms @code{read} and @code{write}.
%@  @end table
%@
%@  Enumerates all four properties on backtracking.
:- lmdb_property(+, +) is det.
:- lmdb_property(+, -) is nondet.
lmdb_property(DBRef, Property) :-
        ErrGoal = lmdb_property(DBRef,Property),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        db_property_int(Property, DBAddr).

db_property_int(speclist(SpecList), DBAddr) :-
        db_get_spec(DBAddr, SpecList).
db_property_int(path(Path), DBAddr) :-
        c_property(DBAddr, path, Path, R),
        db_error_chk(R, db_property).
db_property_int(mapsize(MapSize), DBAddr) :-
        c_property(DBAddr, mapsize, MapSize, R),
        db_error_chk(R, db_property).
db_property_int(mode(RW), DBAddr) :-
        c_property(DBAddr, 'rdonly', Flag, R),
        db_error_chk(R, db_property),
        (   Flag = 0 ->
            RW = write
        ;   RW = read
        ).

%@  @item lmdb_export(@var{+DBRef}, @var{+ExportFile})
%@  @PLXindex {lmdb_export/2 (lmdb)}
%@  Exports the database given by @var{DBRef} to the text file
%@  @var{ExportFile}. @var{ExportFile} can be imported by
%@  @code{lmdb_import/2}.
:- lmdb_export/2 is det.
lmdb_export(DBRef, ExportFile) :-
        ErrGoal = lmdb_export(DBRef,ExportFile),
        must_be_db(DBRef, DBAddr, ErrGoal, 1),
        db_get_spec(DBAddr, SpecList),
        export_version(Version),
        open(ExportFile, write, Stream),
        format(Stream, '%% This is an export of the database "~w".\n\n', [DBName]),
        format(Stream, '%% The first term in this file is a property list for the exported database.\n\n', []),
        portray_clause(Stream, properties([database(DBName),
                                           speclist(SpecList),
                                           version(Version)
                                          ])),
        write(Stream, '\n\n'),
        format(Stream, '%% The rest of the file contains the exported terms.\n', []),
        format(Stream, '%% Each term is wrapped like this: "term(<term>)".\n\n', []),
        (   lmdb_enumerate(DBRef, Term, _),
            write_canonical(Stream, term(Term)),
            write(Stream, '.\n'),
            fail
        ;   close(Stream)
        ).

%%% *** IMPORTANT!
%%% This version number should change if the export format is changed.
%%% The version was 1 in library(bdb).
export_version(2).

%@  @item lmdb_import(@var{+DBRef}, @var{+ImportFile})
%@  @PLXindex {lmdb_import/2 (lmdb)}
%@  Imports the text file @var{ImportFile} into the database given by
%@  @var{DBRef}.
%@
%@  The @file{ImportFile} should have been written by @code{lmdb_export/2}
%@  or by the corresponding functionality in @code{library(bdb)}.
:- lmdb_import/2 is det.
lmdb_import(DBRef, ImportFile) :-
        open(ImportFile, read, Stream),
        read(Stream, PropertyTerm),
        import_properties(PropertyTerm, _DBName, _SpecList, Version),
        % Legacy library(bdb) export format (version 1) works as-is currently, and we want to keep import from bdb working, regardles of our own export format/version.
        (   Version == 1 ->
            true
        ;
            import_check_version(Version)
        ),
        repeat,
        read(Stream, ImportTerm),
        import_store(DBRef, ImportTerm),
        ImportTerm == end_of_file,
        !,
        close(Stream).

import_properties(Properties, DBName, SpecList, Version) :-
        nonvar(Properties),
        Properties = properties(PropertyList), !,
        import_get_properties(PropertyList, DBName, SpecList, Version).
import_properties(PropertyTerm, _DBName, _SpecList, _Version) :-
        throw(unexpected_properties_error('First data must be properties',PropertyTerm)).

import_get_properties([], _DBName, _SpecList, _Version).
import_get_properties([Property|PropertyList], DBName, SpecList, Version) :-
        import_property(Property, DBName, SpecList, Version),
        import_get_properties(PropertyList, DBName, SpecList, Version).

import_property(Property, _DBName, _SpecList, _Version) :- var(Property), !,
        true.
import_property(database(DBName1), DBName, _SpecList, _Version) :- !,
        DBName = DBName1.
import_property(speclist(SpecList1), _DBName, SpecList, _Version) :- !,
        SpecList = SpecList1.
import_property(version(Version1), _DBName, _SpecList, Version) :- !,
        Version = Version1.
% Ignore unknown properties
import_property(_Properties, _DBName, _SpecList, _Version).

import_check_version(Version) :-
        (   export_version(Version) ->
            true
        ;   otherwise ->
            throw(wrong_export_version_error(Version))
        ).

import_store(end_of_file, _DBRef) :- !.
import_store(term(Term), DBRef) :- !,
        lmdb_store(DBRef, Term, _TermRef).
import_store(ImportTerm, _DBRef) :-
        throw(unexpected_data_error(ImportTerm)).

foreign(create_db,
        c_create_db(+string, +integer, +integer, +integer, +integer, [-integer])).
foreign(open_db,
        c_open_db(+string, +integer, +integer, +string, -address('db_struct'), [-integer])).
foreign(close_db,
        c_close_db(+address('db_struct'), [-integer])).
foreign(read_spec,
        c_read_spec(+address('db_struct'), -integer/* address(void) */, [-integer])).
foreign(next_termref,
        c_next_termref(+address('db_struct'), -integer, [-integer])).
foreign(store_termref,
        c_store_termref(+address('db_struct'), +integer, +integer, [-integer])).
foreign(store_term,
        c_store_term(+address('db_struct'), +integer, +integer/* address(void) */, +integer, [-integer])).
foreign(fetch_term,
        c_fetch_term(+address('db_struct'), +integer, -integer/* address(void) */, [-integer])).
foreign(delete_term,
        c_delete_term(+address('db_struct'), +integer, [-integer])).
foreign(delete_termref,
        c_delete_termref(+address('db_struct'), +integer, +integer, [-integer])).
foreign(global_iterator,
        c_global_iterator(+address('db_struct'), -address('db_cursor'), [-integer])).
foreign(global_iterator_next,
        c_global_iterator_next(+address('db_struct'), +address('db_cursor'), -integer/* address(void) */, -integer, [-integer])).
foreign(global_iterator_done,
        c_global_iterator_done(+address('db_struct'), +address('db_cursor'), [-integer])).
foreign(term_iterator,
        c_term_iterator(+address('db_struct'), +term, -address('iterator'), [-integer])).
foreign(term_iterator_next,
        c_term_iterator_next(+address('db_struct'), +address('iterator'), -integer/* address(void) */, -integer, [-integer])).
foreign(term_iterator_done,
        c_term_iterator_done(+address('db_struct'), +address('iterator'), [-integer])).
foreign(db_term_hash,
        c_term_hash(+term, [-integer])).
foreign(ixkeys,
        c_index_keys(+term, +term, -term, [-integer])).
foreign(decode_error,
        c_decode_error(+integer, [-string])).
foreign(sync_db,
        c_sync_db(+address('db_struct'), [-integer])).
foreign(property,
        c_property(+address('db_struct'), +string, -term, [-integer])).

foreign_resource(lmdb,
                 [
                     init(db_init),
                     deinit(db_deinit),
                     create_db,
                     open_db,
                     close_db,
                     read_spec,
                     next_termref,
                     store_termref,
                     store_term,
                     fetch_term,
                     delete_termref,
                     delete_term,
                     global_iterator,
                     global_iterator_next,
                     global_iterator_done,
                     term_iterator,
                     term_iterator_next,
                     term_iterator_done,
                     db_term_hash,
                     ixkeys,
                     decode_error,
                     sync_db,
                     property
                 ]).

:- load_foreign_resource(library(system(lmdb))).

%@  @end table
%@
%@  @node LMDB Example Session
%@  @subsection Example Session
%@
%@  This example creates a database, opens it, and add some values to it. Finally the values are read.
%@
%@  @example
%@  @group
%@  | ?- lmdb_create(tempdb, [a(+,-)]).
%@
%@  | ?- lmdb_open(tempdb, R, [mode(write)]), assertz(tempdb(R)).
%@  R = '$lmdb'(4611936272) ?
%@
%@  | ?- tempdb(R), lmdb_store(R, a(b,1), T).
%@  R = '$lmdb'(4611936272),
%@  T = '$db_termref'(0) ?
%@
%@  | ?- tempdb(R), lmdb_store(R, a(c,2), T).
%@  R = '$lmdb'(4611936272),
%@  T = '$db_termref'(1) ?
%@
%@  | ?- tempdb(R), lmdb_store(R, a(b,X), T).
%@  R = '$lmdb'(4611936272),
%@  T = '$db_termref'(2) ?
%@
%@  | ?- tempdb(R), lmdb_enumerate(R, Term, _).
%@  R = '$lmdb'(4611936272),
%@  Term = a(b,1) ? ;
%@  R = '$lmdb'(4611936272),
%@  Term = a(c,2) ? ;
%@  R = '$lmdb'(4611936272),
%@  Term = a(b,_A) ? ;
%@
%@  | ?- tempdb(R), lmdb_findall(R, Term, Term, ground(Term), Bag).
%@  R = '$lmdb'(4611936272),
%@  Bag = [a(b,1),a(c,2)] ?
%@
%@  | ?- retract(tempdb(R)), lmdb_close(R).
%@  R = '$lmdb'(4611936272) ?
%@  @end group
%@  @end example
%@
%@  Later, in another (or the same) process, the stored data can be read.
%@
%@  @example
%@  @group
%@  | ?- lmdb_open(tempdb, R, [mode(read)]), assertz(tempdb(R)).
%@  R = '$lmdb'(4560556048) ?
%@
%@  | ?- tempdb(R), lmdb_findall(R, Term, Term, ground(Term), Bag).
%@  R = '$lmdb'(4560556048),
%@  Bag = [a(b,1),a(c,2)] ?
%@
%@  | ?- retract(tempdb(R)), lmdb_close(R).
%@  R = '$lmdb'(4560556048) ?
%@  @end group
%@  @end example
%@
%@  @node LMDB DB-Spec Details
%@  @subsection DB-Spec---Details
%@
%@  A db-spec has the form of a @var{SpecList}:
%@
%@  @table @var
%@  @item speclist
%@  = @code{[}@var{spec1}, @dots{}, @var{specM}@code{]}
%@
%@  @item spec
%@  = @var{functor}@code{(}@var{argspec1}, @dots{}, @var{argspecN}@code{)}
%@
%@  @item argspec
%@  = @code{+} | @code{-}
%@  @end table
%@  where @var{functor} is a Prolog atom.  The case @var{N} = 0 is
%@  allowed.
%@
%@  A spec @var{F}@code{(}@var{argspec1}, @dots{}, @var{argspecN}@code{)} is
%@  @emph{applicable} to any nonvar term with principal functor
%@  @var{F}/@var{N}.
%@
%@  When storing a term @var{T}, we generate a hash code for every
%@  applicable spec in the db-spec, and a reference to @var{T} is stored
%@  with each of them.  (More precisely, with each element of the set of
%@  generated hash codes).  If @var{T} contains nonvar elements on each
%@  @code{+} position in the spec, then the hash code depends on each of
%@  these elements.  If @var{T} does contain some variables on
%@  @code{+} position, then the hash code depends only on the functor
%@  of @var{T}.
%@
%@  When fetching a term @var{Q}, we look for an applicable spec for
%@  which there are no variables in @var{Q} on positions marked
%@  @code{+}.  If no applicable spec can be found, a domain error is raised.
%@  If no spec can be found where on each @code{+} position a nonvar
%@  term occurs in @var{Q}, an instantiation error is raised.
%@  Otherwise, we choose the spec with the most @code{+} positions,
%@  breaking ties by choosing the leftmost one.
%@
%@  The terms that contain nonvar terms on every @code{+}
%@  position will be looked up using indexing based on the principal
%@  functor of the term and the principal functor of
%@  terms on @code{+} positions.  The other (more general)
%@  terms will be looked up using an indexing based on the principal
%@  functor of the term only.
%@
%@  @node LMDB Export-Import
%@  @subsection Exporting and Importing a Database
%@
%@  Since the database format of LMDB may change from version to
%@  version, it may become necessary to migrate a database when upgrading.
%@  Two predicates are provided for this purpose: @code{lmdb_export/2} and @code{lmdb_import/2}.
%@
%@  To ease migration from the legacy @code{library(bdb)} library, files exported with
%@  @code{bdb:db_export/[2,3]} can be imported into the LMDB format using @code{lmdb_import/2}.
