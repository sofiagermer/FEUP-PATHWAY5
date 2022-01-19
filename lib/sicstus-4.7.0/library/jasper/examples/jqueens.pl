/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(jqueens, [jqueens/2,queens_play/2,main/0]).

:- use_module(library('clpfd/examples/queens')).

:- use_module(library(jasper)).

main :-
        queens_play(8, 0).


jqueens(Size, L) :-
        queens([ff], L, Size).


% Call from prolog toplevel.
queens_play(Size, Nsol) :-
        queens_play(Size, Nsol, []).

queens_play(Size, Nsol, Extra_Jasper_options) :-
        Jasper_options = [classpath([library('jasper/examples')])|Extra_Jasper_options],
        jasper_initialize(Jasper_options, JVM),
        jasper_call(JVM,
		    method('Queens',play,[static]),
		    play(+integer,+integer),
		    play(Size,Nsol)).

%% Not used
:- public queens_play_debug/3.

queens_play_debug(Size, Nsol, Dlevel) :-
        number_codes(Dlevel, L),
        atom_codes(DlevelA, L),
        atom_concat('-Dse.sics.jasper.debugLevel=', DlevelA, Doption),
        queens_play(Size, Nsol, [Doption]).
