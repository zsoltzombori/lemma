:- module(provecd, [verify_cd/2, verify_cd_and_halt/2]).

:- use_module(lemma_problem_location).
:- use_module(swilib(err)).
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(tptp_cd)).
:- use_module(cdtools(dc_representation_cd)).

verify_cd_and_halt(Problem, DTerms) :-
	( verify_cd(Problem, DTerms) ->
	  halt(0)
	; halt(1)
	).
	
verify_cd(Problem, DTerms) :-
	problem_absname(Problem, Problem1),
	tptpcd_problem_install(Problem1, [goal=Goal]),
	( atom(DTerms) ->
	  read_file_to_terms(DTerms, DTerms1, [])
	; var(DTerms) ->
	  err('Bad arguments')
	; DTerms = [] ->
	  DTerms1 = DTerms
	; DTerms = [_|_] ->
	  DTerms1 = DTerms
	; DTerms1 = [DTerms]
	),
	length(DTerms1, Len),
	info(10, 'Verifying ~d proofs of problems ~q', [Len, Problem]),
	Fail = fail(0),
	( member_n(D, N, DTerms1),
	  ( D = (_-_) ->
	    ( dc_to_d(D, D1) ->
	      true
	    ; info(10, 'Proof ~d of ~q: Validation failed - DC- to D-term conversion failed', [N, Problem]),
	      nb_setarg(1, Fail, 1),
	      fail
	    )
	  ; D1 = D
	  ),
	  ( \+ ground(D1) ->
	    info(10, 'Proof ~d of ~q: Validation failed - D-term not ground', [N, Problem]),
	    nb_setarg(1, Fail, 1)
	  ; d_mgt(D1, Goal) ->
	    info(10, 'Proof ~d of ~q: Validation succeeded', [N, Problem])
	  ; info(10, 'Proof ~d of ~q: Validation failed', [N, Problem]),
	    nb_setarg(1, Fail, 1)
	  ),
	  fail
	; true
	),
	Fail = fail(0).

member_n(X, 1, [X|_]).
member_n(X, N, [_|Y]) :-
	member_n(X, N1, Y),
	N is N1+1.

