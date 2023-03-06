:- module(lemgen_topdown,
	  [ gen_tlemma_topdown_tsize/5 ]).
			  
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(tptp_cd)).
:- use_module(cdtools(callutils_cd)).
:- use_module(cdtools(axioms_cd)).

%%%% 
%%%% lemgen_topdown_tsize(+Problem, +Timeout, -DTerm, -FTerm, -Status)
%%%% 
%%%% Enumerate lemmas as D-Term (DTerm) and MGT (FTerm) pairs. If it was
%%%% detected that the problem can be already proven by some generated lemma,
%%%% the proof and its MGT are the single solution with Status proof.
%%%% Otherwise Status is lemma. FTerm is in the format of CDTools (=> for
%%%% implication).
%%%%
%%%% Lemmas are just all encountered subproofs for a naive implementation of
%%%% goal-driven iterative deepening upon tree size. At most one proof per
%%%% MGT is recorded as lemma.
%%%%
%%%% Naive implementation that involves recomputation of MGTs. May be improved
%%%% with techniques used for CCS.
%%%%

:- dynamic seen/1.
:- dynamic seen_d/1.
:- dynamic lemma/2.

gen_tlemma_topdown_tsize(Problem, Timeout, D, F, Status) :-
	retractall( seen(_) ),
	retractall( seen_d(_) ),
	retractall( lemma(_, _) ),
	tptpcd_problem_install(Problem, [goal=Goal]),
	( try_with_time_limit(Timeout,
			      (between(0,100000,I),
			       predicate_property( lemma(_,_),
						   number_of_clauses(N) ),
			       info(10, 'Level: ~w, lemmas: ~w', [I, N]),
			       gen_d_mgt_upto_tsize_for_lemgen(I, D1, Goal))) ->
	  info(10, 'Proof found'),
	  Status = proof,
	  D = D1,
	  d_mgt(D, F)
	; info(10, 'No proof found'),
	  predicate_property( lemma(_,_),
			      number_of_clauses(N1) ),
	  info(10, 'Lemmas: ~w', [N1]),
	  Status = lemma,
	  lemma(D, F)
	).

gen_d_mgt_upto_tsize_for_lemgen(N, D, F) :-
	gen_d_mgt_upto_tsize_for_lemgen_1(N, D, _, F).

gen_d_mgt_upto_tsize_for_lemgen_1(N, I, N, F) :-
	axiom_id(F, I),
	acyclic_term(F).
gen_d_mgt_upto_tsize_for_lemgen_1(N, d(A,B), N1, FY) :-
	N > 0,
	N2 is N - 1,
	gen_d_mgt_upto_tsize_for_lemgen_1(N2, A, N3, (FX=>FY)),
	gen_d_mgt_upto_tsize_for_lemgen_1(N3, B, N1, FX),
	store_lemma(d(A,B)).

store_lemma(D) :-
	variant_sha1(D, HD),
	( seen_d(HD) ->
	  true
	; assertz( seen_d(HD) ),
	  d_mgt(D, F),
	  variant_sha1(F, H),
	  ( seen(H) ->
	    true
	  ; assertz( lemma(D, F) ),
	    assertz( seen(H) )
	  )
	).