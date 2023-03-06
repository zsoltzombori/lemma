:- module(lemma_features,
	  [ configure_lemma_features/1,
	    spo/3,
	    problem_spo/2,
	    proof_spo/7,
	    lemma_spo/5,
	    lemma_utility_spo/2,
	    reset_lemma_features/0,
	    mk_proof_id/6,
	    convert_to_short_ids/0,
	    number_of_incoming_edges/3 ]).

:- use_module(lemma_problem_location).
:- use_module(cdtools(trmutils_cd)).
:- use_module(swilib(err)).
:- use_module(cdtools(tptp_cd)).
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(dc_representation_cd)).
:- use_module(cdtools(compress_cd)).
:- use_module(cdtools(dv_cd)).
:- use_module(cdtools(fterm_conversions_cd)).
:- use_module(cdtools(numutils_cd)).
:- use_module(cdtools(callutils_cd)).
:- use_module(cdtools(named_axioms_cd)).
:- use_module(cdtools(combred_cd)).
:- use_module(cdtools(organic_cd)).
:- use_module(cdtools(grounding_cd)).

:- dynamic spo/3.
:- dynamic d_to_gr_cache/2.
:- dynamic name_cache/2.
:- dynamic lemma_feature/1.
:- dynamic id_table/2.

:- initialization( init_name_cache ).
:- initialization( configure_lemma_features([]) ).

reset_lemma_features :-
	retractall( spo(_, _, _) ),
	retractall( d_to_gr_cache(_, _) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% OIDs
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_proof_id(Problem, RunId, ProofNumber, DC, SrcId, ProofId) :-
	variant_sha1(proof(Problem, RunId, ProofNumber, DC, SrcId), ProofId).

mk_lemma_id(ContextId, DC, Method, LemmaId) :-
	variant_sha1(lemma(ContextId, DC, Method), LemmaId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

configure_lemma_features(Opts) :-
	retractall( lemma_feature(_, _) ),
	( memberchk( include_special_features=Fs, Opts ) -> true ; Fs = [] ),
	findall((lemma_feature(F, F) :- !), member(F, Fs), CFs),
	append(CFs, [(lemma_feature(F, _) :- costly_feature(F), !, fail),
		     lemma_feature(F, F)],
	       CFs1),
	( member(C, CFs1),
	  assertz(C),
	  fail
	; true
	).

costly_feature(lf_hb_compression_ratio_treerepair).
costly_feature(lf_hb_organic).
% costly_feature(lf_hb_compression_ratio_raw_deflate).
% costly_feature(lf_hb_compression_ratio_dag).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Problem
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problem_spo(Problem, spo(Problem1, P, O)) :-
	problem_atomic_name(Problem, Problem1),
	problem_absname(Problem, AbsProblem),
	canonicalize_tptpcd_problem(AbsProblem, TheoremClause, AxiomsMatrix),
	( TheoremClause = [~thm(Goal1)] ->
	  f_to_if(Goal1, Goal)
	; err('Bad theorem clause: ~q', [TheoremClause])
	),
	( map_axiom_clause_to_if(AxiomsMatrix, 1, Axioms) ->
	  true
	; err('Bad axiom clauses: ~q', [AxiomsMatrix])
	),
	( lemma_feature(type, P), O = problem
	; lemma_feature(goal, P), O = Goal
	; lemma_feature(axiom(I), P),
	  member(I-O, Axioms)
	; lemma_feature(number_of_axioms, P),
	  length(Axioms, O)
	).

map_axiom_clause_to_if([X|Xs], N, [N-IF|Xs1]) :-
	( X = [thm(F)] ->
	  f_to_if(F, IF)
	; err('Bad axiom clause: ~q', [X])
	),
	N1 is N+1,
	map_axiom_clause_to_if(Xs, N1, Xs1).
map_axiom_clause_to_if([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Proof
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proof_spo(S, Problem, SourceId, DTerm, MetaInfo, spo(S, P, O), Opts) :-
	( lemma_feature(type, P), O = proof
	; lemma_feature(problem, P), problem_atomic_name(Problem, O)
	; lemma_feature(source, P), O = SourceId
	; lemma_feature(dcterm, P), d_to_dc(DTerm, O)
	; ( memberchk(no_dterm, Opts) -> fail ; lemma_feature(dterm, P), O = DTerm )
	; lemma_feature(meta_info, P), O = MetaInfo
	; lemma_feature(d_tsize, P), d_tsize(DTerm, O)
	; lemma_feature(d_height, P), d_height(DTerm, O)
	; lemma_feature(d_csize, P), d_csize(DTerm, O)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Axioms are assumed to be installed when this is called.
%%%% 
lemma_spo(D, Method, LemmaContextOpts, spo(S, P, O), Opts) :-
	d_to_dc(D, DC),
	( memberchk(proof=ContextId, LemmaContextOpts) ->
	  true
	; ContextId = 'no_context'
	),
	mk_lemma_id(ContextId, DC, Method, S),
	copy_term(D, D1),
	lemma_formula(D1, lemma(H, B), FHash, FArrowSyntax),
	( lemma_feature(type, P), O = lemma
	; ( memberchk(problem=Problem,  LemmaContextOpts) ->
	    lemma_feature(problem, P),
	    problem_atomic_name(Problem, O)
	  ; fail
	  )
	; ( memberchk(proof=ProofId1,  LemmaContextOpts) ->
	    lemma_feature(lf_proof, P), O = ProofId1
	  ; fail
	  )
	; ( memberchk(is_not_in_proof, LemmaContextOpts) ->
	    lemma_feature(lf_is_in_proof, P), O = 0
	  ; ( lemma_feature(lf_is_in_proof, P), O = 1
	    ; ( memberchk(proof=ProofId1,  LemmaContextOpts) ->
		lemma_feature(lfp_containing_proof, P), O = ProofId1
	      ; fail
	      )
	    )
	  )
	; lemma_feature(formula, P), O = lemma(H,B)
	; lemma_feature(lf_hb_name, P),
	  %% no name: to end of alphabet for sorting
	  ( name_cache(FHash, O) -> true ; O = zzz )
	; lemma_feature(lf_hb_name_status, P),
	  ( name_cache(FHash, Name) ->
	    ( atom_prefix(Name, meta_) ->
	      O = 1
	    ; O = 0
	    )
	  ; O = 2
	  )
	; lemma_feature(dcterm, P), O = DC
	; ( memberchk(no_dterm, Opts) -> fail ; lemma_feature(dterm, P), O = D )
	; lemma_feature(method, P), O = Method
	; lemma_feature(lf_b_length, P), length(B, O)
	; lemma_spo_d_features(D, P, O, Opts)
	; ( memberchk(proof_d=ProofD, LemmaContextOpts) ->
	    lemma_spo_d_context_related_features(D, ProofD, P, O, Opts)
	  ; fail
	  )
	; ( memberchk(goal=Goal, LemmaContextOpts) ->
	    lemma_spo_hb_context_related_features([H|B], Goal, P, O, Opts)
	  ; fail
	  )
	; lemma_spo_h_features(H, P, O, Opts)
	; lemma_spo_b_features(B, P, O, Opts)
	; lemma_spo_hb_features([H|B], P, O, Opts)
	; lemma_spo_hb_further_features(FArrowSyntax, P, O, Opts)
	).

lemma_spo_d_features(D, P, O, _Opts) :-
	d_csize(D, CS),
	( ground(D) -> GCS = CS ; d_var_to_0(D, D0), d_csize(D0, GCS) ),	
	( lemma_feature(lf_d_tsize, P), d_tsize(D, O)
	; lemma_feature(lf_d_height, P), d_height(D, O)
	; lemma_feature(lf_d_csize, P), O = CS
	; lemma_feature(lf_d_grd_csize, P), O = GCS
	; lemma_feature(lf_d_major_minor_relation, P),
	  ( ground(D), D = d(D1,D2) ->
	    ( D1 = D2 -> O = 0
	    ; sub_term(D2, D1) -> O = 1
	    ; sub_term(D1, D2) -> O = 2
	    ; O = 3
	    )
	  ; atomic(D) -> O=0
	  ; O = 4
	  )
	; lemma_feature(lf_d_number_of_terminals, P),
	  d_number_of_terminals(D, O)
	).

lemma_spo_d_context_related_features(D, ProofD, P, O, Opts) :-
	( lemma_feature(lfp_d_occs, P), occurrences_of_term(D, ProofD, O)	
	; lemma_feature(lfp_d_occs_outermost_matches, P),
	  number_of_outermost_matches(ProofD, D, O)
	; lemma_feature(lfp_d_occs_innermost_matches, P),
	  number_of_innermost_matches(ProofD, D, O)
	; lemma_feature(lfp_d_min_goal_dist, P),
	  min_depth_of_match(ProofD, D, O)
	; lemma_feature(lfp_d_incoming, P),
	  ( memberchk(lfp_d_incoming=Incoming, Opts), \+ var(Incoming) ->
	    O = Incoming
	  ; ground(D) ->
	    number_of_incoming_edges(ProofD, D, O)
	  ; O=0
	  )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d_number_of_terminals(D, N) :-
	nsols((sub_term(X, D),
	       \+ var(X),
	       X = d(A,B),
	       ( atomic(A) -> true ; var(A) ),
	       ( atomic(B) -> true ; var(B) )
	       ),
	      N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemma_spo_hb_context_related_features(HB, Goal, P, O, _Opts) :-
	( lemma_feature(lf_hb_nongoal_symbol_occs, P),
	  local_trm_signature(Goal, GoalSig),
	  trms_signature_multiset(HB, S),
	  nsols((member(X, S), \+ memberchk(X, GoalSig)), O)
	; HB = [H|_],
	  goal_and_term_subterm_comparison(Goal, H, N, M),
	  ( lemma_feature(lf_h_excluded_goal_subterms, P),
	    O = N
	  ; lemma_feature(lf_h_subterms_not_in_goal, P),
	    O = M
	  )
	).

goal_and_term_subterm_comparison(Goal, F, N, M) :-
	ungrd_atomics(Goal, Goal1, [falsehood]),
	setof(X, Y^(sub_term(Y, Goal1), variant_sha1(Y,X)), SGoal),
	setof(X, Y^(sub_term(Y, F), variant_sha1(Y,X)), SF),
	ord_subtract(SGoal, SF, SRest),
	ord_subtract(SF, SGoal, GRest),
	length(SRest, N),
	length(GRest, M).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemma_spo_h_features(H, P, O, _) :-
	( lemma_feature(lf_h_tsize, P), trm_tsize(H, O)
	; lemma_feature(lf_h_height, P), trm_height(H, O)
	; lemma_feature(lf_h_csize, P), trm_csize(H, O)
	; lemma_feature(lf_h_distinct_vars, P), trm_distinct_vars(H, O)
	; lemma_feature(lf_h_var_occs, P), trm_var_occs(H, O)
	; lemma_feature(lf_h_occs_of_most_frequent_var, P),
	  trm_occs_of_most_frequent_var(H, O)
	; lemma_feature(lf_h_const_occs, P), trms_const_occs([H], O)
	; lemma_feature(lf_h_occs_of_most_frequent_const, P),
	  trms_occs_of_most_frequent_const([H], O)
	; lemma_feature(lf_h_fun_occs, P), trms_fun_occs([H], O)
	; lemma_feature(lf_h_occs_of_most_frequent_fun, P),
	  trms_occs_of_most_frequent_fun([H], O)
	).

lemma_spo_b_features(B, P, O, _) :-
	( lemma_feature(lf_b_tsize, P), trms_tsize(B, O)
	; lemma_feature(lf_b_height, P), trms_height(B, O)
	; lemma_feature(lf_b_csize, P), trms_csize(B, O)
	; lemma_feature(lf_b_distinct_vars, P), trm_distinct_vars(B, O)
	; lemma_feature(lf_b_var_occs, P), trm_var_occs(B, O)
	; lemma_feature(lf_b_occs_of_most_frequent_var, P),
	  trm_occs_of_most_frequent_var(B, O)
	; lemma_feature(lf_b_const_occs, P), trms_const_occs(B, O)
	; lemma_feature(lf_b_occs_of_most_frequent_const, P),
	  trms_occs_of_most_frequent_const(B, O)
	; lemma_feature(lf_b_fun_occs, P), trms_fun_occs(B, O)
	; lemma_feature(lf_b_occs_of_most_frequent_fun, P),
	  trms_occs_of_most_frequent_fun(B, O)
	).

lemma_spo_hb_features(HB, P, O, Opts) :-
	( lemma_feature(lf_hb_tsize, P), trms_tsize(HB, O)
	; lemma_feature(lf_hb_height, P), trms_height(HB, O)
	; lemma_feature(lf_hb_csize, P), trms_csize(HB, O)
	; lemma_feature(lf_hb_distinct_vars, P), trm_distinct_vars(HB, O)
	; lemma_feature(lf_hb_var_occs, P), trm_var_occs(HB, O)
	; lemma_feature(lf_hb_occs_of_most_frequent_var, P),
	  trm_occs_of_most_frequent_var(HB, O)
	; lemma_feature(lf_hb_const_occs, P), trms_const_occs(HB, O)
	; lemma_feature(lf_hb_occs_of_most_frequent_const, P),
	  trms_occs_of_most_frequent_const(HB, O)
	; lemma_feature(lf_hb_fun_occs, P), trms_fun_occs(HB, O)
	; lemma_feature(lf_hb_occs_of_most_frequent_fun, P),
	  trms_occs_of_most_frequent_fun(HB, O)
	; HB = [H|B],
	  term_variables(H, VH),
	  term_variables(B, VB),
	  sort(VH, VH1),
	  sort(VB, VB1),
	  ( lemma_feature(lf_hb_distinct_hb_shared_vars, P),
	    ord_intersection(VH1, VB1, V),
	    length(V, O)
	  ; lemma_feature(lf_hb_distinct_h_only_vars, P),
	    ord_subtract(VH1, VB1, V),
	    length(V, O)
	  ; lemma_feature(lf_hb_distinct_b_only_vars, P),
	    ord_subtract(VB1, VH1, V),
	    length(V, O)
	  )
	; lemma_feature(lf_hb_singletons, P),
	  term_variables(HB, Vars),
	  nsols(( member(Var, Vars),
		  occurrences_of_var(Var, HB, 1)),
		O)
	; lemma_feature(lf_hb_double_negation_occs, P),
	  trms_double_negation_occs(HB, O)
	; ( memberchk(fast, Opts) ->
	    fail
	  ; hb_to_implication(HB, C),
	    ( lemma_feature(lf_hb_compression_ratio_raw_deflate, P),
	      trm_compression_ratio_raw_deflate(C, O)
	    ; lemma_feature(lf_hb_compression_ratio_treerepair, P),
	      trm_compression_ratio_treerepair(C, O)
	    ; lemma_feature(lf_hb_compression_ratio_dag, P),
	      trm_compression_ratio_dag(C, O)
	    )
	  )
	).

lemma_spo_hb_further_features(_, _, _, Opts) :-
	memberchk(fast, Opts),
	!,
	fail.
lemma_spo_hb_further_features(F, P, O, _Opts) :-
	( lemma_feature(lf_hb_organic, P),
	  ( f_is_organic(F) -> O = 0
	  ; f_is_weakly_organic(F) -> O = 1
	  ; O = 2
	  )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trm_distinct_vars(T, N) :-
	term_variables(T, V), length(V, N).

trm_var_occs(T, N) :-
	nsols((sub_term(X, T), var(X)), N).

trm_occs_of_most_frequent_var(T, N) :-
	  term_variables(T, Vs),
	  ( Vs = [] ->
	    N = 0
	  ; findall(N1, (member(V1, Vs), occurrences_of_var(V1, T, N1)), NV),
	    numbers_maximum(NV, N)
	  ).

trms_const_occs(Ts, N) :-
	nsols((member(T, Ts), sub_term(X, T), atomic(X)), N).

trms_occs_of_most_frequent_const(Ts, N) :-
	findall(V, (member(T, Ts), sub_term(V, T), atomic(V)), Vs1),
	( Vs1 = [] ->
	  N = 0
	; sort(Vs1, Vs2),
	  findall(N1, (member(V1, Vs2), occurrences_of_var(V1, Ts, N1)), NV),
	  numbers_maximum(NV, N)
	).

trms_fun_occs(Ts, N) :-
	nsols((member(T, Ts), sub_term(X, T), compound(X)), N).

local_trm_signature(T, S) :-
	trms_signature_multiset([T], S1),
	sort(S1, S).

trms_signature_multiset(Ts, S) :-
	findall(F/N, ( member(T, Ts),
		       sub_term(X, T),
		       ( atomic(X) ->
			 X=F, N=0
		       ; compound(X) ->
			 functor(X, F, N)
		       )
		     ),
		S).

trms_double_negation_occs(Ts, N) :-
	nsols(( member(T, Ts),
		sub_term(X, T),
		%% repeatedly counts overlapping occs, perhaps sufficient for
		%% our heuristic purposes
		subsumes_term(n(n(_)), X) ),
	      N).

trms_occs_of_most_frequent_fun(Ts, N) :-
	findall(F-A, (member(T, Ts),
		      sub_term(V, T),
		      compound(V),
		      functor(V, F, A)),
		Vs1),
	( Vs1 = [] ->
	  N = 0
	; sort(Vs1, Vs2),
	  findall(N1, (member(F-A, Vs2),
		       functor(X, F, A),
		       nsols((member(T, Ts),
			      sub_term(Y, T),
			      subsumes_term(X, Y)),
			     N1)),
		  NV),
	  numbers_maximum(NV, N)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hb_to_implication([H|B], C) :-
	hb_to_implication_1(B, H, C).

hb_to_implication_1([], H, H).
hb_to_implication_1([X|B], H, i(X,C)) :-
	hb_to_implication_1(B, H, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trm_compression_ratio_dag(T, V) :-
	trm_csize(T, SC),
	trm_tsize(T, ST),
	( ST = 0 ->
	  V = 1
	; V is SC/max(ST,SC)
	).

%%%% 
%%%% Note Thu Jan 12 15:15:55 2023
%%%% 
%%%% Not sure how solid this value is. It seems to depend on whether terms
%%%% happen to be internally shared in Prolog via bindings to a multiply
%%%% occurring variable.
%%%% 
trm_compression_ratio_raw_deflate(T, V) :-
	broadcopy_term(T, T1),
	fast_term_serialized(T1, A),
	string_length(A, L1),
	( L1 = 0 ->
	  V = 1
	; new_memory_file(F),
	  open_memory_file(F, write, S),
	  zopen(S, ZS, [format(raw_deflate), level(9)]),
	  write(ZS, A),
	  close(ZS),
	  size_memory_file(F, L2),
	  V is L2/max(L1,L2)
	).

broadcopy_term(T, T1) :-
	compound(T),
	!,
	T =.. [F|Ts],
	map_broadcopy_term(Ts, Ts1),
	T1 =.. [F|Ts1].
broadcopy_term(X, X).

map_broadcopy_term([X|Xs], [X1|Xs1]) :-
	broadcopy_term(X, X1),
	map_broadcopy_term(Xs, Xs1).
map_broadcopy_term([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trm_compression_ratio_treerepair(T, V) :-
	%%
	%% This implementation uses the tree-repair interface for D-terms. For
	%% this purpose it translates the given term into a similar D-term
	%% (binary tree). For this translation only function symbols with
	%% arity up to 2 are supported. In the translation symbols with arity
	%% 2 are not distinguished. Up to 16 distinct symbols with arity 0
	%% (constants, variables) and 1 are distinguished. Terms may have more
	%% such symbols, but then some of these are translated into the same
	%% symbol.
	%%
	%% Calls the external treerepair program and is thus slow.
	%%
	trm_translate_to_d(T, D),
	d_tsize(D, TS),
	( TS = 0 ->
	  V = 1
	; d_to_gr_cached(D, G),
	  gr_size(G, GS),
	  V is GS/max((TS*2),GS)
	).


d_to_gr_cached(D, G) :-
	variant_sha1(D, DH),
	( d_to_gr_cache(DH, G) ->
	  true
	; d_to_gr(D, G),
	  assertz(d_to_gr_cache(DH, G))
	).

tab_get(Z, X, N, N) :-
	var(Z),
	!,
	( N < 15 ->
	  Z = [X|_]
	; true
	).
tab_get([Y|_], X, N, N) :-
	X == Y,
	!.
tab_get([_|Y], X, N, N1) :-
	N2 is N+1,
	tab_get(Y, X, N2, N1).

trm_translate_to_d(T, D) :-
	trm_translate_to_d(T, _, D).

trm_translate_to_d(X, Tab, Y) :-
	var(X),
	!,
	tab_get(Tab, X, 1, Y).
trm_translate_to_d(X, Tab, d(A,B)) :-
	X =.. [_,A1,B1],
	!,
	trm_translate_to_d(A1, Tab, A),
	trm_translate_to_d(B1, Tab, B).
trm_translate_to_d(X, Tab, d(A,B)) :-
	X =.. [F,B1],
	!,
	tab_get(Tab, F, 1, A),
	trm_translate_to_d(B1, Tab, B).
trm_translate_to_d(X, Tab, Y) :-
	tab_get(Tab, X, 1, Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d_var_to_0(D, D1) :-
	copy_term(D, D1),
	term_variables(D1, Vs),
	bind_vars_to_value(Vs, 0).

bind_vars_to_value([X|Xs], X) :-
	bind_vars_to_value(Xs, X).
bind_vars_to_value([], _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Assumes that the axioms are installed when called.
%%%%
%%%% Returns two further values: variant_sha1 value and the formula in the
%%%% usual format of CDTools for testing the organic property (for D-terms
%%%% with variables =>/2 is also used for meta-level implication).
%%%%
lemma_formula(D, F, FH, H1) :-
	ground(D),
	!,
	d_mgt(D, H1),
	f_to_if(H1, H),
	F = lemma(H, []),
	variant_sha1(F, FH).
lemma_formula(D, F, FH, U) :-
	copy_term(D, D1),
	term_variables(D1, B1),
	dv_mgt(D1, H1),
	f_to_if(H1, H),
	map_f_to_if(B1, B),
	F = lemma(H, B),
 	variant_sha1(lemma(H,B), FH),
 	f_vars_to_ante(B1, H1, U).

map_f_to_if([X|Xs], [X1|Xs1]) :-
	f_to_if(X, X1),
	map_f_to_if(Xs, Xs1).
map_f_to_if([], []).

f_vars_to_ante([], X, X) :-
	!.
f_vars_to_ante([X|Y], Z, (X=>U)) :-
	f_vars_to_ante(Y, Z, U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trms_csize(Fs, S) :-
	( Fs = [] ->
	  S=0
	; T =.. [t|Fs],
	  trm_csize(T, S1),
	  S is S1-1
	).
trms_tsize(Fs, S) :-
	( Fs = [] ->
	  S=0
	; T =.. ['$t'|Fs],
	  trm_tsize(T, S1),
	  S is S1-1
	).
trms_height(Fs, S) :-
	( Fs = [] ->
	  S=0
	; T =.. [t|Fs],
	  trm_height(T, S1),
	  S is S1-1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

number_of_incoming_edges(D, D1, N) :-
	%%
	%% works only for non-atomic D1
	%%
	%% Probably does not yield something meaningful for nonground D1
	%% (s-lemmas). Can however be invoked with combinatory-compressed
	%% d-terms (see implementation of gen_lemma_treerepair/3).
	%%
	\+ atomic(D1),
	term_factorized(D, D2, Ds2),
	VMap = [_=D2|Ds2],
	copy_term(VMap, VMap1),
	map_bind(VMap1),
	map_factor_map(VMap, VMap1, FMap),
	( memberchk(X=D1, FMap) ->
	  occurrences_of_var(X, VMap, N1),
	  ( member(Y=_, VMap), Y == X ->
	    N is N1-1
	  ; N = N1
	  )
	; N = 1
	).

map_bind([X=X|Y]) :- map_bind(Y).
map_bind([]).

map_factor_map([X=_|Y], [_=D|Z], [X=D|U]) :-
	map_factor_map(Y, Z, U).
map_factor_map([], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary predicates that may be useful on the result data
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Conversion of the lemma formula to a first-order Horn clause.
%%%% 
lemma_formula_to_clause(lemma(H, B), [thm(H)|B1]) :-
	map_wrap_terms_with_not_thm(B, B1).

map_wrap_terms_with_not_thm([X|Xs], [~(thm(X))|Xs1]) :-
	map_wrap_terms_with_not_thm(Xs, Xs1).
map_wrap_terms_with_not_thm([], []).


%%%% 
%%%% Conversion between D-terms and DC-terms (DAG-compressed representation of
%%%% D-term). Prefixed with local_ to distinguish from d_to_dc/2 and dc_to_d/2
%%%% in module dc_representation_cd.pl in case CD Tools is loaded. The
%%%% respective predicates from that module can be used for the same purposes.
%%%% 
local_d_to_dc(D, A-B) :-
	term_factorized(D, A, B).

local_dc_to_d(A-B, D) :-
	( B = [] -> D = A
	; copy_term(A-B, D-B1),
	  local_map_bind(B1)
	).

local_map_bind([X=X|Y]) :- local_map_bind(Y).
local_map_bind([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Utility Values
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Values are normalized, between 0 and 1, with smaller values indicating
%%%% better utility.
%%%%
%%%% Operates on loaded spo/3 facts for proofs and lemmas.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        
%%%% Note: the formula below does not work for s-lemmas because their
%%%% occurrences may overlap such that NumOfOccurrences*LemmaSize is larger
%%%% than ProofSize.
%%%%
%%%% (proof_size - (lf_d_occs * lf_d_tsize)) / proof_size
%%%% 
%%%% Hence, for now calculations for t-lemmas are based on 
%%%% subst_innermost_matches/4

%%%% 
%%%% Note: It is not clear whether a large size (height, treesize) of a lemma
%%%% proof is to be considered as something good (high utility value):
%%%% 
%%%% - the larger is more difficult to get, hence more valuable
%%%% - the larger is more distanced from the axioms, hence less valuable
%%%% 
%%%% Similar with distance from/to goal.
%%%%

lemma_utility_spo(L, spo(L, U, O)) :-
	member(U, [ u_tsize_reduction,
		    u_height_reduction,
		    u_csize_reduction,
		    u_tsize_reduction_subst1,
		    u_height_reduction_subst1,
		    u_csize_reduction_subst1,
		    u_tsize_reduction_subst2,
		    u_height_reduction_subst2,
		    u_csize_reduction_subst2,
		    u_occs,
		    u_incoming,
		    u_close_to_goal_path,
		    u_close_to_axioms_height,
		    u_close_to_axioms_tsize ]),
	u_value(U, L, O).

u_value(u_tsize_reduction, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_innermost_exhaustively(DP, DL, DP1),
	d_tsize(DP, PS),
	trm_tsize_ignorespecial(DP1, PS1),
	V is PS1 / PS.
u_value(u_height_reduction, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_innermost_exhaustively(DP, DL, DP1),
	d_height(DP, PS),
	trm_height_ignorespecial(DP1, PS1),
	V is PS1 / PS.
u_value(u_csize_reduction, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_innermost_exhaustively(DP, DL, DP1),
	d_csize(DP, PS),
	trm_csize_ignorespecial(DP1, PS1),
	V is min(PS1,PS) / PS.
u_value(u_tsize_reduction_subst1, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_innermost(DP, DL, DP1),
	d_tsize(DP, PS),
	trm_tsize_ignorespecial(DP1, PS1),
	V is PS1 / PS.
u_value(u_height_reduction_subst1, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_innermost(DP, DL, DP1),
	d_height(DP, PS),
	trm_height_ignorespecial(DP1, PS1),
	V is PS1 / PS.
u_value(u_csize_reduction_subst1, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_innermost(DP, DL, DP1),
	d_csize(DP, PS),
	trm_csize_ignorespecial(DP1, PS1),
	V is min(PS1,PS) / PS.
u_value(u_tsize_reduction_subst2, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_outermost(DP, DL, DP1),
	d_tsize(DP, PS),
	trm_tsize_ignorespecial(DP1, PS1),
	V is PS1 / PS.
u_value(u_height_reduction_subst2, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_outermost(DP, DL, DP1),
	d_height(DP, PS),
	trm_height_ignorespecial(DP1, PS1),
	V is PS1 / PS.
u_value(u_csize_reduction_subst2, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, dcterm, DCP),
	spo(L, dcterm, DCL),
	dc_to_d(DCP, DP),
	dc_to_d(DCL, DL),
	subst_lemma_outermost(DP, DL, DP1),
	d_csize(DP, PS),
	trm_csize_ignorespecial(DP1, PS1),
	V is min(PS1,PS) / PS.
u_value(u_occs, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	findall(N,
		( spo(L1, lfp_containing_proof, P),
		  spo(L1, lfp_d_occs_innermost_matches, N)),
		Ns),
	numbers_maximum(Ns, MaxOccs),
	spo(L, lfp_d_occs_innermost_matches, NL),
	V is (MaxOccs - NL) / MaxOccs.
u_value(u_incoming, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	findall(N, (spo(L1, lfp_containing_proof, P), spo(L1, lfp_d_incoming, N)), Ns),
	numbers_maximum(Ns, MaxIncoming),
	spo(L, lfp_d_incoming, NL),
	V is (MaxIncoming - NL) / MaxIncoming.
u_value(u_close_to_goal_path, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, d_height, HP),
	spo(L, lfp_d_min_goal_dist, HL),
	V is HL / HP.
u_value(u_close_to_axioms_height, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, d_height, HP),
	spo(L, lf_d_height, HL),
	V is HL / HP.
u_value(u_close_to_axioms_tsize, L, V) :-
	!,
	spo(L, lfp_containing_proof, P),
	spo(P, d_tsize, TP),
	spo(L, lf_d_tsize, TL),
	V is TL / TP.

subst_lemma_innermost(D, Lemma, D1) :-
	( ground(Lemma) ->
	  local_subst(D, Lemma, 0, D1)
	; term_variables(Lemma, V),
	  ( V = [X] ->
	    Replacement = X
	  ; Replacement =.. ['$t'|V]
	  ),
	  subst_innermost_matches(D, Lemma, Replacement, D1)
	).

subst_lemma_innermost_exhaustively(D, Lemma, D1) :-
	( ground(Lemma) ->
	  local_subst(D, Lemma, 0, D1)
	; term_variables(Lemma, V),
	  ( V = [X] ->
	    Replacement = X
	  ; Replacement =.. ['$t'|V]
	  ),
	  subst_innermost_matches_exhaustively(D, Lemma, Replacement, D1)
	).

subst_lemma_outermost(D, Lemma, D1) :-
	( ground(Lemma) ->
	  local_subst(D, Lemma, 0, D1)
	; term_variables(Lemma, V),
	  ( V = [X] ->
	    Replacement = X
	  ; Replacement =.. ['$t'|V]
	  ),
	  subst_outermost_matches(D, Lemma, Replacement, D1)
	).

local_subst(T, X, Y, Y) :-
        T == X,
        !.
local_subst(T, _, _, T) :-
        var(T),
        !.
local_subst(T, _, _, T) :-
        atomic(T),
        !.
local_subst(T, X, Y, T1) :-
        compound(T),
        T =.. [F|Ts],
        map_local_subst(Ts, X, Y, Ts1),
        T1 =.. [F|Ts1].

map_local_subst([X|Xs], Y1, Y2, [X1|Xs1]) :-
        local_subst(X, Y1, Y2, X1),
        map_local_subst(Xs, Y1, Y2, Xs1).
map_local_subst([], _, _, []).

%%%%
%%%% Replacement of matches left-right-depth-first ordering.
%%%%
subst_outermost_matches(T, X, Y, Y1) :-
        \+ \+ (T = X),
        !,
	copy_term(X-Y, T-Y1).
subst_outermost_matches(T, _, _, T) :-
        var(T),
        !.
subst_outermost_matches(T, _, _, T) :-
        atomic(T),
        !.
subst_outermost_matches(T, X, Y, T1) :-
        compound(T),
        T =.. [F|Ts],
        map_subst_outermost_matches(Ts, X, Y, Ts1),
        T1 =.. [F|Ts1].

map_subst_outermost_matches([X|Xs], Y1, Y2, [X1|Xs1]) :-
        subst_outermost_matches(X, Y1, Y2, X1),
        map_subst_outermost_matches(Xs, Y1, Y2, Xs1).
map_subst_outermost_matches([], _, _, []).

%%%%
%%%% Replacement of matches left-right-depth-first ordering in case no strict
%%%% subterm matches too.
%%%%
subst_innermost_matches(T, X, Y, Y1) :-
        \+ \+ (T = X),
	\+ ( sub_term(X, T), X \== T ),
        !,
	copy_term(X-Y, T-Y1).
subst_innermost_matches(T, _, _, T) :-
        var(T),
        !.
subst_innermost_matches(T, _, _, T) :-
        atomic(T),
        !.
subst_innermost_matches(T, X, Y, T1) :-
        compound(T),
        T =.. [F|Ts],
        map_subst_innermost_matches(Ts, X, Y, Ts1),
        T1 =.. [F|Ts1].

subst_innermost_matches_exhaustively(T, X, Y, Y1) :-
	subst_innermost_matches(T, X, Y, Y2),
	( Y2 == T ->
	  Y1 = Y2
	; subst_innermost_matches_exhaustively(Y2, X, Y, Y1)
	).

map_subst_innermost_matches([X|Xs], Y1, Y2, [X1|Xs1]) :-
        subst_innermost_matches(X, Y1, Y2, X1),
        map_subst_innermost_matches(Xs, Y1, Y2, Xs1).
map_subst_innermost_matches([], _, _, []).

number_of_outermost_matches(T, T1, N) :-
	number_of_outermost_matches(T, T1, 0, N).

number_of_outermost_matches(T, X, N, N1) :-
        \+ \+ (T = X),
	!,
        N1 is N+1.
number_of_outermost_matches(T, _, N, N) :-
        var(T),
        !.
number_of_outermost_matches(T, _, N, N) :-
        atomic(T),
        !.
number_of_outermost_matches(T, X, N, N1) :-
        compound(T),
        T =.. [_|Ts],
        map_number_of_outermost_matches(Ts, X, N, N1).

map_number_of_outermost_matches([X|Xs], Y1, N, N1) :-
        number_of_outermost_matches(X, Y1, N, N2),
        map_number_of_outermost_matches(Xs, Y1, N2, N1).
map_number_of_outermost_matches([], _, N, N).

number_of_innermost_matches(T, T1, N) :-
	number_of_innermost_matches(T, T1, 0, N).

number_of_innermost_matches(T, X, N, N1) :-
        \+ \+ (T = X),
	\+ ( sub_term(X, T), X \== T ),
	!,
        N1 is N+1.
number_of_innermost_matches(T, _, N, N) :-
        var(T),
        !.
number_of_innermost_matches(T, _, N, N) :-
        atomic(T),
        !.
number_of_innermost_matches(T, X, N, N1) :-
        compound(T),
        T =.. [_|Ts],
        map_number_of_innermost_matches(Ts, X, N, N1).

map_number_of_innermost_matches([X|Xs], Y1, N, N1) :-
        number_of_innermost_matches(X, Y1, N, N2),
        map_number_of_innermost_matches(Xs, Y1, N2, N1).
map_number_of_innermost_matches([], _, N, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Used for shortest path of occurrence (match) to goal
%%%%
min_depth_of_match(T, T1, N) :-
	trm_height(T, H),
	( between(0, H, N),
	  match_at_depth(T, T1, N) ->
	  true
	; N = H
	).
	
match_at_depth(T, T1, _) :-
	\+ \+ (T = T1),
	!.
match_at_depth(T, T1, N) :-
	N > 0,
	N1 is N-1,
	T =.. [_|Ts],
	member(T2, Ts),
	match_at_depth(T2, T1, N1),
	!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Versions where functions starting with '$' are not counted
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trm_tsize_ignorespecial(T, N) :-
	trm_tsize_1_ignorespecial(T, 0, N1),
	N1 = N.

trm_tsize_1_ignorespecial(X, N, N1) :-
	( atomic(X) ; var(X) ),
	!,
	N1 = N.
trm_tsize_1_ignorespecial(T, N, N1) :-
	T =.. [F|Ts],
	( atom_prefix(F, '$') ->
	  N2 is N
	; N2 is 1+N
	),
	map_trm_tsize_1_ignorespecial(Ts, N2, N1).

map_trm_tsize_1_ignorespecial([X|Xs], N, N1) :-
	trm_tsize_1_ignorespecial(X, N, N2),
	map_trm_tsize_1_ignorespecial(Xs, N2, N1).
map_trm_tsize_1_ignorespecial([], N, N).


trm_height_ignorespecial(X, N) :-
	( atomic(X) ; var(X) ),
	!,
	N = 0.
trm_height_ignorespecial(T, N) :-
 	T =.. [F|Ts],
	trm_height_1_ignorespecial(Ts, 0, N1),
	( atom_prefix(F, '$') ->
	  N = N1
	; N is N1+1
	).

trm_height_1_ignorespecial([T|Ts], H, N) :-
	trm_height_ignorespecial(T, H1),
	H2 is max(H, H1),
	trm_height_1_ignorespecial(Ts, H2, N).
trm_height_1_ignorespecial([], H, H).

trm_csize_ignorespecial(T, N) :-
	term_factorized(T, T0, F),
	trm_tsize_ignorespecial(T0, N1),
	trm_csize_1_ignorespecial(F, N1, N2),
	N = N2.

trm_csize_1_ignorespecial([_=T|F], N, N1) :-
	trm_tsize_1_ignorespecial(T, N, N2),
	trm_csize_1_ignorespecial(F, N2, N1).
trm_csize_1_ignorespecial([], N, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_name_cache :-
	retractall( name_cache(_,_) ),
	( ( combinator(N),
	    named_axiom(N, F)
	  ; named_axiom(N, F)
	  ),
	  form_to_lemmaform(F, L),
	  variant_sha1(L, LH),
	  form_to_metaform(F, M),
	  variant_sha1(M, MH),
	  atom_concat('meta_', N, MetaN),
	  ( name_cache(LH, _) ->
	    true
	  ; assertz(name_cache(LH, N))
	  ),
	  ( name_cache(MH, _) ->
	    true
	  ; assertz(name_cache(MH, MetaN))
	  ),
	  fail
	; true
	).

form_to_lemmaform(F, lemma(H, [])) :-
	f_to_if(F, H).

form_to_metaform(F, lemma(H, B)) :-
	form_to_hb(F, H1, B1),
	f_to_if(H1, H),
	map_f_to_if(B1, B).
	
%%%%
%%%% (A => (B => (C => D))) TO [A,B,C] and D
%%%%
form_to_hb(X, Y, []) :-
	var(X),
	!,
	Y = X.
form_to_hb((X => Y), H, [X|B]) :-
	!,
	form_to_hb(Y, H, B).
form_to_hb(X, X, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_to_short_ids :-
	info(10, 'Converting to short IDs'),
	flag(gen_id, _, 1),
	retractall( id_table(_, _) ),
	( spo(I, _, _),
	  atom(I),
	  atom_length(I, 40),
	  ( id_table(I, _) ->
	    true
	  ; flag(gen_id, GI, 1+GI),
	    atom_concat(x, GI, GI1),
	    assertz( id_table(I, GI1) )
	  ),
	  fail
	; true
	),
	( retract( spo(S, P, O) ),
	  short_id(S, S1),
	  short_id(O, O1),
	  assertz( spo(S1, P, O1) ),
	  fail
	; true
	).

short_id(X, X1) :-
	atom(X),
	atom_length(X, 40),
	id_table(X, X1),
	!.
short_id(X, X).