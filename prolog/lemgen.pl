:- module(lemgen,
	  [lemgen/3,
	   lemgen1/3]).

:- use_module(lemma_features).
:- use_module(lemma_problem_location).
:- use_module(lemgen_slemmas).
:- use_module(lemgen_sgcd).
:- use_module(lemgen_topdown).
:- use_module(datagen_lemmas).
:- use_module(negative_lemmas).
:- use_module(provecd, [compute_reproof_utilities/8, reproof_gc/1]).
:- use_module(cdtools(callutils_cd)).
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(axioms_cd)).
:- use_module(cdtools(tptp_cd)).
:- use_module(cdtools(sgcd_core_cd)).
:- use_module(cdtools(dv_cd)).
:- use_module(cdtools(dc_representation_cd)).
:- use_module(cdtools(grounding_cd)).
:- use_module(cdtools(fterm_measures_cd)).
:- use_module(cdtools(fterm_conversions_cd)).
:- use_module(cdtools(sgcd_state_cd)).
:- use_module(swilib(err)).
:- use_module(swilib(info)).

:- dynamic seen/1.

:- dynamic local_d/2.

:- multifile user:lemgen_options/2.

lemgen(Problem, OptFile, FurtherOpts) :-
	optionsfile_absname(OptFile, AbsOptFile),
	info(10, 'Loading options file ~w', [AbsOptFile]),
	consult(AbsOptFile),
	( lemgen_options(Methods, Options1) ->
	  true
	; Methods = [tlemma_sgcd_default([])],
	  Options1 = []
	),
	append(FurtherOpts, Options1, Options),
	info(10, 'Effective lemgen options: ~q', [Options]),
	lemgen1(Problem, Methods, Options).

lemgen1(Problem, Methods, Options) :-
	catch( generate_lemmas(Problem, Methods, Options),
	       proof_found(Problem1, D),
	       format('% ~q.~n', [proof_found(Problem1, D)])
	     ).

generate_lemmas(Problem, Methods, Opts) :-
	configure_lemma_features(Opts),
	Counter = counter(0),
	retractall( seen(_) ),
	retractall( local_d(_,_) ),
	( memberchk(processing=ProcLemmaMethod, Opts) ->
	  true
	; ProcLemmaMethod = lemgen
	),
	( memberchk(red=Red, Opts) ->
	  true
	; Red = []
	),
	( memberchk(head=Head, Opts) ->
	  true
	; Head = nil
	),
	problem_absname(Problem, Problem1),
	( memberchk(unique, Opts) ->
	  Unique = true
	; Unique = false
	),
	( problem_spo(Problem, spo(S,P,O)),
	  assertz( spo(S,P,O) ),
	  fail
	; true
	),
	( spo(Problem, goal, Goal) ->
	  CtxOpts = [problem=Problem, goal=Goal]
	; CtxOpts = [problem=Problem]
	),
	( member(Method, Methods),
	  predicate_property( local_d(_,_), number_of_clauses(N) ),
	  info(10, '~|Number of lemmas:~t ~D~50+', [N]),
	  info(10, 'Generating lemmas for method ~q', [Method]),
	  lemgen_core(Method, Problem1, D, Status, Opts),
	  ( Status = proof ->
	    throw(proof_found(Problem, D))
	  ; true
	  ),
	  ( Unique = true ->
	    variant_sha1(D, DH),
	    ( seen(DH) ->
	      fail
	    ; assertz( seen(DH) )
	    )
	  ; true
	  ),
	  assertz( local_d(D, Method) ),
	  fail
	; arg(1, Counter, N),
	  info(10, 'Lemmas (total): ~D', [N])
	),
	predicate_property( local_d(_,_), number_of_clauses(NFinal) ),
	info(10, '~|Number of lemmas:~t ~D~50+', [NFinal]),
	reduce_lemmas(Red, Head),
	process_lemmas(ProcLemmaMethod, CtxOpts, Opts),
	( memberchk( short_ids, Opts ) ->
	  convert_to_short_ids
	; true
	),	
	( memberchk(write_spo, Opts), \+ memberchk(nowrite, Opts) ->
	  info(10, 'Writing spo/3 facts'),
	  write_spo,
	  info(10, 'Writing done')
	; true
	).

process_lemmas(lemgen, CtxOpts, Opts) :-
	!,
	info(10, 'Processing lemmas: computing lemma features'),
	info_progress_start,
	( local_d(D, Method),
	  info_progress(10, 100),
	  lemma_spo(D, Method, CtxOpts, spo(S,P,O), Opts),
	  assertz(spo(S,P,O)),
	  fail
	; true
	).
process_lemmas(datagen, _, Opts) :-
	!,
	info(10, 'Processing lemmas: generate training data'),
	info_progress_start,
	findall(F, axiom_id(F, _), Axioms),
	findall(R, ( local_d(D, tlemma_sgcd(_, SOptions)),
		     d_to_dc(D, DC),
		     d_mgt(D, Goal),
		     grd_p(Goal),
		     Problem = cd_problem(Goal, Axioms),
		     R = result(Problem,
				singlerun, [0.0,0.0,0.0], 1, DC,
				[prover=datagen, sgcd_options=SOptions]) ),
		ProofDB),
	generate_training_data(ProofDB, [nowrite|Opts]),
	( memberchk(no_reproof, Opts), \+ memberchk(lemmas_not_in_proof, Opts) ->
	  true
	; ( memberchk(timeout=Timeout, Opts) ->
	    true
	  ; Timeout = inf(15)
	  ),
	  lemgen_basic_sgcd_options(BasicOpts),
	  nsols( ( spo(Proof, type, proof),
		   spo(Proof, problem, Problem),
		   spo(Proof, meta_info, Meta),
		   memberchk(sgcd_options=ProverOpts, Meta) ), NProofs),
	  info(10, 'Number of proofs as basis for re-proving with a lemma: ~D', [NProofs]),
	  flag(rpr_proof, _, 1),
	  ( spo(Proof, type, proof),
	    spo(Proof, problem, Problem),
	    spo(Proof, meta_info, Meta),
	    memberchk(sgcd_options=ProverOpts, Meta),
	    flag(rpr_proof, IProof, IProof+1),
	    info(10, 'Re-proving for proof ~w/~w', [IProof, NProofs]),
	    append(BasicOpts, ProverOpts, ProverOpts1),
	    findall(I-F, ( spo(Problem, axiom(I), F1), if_to_f(F1, F) ), Ax),
	    keysort(Ax, Ax1),
	    install_axioms(Ax1, []),
	    spo(Problem, goal, G),
	    if_to_f(G, G1),
	    %% re-proof the full problem to find Inf and as basis for negative data
	    init_sgcd_state(false), %% ? needed
	    info(10, 'Timeout for proof attempt: ~q', [Timeout]),
	    statistics(inferences, Inf1),
	    ( try_with_time_limit(Timeout, sgcd([goal=G1|ProverOpts1])) ->
	      true
	    ; true
	    ),
	    statistics(inferences, Inf2),
	    MaxLems = -1,
	    ( memberchk(lemmas_not_in_proof, Opts) ->
	      %% takes the state left by SGCD as basis
	      %% thus needs the above re-proof call
	      CtxOpts = [problem=Problem, goal=G],
	      generate_negative_training_data(sgcd, Proof, MaxLems, CtxOpts, Opts)
	    ; true
	    ),
	    ( memberchk(no_reproof, Opts) ->
	      true
	    ; Inf is Inf2 - Inf1,
	      format(atom(CtxMsg), ' for proof ~w of ~w', [IProof, NProofs]),
	      compute_reproof_utilities(sgcd, Proof, ProverOpts1, [],
					100000, Inf, MaxLems,
					[context_msg=CtxMsg|Opts])
	    ),
	    fail
	  ; true
	  )
	; true
	),
	( memberchk(no_reproof, Opts) ->
	  true
	; reproof_gc(Opts)
	),
	info(10, 'Generating training data finished').
process_lemmas(none, _, _) :-
	!.
process_lemmas(X, _, _) :-
	err('No such lemma processing method: ~q', [X]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemgen_core(tlemma_sgcd_default(LOpts), Problem, D, S, Opts) :-
	!,
	lemgen_default_sgcd_options(SOpts),
	lemgen_core(tlemma_sgcd(LOpts, SOpts), Problem, D, S, Opts).
lemgen_core(tlemma_sgcd_default(LOpts, Generator, LevelTimeOut,
				FTermFactor, TrimSize),
	    Problem, D, S, Opts) :-
	!,
	lemgen_default_sgcd_options(Generator, LevelTimeOut,
				    FTermFactor, TrimSize, SOpts),
	lemgen_core(tlemma_sgcd(LOpts, SOpts), Problem, D, S, Opts).
lemgen_core(tlemma_sgcd(LOpts, SOpts), Problem, D, S, Opts) :-
	!,
	( memberchk(timeout=Timeout1, Opts) ->
	  LOpts1 = [timeout=Timeout1|LOpts]
	; LOpts1 = LOpts
	),
	gen_tlemma_sgcd(Problem, LOpts1, SOpts, D, _, S).
lemgen_core(tlemma_topdown_tsize(Timeout), Problem, D, S, Opts) :-
	!,
	( memberchk(timeout=Timeout1, Opts) ->
	  Timeout2 = Timeout1
	; Timeout2 = Timeout
	),
	gen_tlemma_topdown_tsize(Problem, Timeout2, D, _, S).
lemgen_core(slemma_pure_from_table, Problem, D, lemma, _) :-
	!,
	tptpcd_problem_install(Problem, []),
	nsols(axiom_id(_,_), NumberOfAxioms),
	( NumberOfAxioms = 1 ->
	  gen_slemma_pure_v1(D)
	; gen_slemma_pure_v2(D)
	).
lemgen_core(slemma_pure_upto_tsize(S), _Problem, D, lemma, _) :-
	!,
	gen_slemma_pure_upto_tsize(S, D).
lemgen_core(slemma_nonpure_from_table, Problem, D, lemma, _) :-
	!,
	tptpcd_problem_install(Problem, []),
	nsols(axiom_id(_,_), NumberOfAxioms),
	( NumberOfAxioms = 1 ->
	  gen_slemma_nonpure_v1(D)
	; gen_slemma_nonpure_v2(D)
	),
	\+ \+ dv_mgt(D, _).
lemgen_core(slemma_nonpure_upto_tsize(S), Problem, D, lemma, _) :-
	!,
	tptpcd_problem_install(Problem, []),
	gen_slemma_mgt_nonpure_upto_tsize(S, D, _).
lemgen_core(M, _, _, _, _) :-
	err('No such lemma generation method: ~q', [M]).

write_spo :-
	( spo(A,B,C),
	  numbervars(C,0,_),
	  writeq(spo(A,B,C)),
	  writeln('.'),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Reduce Lemmas
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_lemmas([], _) :-
	!.
reduce_lemmas(Reds, Head) :-
	info(10, 'Reducing lemmas'),
	findall(K-lem(F,D,M),
		( local_d(D, M),
		  d_mgt(D, F),
		  lem_red_key(D, F, K) ),
		Ls1),
	retractall( local_d(_,_) ),
	keysort(Ls1, Ls2),
	local_map_val(Ls2, Ls3),
	reverse(Ls3, Ls4),
	apply_reds(Reds, Ls4, Ls5),
	reverse(Ls5, Ls6),
	local_list_prefix(Head, Ls6, Ls7),
	( member(lem(_,D,M), Ls7),
	  assertz( local_d(D, M) ),
	  fail
	; true
	).

lem_red_key(D, F, K) :-
	K = k(FT, NV, DT, DH),
	f_tsize(F, FT),
	term_variables(F, V),
	length(V, NV1),
	NV is NV1 * -1,
	d_tsize(D, DT),
	d_height(D, DH).

apply_reds([Red|Reds], Ls, Ls1) :-
	length(Ls, N),
	info(10, 'Applying lemma reduction ~q', [Red]),
	info(10, '~|Number of lemmas before ~w:~t ~D~50+', [Red, N]),
	red(Red, Ls, Ls2),
	length(Ls2, N1),
	info(10, '~|Number of lemmas after ~w:~t ~D~50+', [Red, N1]),
	apply_reds(Reds, Ls2, Ls1).
apply_reds([], Ls, Ls).

red(subs, Ls, Ls1) :-
	%% Subsumption
	!,
	info_progress_start,
	lemred_subs(Ls, Ls1).
red(subt, Ls, Ls1) :-
	%% Exclude if formula appears as strict subformula of another lemma
	%% (somewhat like organic)
	!,
	info_progress_start,
	lemred_subt(Ls, Ls1).
red(X, _, _) :-
	err('No such lemma reduction method: ~q', [X]).

lemred_subs([lem(F,_,_)|L], L1) :-
	grd_n(F),
	info_progress(10, 1000),
	\+ \+ memberchk(lem(F,_,_), L),
	!,
	lemred_subs(L, L1).
lemred_subs([X|L], [X|L1]) :-
	lemred_subs(L, L1).
lemred_subs([], []).

lemred_subt(Ls, Ls1) :-
	map_add_subt(Ls, Ls2),
	lemred_subt_1(Ls2, Ls1).

add_subt(lem(F,D,M), subt(S, S1)-lem(F,D,M)) :-
	variant_sha1(F, S),
	findall(H, (sub_term(U,F), \+ var(U), U \== F, variant_sha1(U,H)), S2),
	sort(S2, S1).

map_add_subt([X|Xs], [X1|Xs1]) :-
	add_subt(X, X1),
	map_add_subt(Xs, Xs1).
map_add_subt([], []).

lemred_subt_1([subt(_,S)-_|L], L1) :-
	info_progress(10, 1000),
	member(S1, S),
	memberchk(subt(S1,_)-_, L),
	!,
	lemred_subt_1(L, L1).
lemred_subt_1([_-E|L], [E|L1]) :-
	lemred_subt_1(L, L1).
lemred_subt_1([], []).

local_map_val([_-X|Xs], [X|Xs1]) :-
	local_map_val(Xs, Xs1).
local_map_val([], []).

local_list_prefix(nil, X, X) :-
	!.
local_list_prefix(N, X, Y) :-
	N < 0,
	!,
	reverse(X, X1),
	N1 is N * -1,
	local_list_prefix_1(N1, X1, X2),
	reverse(X2, Y).
local_list_prefix(N, X, Y) :-
	local_list_prefix_1(N, X, Y).

local_list_prefix_1(0, _, []) :-
	!.
local_list_prefix_1(N, [X|Xs], [X|Ys]) :-
	N1 is N-1,
	local_list_prefix_1(N1, Xs, Ys).
