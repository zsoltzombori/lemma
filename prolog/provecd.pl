:- module(provecd, [provecd/4,
		    compute_reproof_utilities/8,
		    reproof_gc/1
		    ]).

:- use_module(datagen_lemmas).
:- use_module(lemma_features).
:- use_module(lemma_problem_location).
:- use_module(negative_lemmas).
:- use_module(swilib(err)).
:- use_module(cdtools(axioms_cd)).
:- use_module(cdtools(callutils_cd)).
:- use_module(cdtools(ccs_main_cd)).
:- use_module(cdtools(dc_representation_cd)).
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(dv_cd)).
:- use_module(cdtools(fterm_conversions_cd)).
:- use_module(cdtools(grounding_cd)).
:- use_module(cdtools(import_cd)).
:- use_module(cdtools(numutils_cd)).
:- use_module(cdtools(sgcd_core_cd)).
:- use_module(cdtools(sgcd_state_cd)).
:- use_module(cdtools(tptp_cd)).
:- use_module(nf(nfutils), [prolog_vars_to_atoms/2]).

:- multifile user:provecd_options/4.
:- thread_local seen/1.

provecd(Problem, Timeout, OptFile, Opts) :-
	optionsfile_absname(OptFile, AbsOptFile),
	info(10, 'Loading options file ~w', [AbsOptFile]),
	consult(AbsOptFile),
	( provecd_options(Prover, ProverOpts0, LemmaOpts0, TrainingOpts0) ->
	  true
	; info(10, 'Defaulting provecd_options'),
	  Prover = sgcd,
	  sgcd_default_options(ProverOpts0),
	  training_data_default_options(TrainingOpts0),
	  LemmaOpts0 = []
	),
	( memberchk(opts_prover=ProverOpts1, Opts) -> true ; ProverOpts1 = [] ),
	( memberchk(opts_lemmas=LemmaOpts1, Opts) -> true ; LemmaOpts1 = [] ),
	( memberchk(opts_training=TrainingOpts1, Opts) -> true ; TrainingOpts1 = [] ),
	append(ProverOpts1, ProverOpts0, ProverOpts2),
	append(LemmaOpts1, LemmaOpts0, LemmaOpts),
	append(TrainingOpts1, TrainingOpts0, TrainingOpts),
	( memberchk(lemmas=LemmasSpec, Opts), LemmasSpec \= none ->
	  read_lemmas(LemmasSpec, Lemmas, LemmaOpts)
	; Lemmas = []
	),
	%% A timeout can be specified in opts_prover to override Timeout just
	%% for the proving task. Useful if the resulting proof is used for
	%% training data generation, where Timeout is typically some overall
	%% timeout coming from scripts.
	( select(timeout=Timeout1, ProverOpts2, ProverOpts) ->
	  true
	; Timeout1 = Timeout,
	  ProverOpts = ProverOpts2
	),
	( memberchk(inf_limit=InfLimit, ProverOpts) -> true ; InfLimit = -1 ),
	( memberchk(pl_enum_d=D, Opts) ->
	  info(10, 'Running prove_problem in internal special mode'),
	  prove_problem(Prover, Problem, Timeout1, ProverOpts, Lemmas, D, _, _)
	; try_with_time_limit(Timeout1,
			      try_with_inference_limit(InfLimit,
						       prove_problem(Prover,
								     Problem, Timeout1, ProverOpts,
								     Lemmas,
								     D, Time, Inf))) ->
	  ( D = system_proof(Format, Structure) ->
	    info(10, 'Proof found for ~q in ~w sec, format: ~q',
		 [Problem, Time, Format]),
	    term_factorized(Structure, DA, DB),
	    Prf = proof_external(Problem, Time, factorized(Format), DA-DB),
	    info(10, '~q.', [Prf]),
	    \+ \+ ( numbervars(Prf, 0, _), format('% ~q.~n', [Prf])),
	    ( memberchk(halt, Opts) ->
	      show_infrate,
	      halt(0) %% or 2 ???
	    ; true
	    )
	  ; d_to_dc(D, DC),
	    d_csize(D, SC),
	    d_tsize(D, ST),
	    d_height(D, SH),
	    info(10, 'Proof found for ~q in ~w sec; csize: ~D, tsize: ~D, height: ~D, ',
		 [Problem, Time, SC, ST, SH]),
	    Prf = proof_dc(Problem, Time, DC),
	    info(10, '~q.', [Prf]),
	    \+ \+ ( numbervars(Prf, 0, _), format('% ~q.~n', [Prf])),
	    ( memberchk(no_training_data, TrainingOpts) ->
	      true
	    ; ProofDB = result(Problem,
			       singlerun, [Time,Time,Time], 1, DC,
			       [prover=Prover, prover_options=ProverOpts]),
	      generate_training_data(ProofDB, [nowrite|TrainingOpts]),
	      ( spo(Proof, type, proof),
		spo(Proof, problem, Problem),
		spo(Problem, goal, Goal) ->
		CtxOpts = [problem=Problem, goal=Goal]
	      ; err('No proof to generate training data from')
	      ),
	      ( memberchk(Prover, [prover9]),
		memberchk(reproof_timeout=TimeoutR, TrainingOpts), Time > 0 ->
		time_to_secs(TimeoutR, ReproofSecs),
		MaxLems is floor(ReproofSecs / Time)
	      ; memberchk(reproof_timeout=TimeoutR, TrainingOpts), Inf > 0 ->
		time_to_infs(TimeoutR, Time, Inf, InfR),
		MaxLems is floor(InfR / Inf)
	      ; MaxLems = -1
	      ),
 	      ( memberchk(lemmas_not_in_proof, TrainingOpts) ->
 		generate_negative_training_data(Prover, Proof, MaxLems, CtxOpts, TrainingOpts)
 	      ; true
 	      ),
	      ( memberchk(no_reproof, TrainingOpts) ->
		true
	      ; compute_reproof_utilities(Prover, Proof, ProverOpts, Lemmas,
					  Time, Inf, MaxLems, TrainingOpts),
		reproof_gc(TrainingOpts)
	      ),
	      ( memberchk( short_ids, TrainingOpts ) ->
		convert_to_short_ids
	      ; true
	      ),
	      ( memberchk(write_spo, TrainingOpts), \+ memberchk(nowrite, TrainingOpts) ->
		info(10, 'Writing spo/3 facts for training data'),
		write_spo,
		info(10, 'Writing done')
	      ; true
	      )
	    ),
	    ( memberchk(halt, Opts) ->
	      show_infrate,
	      halt(0)
	    ; true
	    )
	  )
	; info(10, 'Timeout for ~q', [Problem]),
	  ( memberchk(halt, Opts) ->
	    show_infrate,
	    halt(1)
	  ; true
	  )
	).

prove_problem(Prover, Problem, Timeout, ProverOpts, Lemmas, D,
	      UsedTime, UsedInf) :-
	problem_absname(Problem, Problem1),
	get_time(T0),
	statistics(inferences, I0),
	( Prover = sgcd ->
	  tptpcd_problem_install(Problem1, [goal=Goal]),
	  ( Lemmas = [] ->
	    sgcd([goal=Goal, proof=D|ProverOpts])
	  ; sgcd_inject_lemmas(Lemmas, [goal=Goal|ProverOpts]),
	    sgcd([keep_levels=true, min_level=continue, goal=Goal, proof=D|ProverOpts])
	  )  
	; Prover = ccs ->
	  mk_ccs_lemmas(Problem1, Lemmas, CCSInitLemmas),
	  ccs(Problem1, [proof=D1, out_pattern_rules=Rules,
			 init_lemmas=CCSInitLemmas | ProverOpts]),
	  ( Rules = [] ->
	    D = D1
	  ; D = system_proof(ccsc, D1)
	  )
	; %% Experimental
	  Prover = ccscx ->
	  mk_ccs_lemmas(Problem1, Lemmas, CCSInitLemmas),
	  tmp_ccs_lemmas_to_r1i(CCSInitLemmas, CCSInitLemmas1),
	  ccs(Problem1, [proof=D1, out_pattern_rules=Rules,
			 init_lemmas=CCSInitLemmas1 | ProverOpts]),
	  ( Rules = [], CCSInitLemmas1 = [] ->
	    D = D1
	  ; D = system_proof(ccscx, D1)
	  )
	; Prover = prover9 ->
	  tptpcd_prover9_proof(Problem1, Timeout, Lemmas, D, ProverOpts)
	; Prover = cmprover ->
	  tptpcd_cmpr_proof(Problem1, Timeout, Lemmas, D, ProverOpts)
	; Prover = tptp(System) ->
	  tptpcd_tptp_proof(System, Problem1, Timeout, Lemmas,
			    D, ProverOpts)
	; err('Undefined prover: ~q', [Prover])
	),
	get_time(T1),
	statistics(inferences, I1),
	UsedInf is I1-I0,
	UsedTime is round((T1 - T0) * 1000)/1000.0.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

training_data_default_options(O) :-
	O = [write_spo,
	     lemma_methods=[subtree,
			    treerepair,
			    slemma_nonpure(10,50),
			    slemma_pure(10,50)]].

sgcd_default_options(O) :-
	O = [once=true,
	     gen=Gen,
	     max_level=10000,
	     test=[lt_fh(truncate(input*Factor)+1),
		   lt_ft(truncate(input*Factor)+1),
		   lt_fv(truncate(input*Factor)+1),
		   lt_dup,
		   lt_subs],
	     process_news=reg([],[],
			      [an_sort([kp_f_ht]),
			       an_trim(Trim)]),
	     pre_add_max=PreAddMax
	    ],
	Gen = inc_tsize1,
	Factor = 5,
	PreAddMax = 1,
	Trim = 1000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Problem and Lemmas to Formula in PIE Format
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tptpcd_problem_to_pieform(P, Lemmas, Form, Axioms, LemmaProofs, Opts) :-
	retractall( seen(_) ),
	( memberchk(signature=original, Opts) ->
	  Opts1 = [original_signature=Sig]
	; Opts1 = []
	),
	canonicalize_tptpcd_problem(P, T1, AxiomsMatrix, Opts1),
	ctp_goal(T1, Goal),
	CD = (thm(X=>Y), thm(X) -> thm(Y)),
	ctp_axioms(AxiomsMatrix, Axioms),
	( member(A, [CD|Axioms]),
	  variant_sha1(A, H),
	  assertz( seen(H) ),
	  fail
	; true
	),
	tptpcd_problem_install(P, []),
	%% filter out invalid lemmas (d_mgt or dv_mgt fails) and duplicates
	%% via seen/1
	findall(L-D, ( member(D, Lemmas),
		       lemma_form(D, L),
		       variant_sha1(L, H),
		       ( seen(H) ->
			 fail
		       ; assertz( seen(H) )
		       )
		     ),
		LDs),
	length(LDs, NLemmas),
	info(10, 'Number of effective lemmas: ~D', [NLemmas]),
	local_map_key(LDs, LemmaForms),
	local_map_val(LDs, LemmaProofs),
	append([CD|Axioms], LemmaForms, Axioms1),
 	map_uclose(Axioms1, Axioms2), 
 	eclose(Goal, Goal1),
	Form1 = (Axioms2 -> Goal1),		
	( memberchk(signature=original, Opts) ->
	  convert_cd_signature(Sig, Form1, Form)
	; memberchk(signature=pinf, Opts) ->
	  convert_cd_signature(cd_signature(p,i,n,falsehood), Form1, Form)
	; Form1 = Form
	).


local_map_key([X-_|Xs], [X|Xs1]) :-
	local_map_key(Xs, Xs1).
local_map_key([], []).

local_map_val([_-X|Xs], [X|Xs1]) :-
	local_map_val(Xs, Xs1).
local_map_val([], []).

map_uclose([X], X1) :-
	!,
	uclose(X, X1).
map_uclose([X|Y], (X1, Y1)) :-
	uclose(X, X1),
	map_uclose(Y, Y1).
	
uclose(X, X1) :-	
	term_variables(X, V),
	( V = [] ->
	  X1 = X
	; prolog_vars_to_atoms(all(V,X), X1)
	).

eclose(X, X1) :-	
	term_variables(X, V),
	( V = [] ->
	  X1 = X
	; prolog_vars_to_atoms(ex(V,X), X1)
	).

lemma_form(D, thm(F)) :-
	ground(D),
	!,
	d_mgt(D, F).
lemma_form(D, F) :-
	term_variables(D, V),
	dv_mgt(D, H),
	list_to_thmseq(V, B),
	F = (B->thm(H)).

list_to_thmseq([], true).
list_to_thmseq([X], thm(X)) :-
	!.
list_to_thmseq([X|Y], (thm(X), Z)) :-
	list_to_thmseq(Y, Z).

ctp_goal([~thm(G)], thm(G)) :-
	!.
ctp_goal(X, _) :-
	err('Goal form not supported: ~q', [X]).

ctp_axioms([[thm(A)]|Ax], [thm(A)|Ax1]) :-
	!,
	ctp_axioms(Ax, Ax1).
ctp_axioms([], []) :-
	!.
ctp_axioms(X, _) :-
	err('Axioms form not supported: ~q', [X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Lemma Terms
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_lemmas(none, [], _) :-
	!.
read_lemmas(LemmasSpec, Lemmas, LemmaOpts) :-	
	info(10, 'Reading input lemmas ~w', [LemmasSpec]),
	read_file_to_terms(LemmasSpec, Lemmas1, []),
	length(Lemmas1, N),
	info(10, 'Number of read lemmas: ~D', [N]),
	canonicalize_and_filter_lemmas(Lemmas1, LemmaOpts, Lemmas).

canonicalize_and_filter_lemmas(L, Opts, L1) :-
	( memberchk(max_lemmas=N, Opts) -> true ; N = - 1),
	map_cl(L, N, Opts, L2),
	( memberchk(add_subproof_lemmas, Opts) ->
	  info(10, 'Adding input lemmas for the subproofs of the given input lemmas'),
	  findall(D1, ( member(D2, L2), sub_term(D1, D2), \+ atomic(D1) ), L3),
	  findall(A, (member(A, L2), atomic(A)), Ax),
	  append(Ax, L3, L4),
	  sort(L4, L1)
	; L1 = L2
	).

map_cl(_, 0, _, []) :-
	!.
map_cl([X|Xs], N, Opts, Xs1) :-
	( X = (_-_) ->
	  dc_to_d(X, X1)
	; X = X1
	),
	( memberchk(only_tlemmas, Opts), \+ ground(X1) ->
	  Xs1 = Xs2,
	  N1 = N
	; Xs1 = [X1|Xs2],
	  N1 is N-1
	),
	map_cl(Xs, N1, Opts, Xs2).
map_cl([], _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Prover9 Interface
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	  
tptpcd_prover9_proof(P, Timeout, Lemmas, D, Opts) :-
	time_to_integer_secs(Timeout, Timeout1),
	( select(timeout=_, Opts, Opts0) -> true ; Opts0 = Opts	),
	tptpcd_problem_install(P, []),
	tptpcd_problem_to_pieform(P, Lemmas, Form, _, LemmaProofs, Opts0),
	install_pr9_to_d_lemmas(LemmaProofs, []),
	( memberchk(prover9_settings=Settings, Opts0) ->
	  true
	; Settings = [clear(auto_denials), clear(print_given)]
	),
	( memberchk(system_proof_format, Opts0) ->
	  GetProof=(proof=D1),
	  D = system_proof(prover9, D1)
	; GetProof=(process_output=import_cd:pr9_proof_to_d(D))
	),
	Opts1 = [mace=false,
		 timeout=Timeout1,
		 GetProof,
		 prover9_settings=Settings,
		 printing=false,
		 r=true,
		 subprover_time=Time],
	append(Opts0, Opts1, Opts2),
	ppl_valid(Form, Opts2),
	( memberchk(print_time=Id, Opts2), Id \= false ->
	  Time1 is round(Time * 1000)/1000.0,
	  writeq(subprover_time(Id, Time1)),
	  writeln('.')
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CMProver Interface
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tptpcd_cmpr_proof(P, Timeout, Lemmas, D, CMOpts) :-
	tptpcd_problem_to_pieform(P, Lemmas, Form, Axioms,
				  LemmaProofs, CMOpts),
	( number(Timeout) ->
	  Timeout1 = Timeout
	; time_to_secs(Timeout, Timeout1)
	),
	( ppl_valid(Form, [prover=cm, result_proof=Proof, proof=dontprint,
			   timeout=Timeout1, r=true,
			   printing=false |CMOpts]) ->
	  ( memberchk(system_proof_format, CMOpts) ->
	    D = system_proof(cmpr,Proof)
	  ; install_axioms_for_cmpr_postprocessing(Axioms, LemmaProofs),
	    catch( cmpr_to_d(Proof, D),
		   cmpr_to_d_failure(_),
		   ( info(10, 'Proof conversion to D-term failed'),
		     D = system_proof(cmpr,Proof)
		   ))
	  )
	; info(10, 'No proof found'),
	  fail
	).

install_axioms_for_cmpr_postprocessing(Axioms, LemmaProofs) :-
	map_mk_axiom_spec(Axioms, 1, AxiomsSpec),
	install_axioms(AxiomsSpec, []),
	local_map_key(AxiomsSpec, AxiomIds),
	append(AxiomIds, LemmaProofs, CMPRLemmas),
	install_cmpr_to_d_lemmas(CMPRLemmas, []).

map_mk_axiom_spec([C|Xs], N, [N-A|Xs1]) :-
	( C = thm(A) ->
	  true
	; err('Bad axiom form: ~q', [C])
	),
	N1 is N+1,
	map_mk_axiom_spec(Xs, N1, Xs1).
map_mk_axiom_spec([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_cd_signature(cd_signature(P,C,N,F), Form, Form1) :-
	subst(Form, thm, 1, P, Form2),
	subst(Form2, '=>', 2, C, Form3),
	subst(Form3, n, 1, N, Form4),
	subst(Form4, falsehood, 0, F, Form1).

subst(X, A, _, A, X) :-
	!.
subst(X, _, _, _, X) :-
	var(X),
	!.
subst(X, A, N, B, Y) :-
	atomic(X),
	!,
	( X = A, N = 0 ->
	  Y = B
	; Y = X
	).
subst(X, A, N, B, Y) :-
	functor(X, F, M),
	( F = A, N = M ->
	  F1 = B
	; F1 = F
	),
	X =.. [_|Xs],
	map_subst(Xs, A, N, B, Ys),
	Y =.. [F1|Ys].

map_subst([X|Xs], Y1, Y2, Y3, [X1|Xs1]) :-
	subst(X, Y1, Y2, Y3, X1),
	map_subst(Xs, Y1, Y2, Y3, Xs1).
map_subst([], _, _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TPTP Prover Interface
%%%% - No creation of D-terms, and hence no creation of training data
%%%% - Invoking TPTP provers (Vampire, E), via PIE
%%%% - Creating TPTP problem files with lemmas
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tptpcd_tptp_proof(System, P, Timeout, Lemmas, D, Opts) :-
	tptpcd_problem_to_pieform(P, Lemmas, Form,
				  _Axioms, _LemmaProofs, Opts),
	( ppl_valid(Form, [prover=tptp(System), r=true,
			   printing=false, timeout=Timeout | Opts]) ->
	  D = system_proof(tptp,no_proof_representation)
	; info(10, 'No proof found'),
	  fail
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CCS Lemma Installation (Experimental)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_ccs_lemmas(Problem, Lemmas, CCSInitLemmas) :-
	tptpcd_problem_install(Problem, []),
	findall( h0(D, F),
		 ( member(D, Lemmas), ground(D), d_mgt(D, F) ),
		 CCSInitLemmas ).
%%%% 
%%%% Experimental
%%%% 
tmp_ccs_lemmas_to_r1i(L, L1) :-
	findall(h0(L3,F), ( member(h0(L2,F), L),
		      d_to_r1i(L2, L3) ),
		L1).
d_to_r1i(X, X) :-
	atomic(X),
	!.
d_to_r1i(d(A,B), r1(i,A1,B1)) :-
	d_to_r1i(A, A1),
	d_to_r1i(B, B1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SGCD Lemma Installation
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sgcd_inject_lemmas(Ds, Opts) :-
	length(Ds, Len),
	info(10, 'Injecting ~D lemmas', [Len]),
	init_sgcd_state(false),
	( memberchk(lemmas_levels=DLevelMethod, Opts) ->
	  true
	; memberchk(gen=Gen, Opts),
	  memberchk(Gen, [inc_height1,inc_height1u]) ->
	  %% inc_psp1, inc_prime1 too ???
	  DLevelMethod = autoclipped_height
	; DLevelMethod = autoclipped_tsize
	),
	( DLevelMethod = autoclipped_tsize ->
	  %% Clip above most frequent occuring tsize. For possible use with
	  %% lemmas generated by different methods. Assume that up to the most
	  %% frequently occurring tsize the levels can be replaced. Although
	  %% there may be other lemmas, obtained e.g. with inc_height1 or
	  %% inc_psp1, with larger tsize values.
	  autoclip_tsize(Ds, ClipSize),
	  DLevelMethod1 = clipped_tsize(ClipSize)
	; DLevelMethod = autoclipped_height ->
	  autoclip_height(Ds, ClipSize),
	  DLevelMethod1 = clipped_height(ClipSize)
	; DLevelMethod1 = DLevelMethod
	),
	info(10, 'Effective level determination method: ~q', [DLevelMethod1]),
	( memberchk(lemmas_max_cached_level=MCL, Opts) -> true ; true ),
	ML = ml(0),
	LSCounter = counter(0),
	( member(D, Ds),
	  d_mgt(D, F),
	  variant_sha1(F, H),
	  \+ level_solution_hash(H),
	  d_level(DLevelMethod1, D, L),
	  ML = ml(ML1),
	  ML2 is max(L, ML1),
	  nb_setarg(1, ML, ML2),
	  LSCounter = counter(LSCount),
	  LSCount1 is LSCount+1,
	  nb_setarg(1, LSCounter, LSCount1),
	  assertz( level_solution(F, D, L) ),
	  assertz( level_solution_hash(H) ),
	  fail
	; true
	),
	LSCounter = counter(LSCountFinal),
	info(10, 'Added ~D lemmas to level_solution/3', [LSCountFinal]),
	ML = ml(DetMCL),
	info(10, 'Found max level: ~D', [DetMCL]),
	( var(MCL) ->
	  MCL = DetMCL
	; true
	),
	info(10, 'Effective max cached level: ~D', [MCL]),
	( between(0, MCL, CL),
	  assertz( cached_level(CL) ),
	  fail
	; true
	),
	CurrL is MCL+1,
	set_level_info(CurrL),
	( memberchk(lemmas_post=Calls, Opts) ->
	  ( member(Call, Calls),
	    info(10, 'Postprocessing injected lemmas: ~q', [Call]),
	    Call,
	    fail
	  ; true
	  )
	; true
	).
	  
d_level(tsize, D, L) :- !, d_tsize(D, L).
d_level(height, D, L) :- !, d_height(D, L).
d_level(zero, _, L) :- !, L = 0.
d_level(clipped_tsize(N), D, L) :- !, d_tsize(D, L1), L is min(N, L1).
d_level(clipped_height(N), D, L) :- !, d_height(D, L1), L is min(N, L1).
d_level(M, _, _) :- err('No such d_level method: ~q', [M]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

autoclip_tsize(Ds, N) :-
	findall(X, ( member(D, Ds), d_tsize(D, X) ), Xs),
	mfv(Xs, N),
	autoclip_info(Xs, tsize, N).

autoclip_height(Ds, N) :-
	findall(X, ( member(D, Ds), d_height(D, X) ), Xs),
	mfv(Xs, N),
	autoclip_info(Xs, height, N).

mfv(Xs, V) :-
	sort(Xs, Xs1),
	findall(N-X, (member(X,Xs1), nsols(member(X,Xs),N)), NX),
	sort(NX, NX1),
	reverse(NX1, [_-V|_]).

autoclip_info(Xs, Type, N) :-
	nsols((member(X, Xs), X=N), SN),
	nsols((member(X, Xs), X<N), SA),
	nsols((member(X, Xs), X>N), SB),
	numbers_maximum(Xs, Max),
	info(10, 'Autoclip ~w: ~D, max: ~D, |lemmas(</=/>)|: ~D/~D/~D',
	     [Type, N, Max, SA, SN, SB]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_spo :-
	( spo(A,B,C),
	  numbervars(C,0,_),
	  writeq(spo(A,B,C)),
	  writeln('.'),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_reproof_utilities(Prover, Proof, ProverOpts, InputLemmas,
			  Time, Inf, MaxLems, Opts) :-
	memberchk(Prover, [sgcd, cmprover, ccs, prover9]),
	!,
	%% Assumes that the axioms of Proof and DC are installed
	( spo(Proof, dcterm, DC) ->
	  true
	; err('Proof has no dcterm: ~q', [Proof])
	),
	dc_to_d(DC, D),
	( ground(D) ->
	  true
	; err('Reproof utilities implemented only ground lemmas D-terms')
	),
	( spo(Proof, problem, Problem),
	  spo(Problem, goal, Goal) ->
	  if_to_f(Goal, Goal1)
	; err('Found no goal for reproof')
	),
	findall(A, axiom_id(A, _), Axioms),
	findall(lem(L,F,D1),
		( spo(L, lfp_containing_proof, Proof),
		  spo(L, type, lemma),
		  spo(L, dcterm, DC1),
		  dc_to_d(DC1, D1),
		  ground(D1),
		  contains_term(D1, D),
		  d_mgt(D1, F)
		),
		LFDs),
	( MaxLems >= 0 ->
	  choose_n_random_elements(LFDs, MaxLems, LFDs1)
	; LFDs1 = LFDs
	),
	( memberchk(context_msg=CtxMsg, Opts) ->
	  true
	; CtxMsg = ''
	),
	( memberchk(reproof_mode=Mode, Opts) ->
	  true
	; Mode = with_lemma
	),
	length(LFDs1, Len),
	info(10, 'Computing reproof utilities for ~D lemmas~w', [Len, CtxMsg]),
	flag(provecd_reproof, _, 1),
	( memberchk(Prover, [prover9]) ->
	  ProverTimeout is round(max(Time,1)),
	  InfLimit = -1
	; ProverTimeout = 1000000,
	  InfLimit = Inf
	),
	findall(InfL-L,
		( member(lem(L,F,LD), LFDs1),
		  flag(provecd_reproof, JLem, JLem+1),
		  info(10, 'Computing reproof utilities for lemma ~w/~w~w',
		       [JLem, Len, CtxMsg]),
		  ( atomic(LD) -> InfL = Inf
		  ; LD = D -> InfL = 0
		  ; ( Mode = without_lemma ->
		      once(select(lem(L,_,_), LFDs1, OtherLemmas)),
		      findall(F1, member(lem(_,F1,_), OtherLemmas), OtherForms),
		      append(OtherForms, Axioms, Axioms1)
		    ; Axioms1 = [F|Axioms]
		    ),
		    ProblemL = cd_problem(Goal1, Axioms1),
		    ( try_with_inference_limit(InfLimit,
					       prove_problem(Prover,
							     ProblemL,
							     ProverTimeout,
							     ProverOpts,
							     InputLemmas,
							     _,
							     _,
							     InfL)) ->
		      true
		    ; InfL is Inf
		    )
		  )
		),
		ILs),
	findall(InfL, (member(InfL-_, ILs), InfL > 0, InfL < Inf), Infs),
	( Infs = [] ->
	  Min = Inf,
	  Max = Inf
	; numbers_minimum(Infs, Min),
	  numbers_maximum(Infs, Max)
	),
	( member(InfL-L, ILs),
	  normalized_u_reproof_value(Mode, InfL, Min, Max, Inf, V),
	  assertz( spo(L, u_reproof, V) ),
	  fail
	; true
	).
compute_reproof_utilities(_Prover, Proof, _ProverOpts, _InputLemmas,
			  _Time, _Inf, _MaxLems, _Opts) :-
	( spo(Proof, dcterm, DC) ->
	  true
	; err('Proof has no dcterm: ~q', [Proof])
	),
	dc_to_d(DC, D),
	( ground(D) ->
	  true
	; err('Reproof utilities implemented only ground lemmas D-terms')
	),
	( spo(L, lfp_containing_proof, Proof),
	  spo(L, type, lemma),
	  spo(L, dcterm, DC1),
	  dc_to_d(DC1, D1), 
	  contains_term(D1, D),
	  assertz( spo(L, u_reproof, 0.5) ),
	  fail
	; true
	).

normalized_u_reproof_value(without_lemma, V, Max, Min, Inf, V1) :-
	!,
	( V >= Inf ->
	  V1 = 1.0
	; V = 0 ->
	  V1 = 0.0
	; Max = Min ->
	  V1 = 0.5
	; V1 is (Max-V)/(Max-Min)
	).
normalized_u_reproof_value(_, V, Max, Min, Inf, V1) :-	  
	( V >= Inf ->
	  V1 = 0.0
	; V = 0 ->
	  V1 = 1.0
	; Max = Min ->
	  V1 = 0.5
	; V1 is 1.0-(Max-V)/(Max-Min)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reproof_gc(Opts) :-
	( memberchk( reproof_gc_off, Opts ) ->
	  true
	; findall(L, ( spo(L, type, lemma), \+ spo(L, u_reproof, _) ), Ls ),
	  nsols( spo(_, type, lemma), NAll ),
	  length(Ls, N),
	  info(10, 'Deleting ~w of ~w lemmas because they have no u_reproof value',
	       [N, NAll]),
	  ( member(L, Ls),
	    retractall( spo(L, _, _) ),
	    retractall( spo(_, _, L) ),
	    fail
	  ; true
	  )
	).

% finalize_reproof_utilities :-
%  	( spo(L, type, lemma),
% 	  \+ spo(L, u_reproof, _),
% 	  ( spo(L, lfp_containing_proof, _) ->
% 	    assertz( spo(L, u_reproof, 0.5) )
% 	  ; assertz( spo(L, u_reproof, -1) )
% 	  ),
%  	  fail
%  	; true
%  	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_n_random_elements(L, N, L1) :-
	%% implementation based on ordering by variant_sha1 value
	map_add_rnd_key(L, L2),
	keysort(L2, L3),
	list_val_prefix(N, L3, L1).

list_val_prefix(0, _, []) :-
	!.
list_val_prefix(_, [], []) :-
	!.
list_val_prefix(N, [_-X|Xs], [X|Ys]) :-
	N1 is N-1,
	list_val_prefix(N1, Xs, Ys).

map_add_rnd_key([X|Xs], [K-X|Xs1]) :-
	variant_sha1(X, K),
	map_add_rnd_key(Xs, Xs1).
map_add_rnd_key([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time_to_infs(inf(X), _, _, Y) :-
	!,
	Y is round(X*5000000).
time_to_infs([inf,X], A, B, Y) :-
	!,
	time_to_infs(inf(X), A, B, Y).
time_to_infs(T, DT, DI, I) :-
	( DT =:= 0 ->
	  statistics(process_cputime, DT1),
	  statistics(inferences, DI1),
	  I is round(T*DI1/DT1)
	; I is round(T*DI/DT)
	).

time_to_integer_secs(X, S) :-
	time_to_secs(X, S1),
	S is round(max(S1,1)).

time_to_secs(inf(Inf), S) :-
	!,
	statistics(inferences, AllInfs),
	statistics(process_cputime, AllCPUTime),
	S is (Inf*5000000)/(AllInfs/max(AllCPUTime,1)),
	info(10, 'Converted ~w inf to ~w sec', [Inf, S]).
time_to_secs([inf,Inf], S) :-
	!,
	time_to_secs(inf(Inf), S).
time_to_secs(S, S).

show_infrate :-
	  statistics(inferences, I),
	  statistics(process_cputime, S),
	  IPS is round(I/max(S,1)),
	  IPS1 is IPS/5000000,
	  SPI1 is 1/IPS1,
	  info(10, 'Inferences per second: ~D; 1 s = ~2f inf; 1 inf = ~2f s',
	       [IPS, IPS1, SPI1]).
	  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
