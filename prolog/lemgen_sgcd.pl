:- module(lemgen_sgcd,
	  [ gen_tlemma_sgcd/6,
	    lemgen_basic_sgcd_options/1,
	    lemgen_default_sgcd_options/5,
	    lemgen_default_sgcd_options/1 ]).

:- use_module(cdtools(sgcd_state_cd)).
:- use_module(cdtools(sgcd_core_cd)).
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(tptp_cd)).
:- use_module(cdtools(callutils_cd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Enumerate t-lemmas generated with SGCD.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% gen_tlemma_sgcd(+Problem, +LemmaOpts, +SGCDOpts,
%%%%                 -DTerm, -FTerm, -Status) is nondet.
%%%% 
%%%% Enumerate lemmas as D-Term (DTerm) and MGT (FTerm) pairs. If it was
%%%% detected that the problem can be already proven by some generated lemma,
%%%% the proof and its MGT are the single solution with Status proof.
%%%% Otherwise Status is lemma. FTerm is in the format of CDTools (=> for
%%%% implication).
%%%%
%%%% Lemmas are generated with the axiom-driven functionality of SGCD. Also
%%%% heuristic restrictions of SGCD are available.
%%%%
%%%% Timeout is the overall time limit for the lemma generation.
%%%%
%%%% SGCDOptions are passed to sgcd/1.
%%%% 
%%%% lemgen_default_sgcd_options/5 and lemgen_default_sgcd_options/2
%%%% greate defaults for SGCDOptions, specified by certain
%%%% parameters. These parameters are:
%%%%  - generator predicate: e.g., inc_tsize1, inc_height1, inc_psp1
%%%%  - timeout for a generation level
%%%%  - factor to limit size values of lemma formulas in relation to
%%%%    the values in the input (goal and axioms)
%%%%  - maximal number of lemmas at lower levels used to generate the
%%%%    next higher level
%%%%

:- dynamic seen/1.

gen_tlemma_sgcd(Problem, LemmaOpts, SGCDOpts, D, F, Status) :-
	retractall( seen(_) ),
	tptpcd_problem_install(Problem, [goal=Goal]),
	( memberchk(timeout=Timeout, LemmaOpts) ->
	  true
	; Timeout = 10
	),
	( memberchk(lemmas=Lemmas, LemmaOpts) ->
	  true
	; Lemmas = [level_solution, abandoned_level_solution]
	),
	lemgen_basic_sgcd_options(SGCDOpts1),
	merge_opts(SGCDOpts, SGCDOpts1, SGCDOpts2),
	SGCDOpts3 = [goal=Goal, proof=Proof|SGCDOpts2],
	info(10, 'Effective SGCD options: ~q', [SGCDOpts3]),
	( try_with_time_limit(Timeout, sgcd(SGCDOpts3)) ->
	  true
	; true
	),
	info_solution_statistics,
	( var(Proof) ->
	  Status = lemma,
	  info(10, 'No proof found, returning lemmas: ~q', [Lemmas]),
	  ( ( memberchk(level_solution, Lemmas) ->
	      level_solution(F, D, _)
	    ; fail
	    )
	  ; ( memberchk(new_level_solution, Lemmas) ->
	      new_level_solution(F, D, _)
	    ; fail
	    )
	  ; ( memberchk(abandoned_level_solution, Lemmas) ->
	      abandoned_level_solution(F, D, _)
	    ; fail
	    )
	  ),
	  variant_sha1(D, H),
	  ( seen(H) ->
	    fail
	  ; assertz( seen(H) )
	  )
	; Status = proof,
	  info(10, 'Proof found'),
	  Proof = D,
	  d_mgt(D, F)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemgen_basic_sgcd_options(O) :-
	O = [once = true,
	     %% pre_add_max=(-1), %% purely axiom driven
	     pre_add_max=0,       %% check for goal
	     post_max=(-1),
	     max_level = 1000000].

lemgen_default_sgcd_options(Opts) :-
	lemgen_default_sgcd_options(inc_tsize1, 2, 5, 5000, Opts).

lemgen_default_sgcd_options(Generator, LevelTimeout, Factor, Trim, Opts) :-
	Opts = [gen = Generator,
		gen_level_timeout = LevelTimeout,
		test=[lt_fh(truncate(input*Factor)+1),
		      lt_ft(truncate(input*Factor)+1),
		      lt_fv(truncate(input*Factor)+1),
		      lt_dup,
		      lt_subs],
		process_news = reg([],[],
				   [an_sort([kp_f_ht]),
				    an_trim(Trim)])
	       ].

merge_opts([X=V|O], O1, [X=V|O2]) :-
	select(X=_, O1, O3),
	!,
	merge_opts(O, O3, O2).
merge_opts([X|O], O1, [X|O2]) :-
	merge_opts(O, O1, O2).
merge_opts([], O, O).

info_solution_statistics :-
	predicate_property( level_solution(_, _, _),
			    number_of_clauses(N1) ),
	predicate_property( abandoned_level_solution(_, _, _),
			    number_of_clauses(N2) ),
	predicate_property( new_level_solution(_, _, _),
			    number_of_clauses(N3) ),
	Sum is N1+N2+N3,
	info(10, 'Lemmas (total): ~D (LevelSolution: ~D; Abandoned: ~D; New: ~D)', [Sum, N1, N2, N3]).
