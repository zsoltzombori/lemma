:- module(datagen_lemmas, [write_testfile/0,
			   proof_db/2,
			   load_proof_db/1,
			   generate_training_data/2,
			   generate_training_data/3,
			   problem_absname/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Extraction of Training Data from Proofs
%%%% 
%%%% The basis are CD proofs represented with result/5 facts and
%%%% metainfo on the proofs with experiment/4 facts. See section
%%%% "Proof Collection" for (the names of) examples of files with such
%%%% proofs by SGCD, CCS and Prover9.
%%%% 
%%%% The outputs are described in papers/training_data_spec.tex
%%%%
%%%% This runs in SWI-Prolog with CD Tools (Dec 2022 versions).
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(lemma_features).
:- use_module(lemma_extraction).
:- use_module(lemma_problem_location).
:- use_module(cdtools(dc_representation_cd)).
:- use_module(cdtools(callutils_cd)).
:- use_module(cdtools(tptp_cd)).
:- use_module(swilib(err)).
:- use_module(swilib(info)).
:- use_module(swilib(fromonto)).

:- multifile result/5.
:- multifile experiment/4.
:- dynamic result/5.
:- dynamic experiment/4.

:- dynamic result1/5.
:- dynamic tmp_seen/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Paths
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proof_collection_dir(Dir) :-
	getenv('HOME', Home),
	format(atom(Dir), '~w/clouds/terms22/experiments/proof_collection_01',
	       [Home]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Example sessions 1:
%%%% ?- write_testfile.
%%%% 
%%%% Example session 2
%%%% ?- generate_training_data(pdb_short, [], '/tmp/data.pl').
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% proof_db/2
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proof_db(pdb_short, [ metainfo_sgcd,
		      metainfo_ccs_pure_dterms,
		      results_selection_from_ccs_sgcd_nsimp ]).

proof_db(pdb_singlebasis, [ metainfo_singlebasis,
			    results_singlebasis ]).


% proof_db(pdb_singlebasis_2, [ metainfo_sgcd,
% 			      metainfo_ccs_pure_dterms,
% 			      metainfo_singlebasis,
% 			      results_selection_from_ccs_sgcd_nsimp,
% 			      results_singlebasis ]).

proof_db(pdb_prover9, [  metainfo_prover9_original,
			 results_prover9_nsimp ]).

proof_db(pdb_prover9_light, [  metainfo_prover9_original,
			       results_prover9_nsimp_light ]).

proof_db(pdb_prover9_heavy, [  metainfo_prover9_original,
			       results_prover9_nsimp_heavy ]).

proof_db(pdb_goaldriven, [ metainfo_sgcd,
			   results_sgcd_goaldriven_tsize,
			   results_sgcd_goaldriven_height ]).

% proof_db(pdb_goaldriven_2, [ metainfo_sgcd,
% 			     metainfo_ccs_pure_dterms,
% 			     results_sgcd_goaldriven_tsize,
% 			     results_sgcd_goaldriven_height,
% 			     results_ccs_pure_dterms
% 			   ]).

proof_db(pdb_sgcd, [ metainfo_sgcd,
		     results_sgcd
		   ]).

% proof_db(pdb_ccs_pure_dterms, [ metainfo_ccs_pure_dterms,
% 				     results_ccs_pure_dterms
% 				   ]).

proof_db(pdb_ccs_pure_dterms_part1, [ metainfo_ccs_pure_dterms,
				      results_ccs_pure_dterms_part1
				    ]).
proof_db(pdb_ccs_pure_dterms_part2, [ metainfo_ccs_pure_dterms,
				      results_ccs_pure_dterms_part2
				    ]).
proof_db(pdb_ccs_pure_dterms_part3, [ metainfo_ccs_pure_dterms,
				      results_ccs_pure_dterms_part3
				    ]).
proof_db(pdb_ccs_pure_dterms_part4, [ metainfo_ccs_pure_dterms,
				      results_ccs_pure_dterms_part4
				    ]).

% 
% proof_db(pdb_many, [ metainfo_sgcd,
% 		     metainfo_ccs_pure_dterms,
% 		     metainfo_singlebasis,
% 		     results_sgcd,
% 		     results_ccs_pure_dterms,
% 		     results_singlebasis
% 		   ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_training_data(ProofDB, Options) :- 
	load_proof_db(ProofDB),
	results_to_spo(Options),
	( memberchk(write_spo, Options), \+ memberchk(nowrite, Options) ->
	  info(10, 'Writing spo/3 facts for training data'),
	  write_spo,
	  info(10, 'Writing done')
	; true
	).

generate_training_data(ProofDB, Options, OutFile) :- 
	onto_file(generate_training_data(ProofDB, [write_spo|Options]),
		  OutFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_testfile :- 
	File = '/tmp/testfile.pl',
	Problem = 'LCL038-1',
%	flag(oid, _, 1),
	retractall( spo(_,_,_) ),
	load_proof_db(pdb_short),
	onto_file((( problem_spo(Problem, SPO)
		   ; result_spo(Problem,
				src,
				_RunId,
				1,
				SPO,
				[lemma_methods=[subtree, treerepair,
						slemma_nonpure(10,5),
						slemma_pure(10,5)]])
		   ),
		   assertz(SPO),
		   portray_clause(SPO),
		   fail
		  ; ( spo(L, type, lemma),
		      lemma_utility_spo(L, SPO),
		      portray_clause(SPO),
		      fail
		    ; true
		    )
		  ),
		  File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Asserts spo/3 to the triples for all proofs in result/5
%%%% 
results_to_spo(Opts) :-	
	configure_lemma_features(Opts),
	( memberchk(lemma_methods=LM, Opts) -> true
	; LM = [subtree,
		treerepair,
		slemma_nonpure(10,50),
		slemma_pure(10,50)]
	),
	( memberchk(src=Src, Opts) -> true
	; Src = src
	),
	retractall( spo(_,_,_) ),
	nsols( result1(_,_,_,_,_), NR ),
	info(10, 'Processing ~w results', [NR]),
	flag(prf, _, 1),
	( result1(Problem, RunId, _, ProofNum, _),
	  flag(prf, ProblemNum, ProblemNum+1),
	  problem_atomic_name(Problem, Problem1),
	  info(10, 'Processing result ~w/~w: ~w ~w ~w',
	       [ProblemNum, NR, Problem1, RunId, ProofNum]),
	  ( spo(Problem, type, problem) ->
	    true
	  ; ( problem_spo(Problem, SPO),
	      assertz( SPO ),
	      fail
	    ; true
	    )
	  ),
	  ( result_spo(Problem, Src, RunId, ProofNum, SPO,
		       [lemma_methods=LM]),
	    assertz(SPO),
	    fail
	  ; true
	  ),
	  fail
	; true
	),
	compute_utility_values,
	info(10, 'Utility values done').

result_spo(Problem, SourceId, RunId, ProofNumber, SPO, Opts) :-	
	findall(DCTerm1, result1(Problem, RunId, _, ProofNumber, DCTerm1), Results),
	( Results = [_] ->
	  true
	; Results = [] ->
	  err('Result not found for ~w ~w ~w', [Problem, RunId, ProofNumber])
	; err('Result not unique for ~w ~w ~w', [Problem, RunId, ProofNumber])
	),
	once(result1(Problem, RunId, [UsedTime|_], ProofNumber, DCTerm)),
	problem_atomic_name(Problem, Problem1),
	findall(K=V, experiment(Problem1, RunId, K, V), MetaInfo1),
	( memberchk(additional_metainfo=MetaInfo2, Opts) ->
	  append(MetaInfo2, MetaInfo1, MetaInfo3)
	; MetaInfo3 = MetaInfo1
	),
	MetaInfo = [used_time=UsedTime|MetaInfo3],
	dc_to_d(DCTerm, DTerm),
	mk_proof_id(Problem, RunId, ProofNumber, DCTerm, SourceId, ProofId),
	( proof_spo(ProofId, Problem, SourceId, DTerm, MetaInfo, SPO, [])
	; problem_absname(Problem, AbsProblem),
  	  tptpcd_problem_install(AbsProblem, [goal=Goal]),
	  memberchk(lemma_methods=Methods, Opts),
	  member(Method, Methods),
	  Opts1 = [lfp_d_incoming=_|Opts],
	  info(10, 'Generating lemmas for method ~q', [Method]),
	  gen_lemma(Method, DTerm, LemmaDTerm, Opts1),
	  LemmaContextOpts = [problem=Problem,
			      proof=ProofId,
			      proof_d=DTerm,
			      goal=Goal],
	  lemma_spo(LemmaDTerm, Method, LemmaContextOpts, SPO, Opts1)
	).

compute_utility_values :- 
        nsols( spo(_, type, lemma), NLemmas),
	info(10, 'Computing utility values for ~D lemmas', [NLemmas]),
	info_progress_start,
	findall(P, (spo(_, P, _), functor(P, F, _), atom_prefix(F, 'u_')), P1),
	sort(P1, P2),
	( member(P, P2),
	  retractall( spo(_, P, _) ),
	  fail
	; true
	),
	( spo(L, type, lemma),
	  info_progress(10, 100),
	  lemma_utility_spo(L, SPO),
	  assertz(SPO),
	  fail
	; true
	).

write_spo :- 
	( spo(A,B,C),
	  numbervars(C,0,_),
	  writeq(spo(A,B,C)),
	  writeln('.'),
	  fail
	; true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Some example proofs for axiom luk for testing purposes.
%%%% 
d_simp(d(d(d(1,1),d(d(d(1,d(1,1)),1),1)),1)).
d_peirce(d(d(d(1,d(d(d(1,d(1,d(d(d(1,d(1,1)),1),1))),1),1)),d(d(d(1,d(1,1)),1),1)),1)).
d_id(d(d(d(d(d(1,d(1,1)),1),1),d(d(d(1,d(1,1)),1),1)),1)).
d_syll(D) :- luk22_dc(DC), dc_to_d(DC, D).

luk22_dc(d(d(d(d(1,d(d(A,A),1)),1),1),1)-[A=d(1,d(d(B,1),B)),B=d(d(d(d(1,d(1,d(1,d(1,d(d(d(d(1,d(1,d(1,1))),1),1),1))))),1),1),1)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Proof Collection
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Load Files with proofs (result/5 facts) and metainfo on the proofs
%%%% (experiment/4 facts).
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_proof_db :-
	retractall( result(_,_,_,_,_) ),
	retractall( experiment(_,_,_,_) ),
	retractall( result1(_,_,_,_,_) ),
	retractall( tmp_seen(_) ),
	reset_lemma_features.

load_proof_db(result(Problem, RunId, Times, ProofNum, DC, MetaInfo)) :-
	!,
	reset_proof_db,
	assertz( result1(Problem, RunId, Times, ProofNum, DC) ),
	problem_atomic_name(Problem, Problem1),
	( member(K=V, MetaInfo),
	  assertz( experiment(Problem1, RunId, K, V) ),
	  fail
	; true
	).
load_proof_db(Results) :-
	( Results = [] ; Results = [_|_] ),
	!,
	reset_proof_db,
	( member( result(Problem, RunId, Times, ProofNum, DC, MetaInfo), Results ),
	  assertz( result1(Problem, RunId, Times, ProofNum, DC) ),
	  problem_atomic_name(Problem, Problem1),
	  ( member(K=V, MetaInfo),
	    assertz( experiment(Problem1, RunId, K, V) ),
	    fail
	  ; true
	  ),
	  fail
	; true
	).
load_proof_db(Key) :-
	reset_proof_db,		
	( proof_db(Key, Files) ->
	  true
	; err('Not configured with proof_db/2: ~q', [Key])
	),
	proof_collection_dir(Dir),
	( member(F, Files),
	  format(atom(File), '~w/~w', [Dir, F]),
	  info(10, 'Loading ~w', [File]),
	  consult(File),
	  fail
	; true
	),
	( result(X1,X2,X3,X4,X5),
	  variant_sha1(X1-X5, XH),
	  ( tmp_seen(XH) ->
	    true
	  ; assertz( result1(X1,X2,X3,X4,X5) ),
	    assertz( tmp_seen(XH) )
	  ),
	  fail
	; true
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_metainfo_available :-
	( result1(A, B, _, _, _),
	  \+ experiment(A, B,  _, _),
	  format('Missing metainfo for ~q ~q~n', [A, B]),
	  fail
	; true
	).

check_result_uniqueness :-
	( result1(A, B, _, C, D),
	  result1(A, B, _, C, D1),
	  \+ variant(D, D1),
	  !,
	  format('Nonunique: ~w ~w ~w~n', [A,B,C]),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
