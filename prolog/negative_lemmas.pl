:- module(negative_lemmas,
	  [generate_negative_training_data/5]).

:- use_module(lemma_features).
:- use_module(cdtools(sgcd_state_cd)).
:- use_module(cdtools(dc_representation_cd)).

generate_negative_training_data(sgcd, Proof, MaxLems, CtxOpts, TrainingOpts) :-
	!,
	%% Assumes sgcd has just found a proof and its state
	%% (sgcd_state_cd.pl) is available. Chooses negative lemmas from this
	%% state.
	%%
	info(10, 'Generating lemmas not in proof'),
	( memberchk(random_seed=Seed, TrainingOpts) ->
	  true
	; Seed = 100
	),
	%% Seed=random for an actual randomly initialization
	set_random(seed(Seed)),
	spo(Proof, dcterm, DC),
	dc_to_d(DC, D),
	findall(D1, (sub_term(D1, D), compound(D1)), Ds),
	sort(Ds, Ds1),
	length(Ds1, L),
	( MaxLems >= 0 ->
	  Num is min(L-1, MaxLems)
	; Num is L-1
	),
	n_random_lemmas(Num, Ds1, Lemmas),
	length(Lemmas, L1),
	info(10, 'Adding ~D lemmas not in proof', [L1]),
	( Num = L1 ->
	  true
	; info(10, 'Could not find enough negative lemmas: required: ~w; found: ~w',
	       [Num, L1])
        ),	       
	findall(spo(S,P,O),
		( member(D2, Lemmas),
		  lemma_spo(D2, not_in_proof,
			    [is_not_in_proof,proof=Proof|CtxOpts],
			    spo(S,P,O), TrainingOpts)
%% \+ spo(S, _, _)
		),
		SPOs),
	( member(SPO, SPOs),
	  assertz(SPO),
	  fail
	; true
	),
	findall(S, member(spo(S,type,lemma), SPOs), LemmaIds),
	sort(LemmaIds, LemmaIds1),
	( memberchk(no_reproof, TrainingOpts) ->
	  true
	; ( member(S, LemmaIds1),
	    assertz( spo(S, u_reproof, -1) ),
	    fail
	  ; true
	  )
	).
generate_negative_training_data(Prover, _, _, _, _) :-
	info(10,
	     'WARNING: generate_lemmas_not_in_proof is for prover ~q not supported',
	     [Prover]).

n_random_lemmas(0, _, []) :-
	!.
n_random_lemmas(N, E, [D|Ds]) :-
	between(0,1000000,_),
	random_lemma(D),
	\+ memberchk(D, E),
	!,
	N1 is N-1,
	n_random_lemmas(N1, [D|E], Ds).
n_random_lemmas(_, _, []).

random_lemma(D) :-
	level_info_current_level(MaxLevel),
	MaxLevel >= 2,
	Level is 1+random(MaxLevel-1),
	findall(D, ( level_solution(_,D,Level)
		   ; abandoned_level_solution(_,D,Level)
		   ),
		Ds),
	length(Ds, Len),
	Len > 0,
	N is random(Len),
	nth0_element(N, Ds, D).

nth0_element(0, [X|_], X) :-
	!.
nth0_element(N, [_|X], Y) :-
	N1 is N-1,
	nth0_element(N1, X, Y).

