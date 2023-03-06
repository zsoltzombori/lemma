:- module(lemextract,
	  [lemextract/2]).

:- use_module(datagen_lemmas).
:- use_module(lemma_problem_location).

:- multifile user:lemextract_options/1.

default_lemextract_options(O) :-
	O = [write_spo,
	     lemma_methods=[subtree,
			    treerepair,
			    slemma_nonpure(10,50),
			    slemma_pure(10,50)]].

lemextract(ProofDB, OptFile) :-
	optionsfile_absname(OptFile, AbsOptFile),
	info(10, 'Loading options file ~w', [AbsOptFile]),
	consult(AbsOptFile),
	( lemextract_options(Options) ->
	  true
	; default_lemextract_options(Options)
	),
	generate_training_data(ProofDB, Options).
