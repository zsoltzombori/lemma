
provecd_options(prover9,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].
