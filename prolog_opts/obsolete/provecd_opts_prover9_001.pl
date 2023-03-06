
provecd_options(prover9,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [],
	LemmaOpts = [],
	GenDataOpts = [write_spo,
		       lemma_methods=[subtree,
				      treerepair,
				      slemma_nonpure(5,50),
				      slemma_pure(5,50)]].

