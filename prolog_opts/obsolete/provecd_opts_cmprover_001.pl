
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [cnf_trafo=fewsimp],
	LemmaOpts = [],
	GenDataOpts = [write_spo,
		       lemma_methods=[subtree,
				      treerepair,
				      slemma_nonpure(5,50),
				      slemma_pure(5,50)]].

