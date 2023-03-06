
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
        ProverOpts = [cnf_trafo=fewsimp,
		      add_cm_options=[hs]-[hd1,r8(_)]],
	LemmaOpts = [],
	GenDataOpts = [no_training_data].

