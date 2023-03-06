
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [cnf_trafo=fewsimp],
	LemmaOpts = [],
	GenDataOpts = [no_training_data].

