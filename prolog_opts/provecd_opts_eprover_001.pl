
provecd_options(tptp(eprover),
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [system_proof_format],
	LemmaOpts = [],
	GenDataOpts = [no_training_data].

