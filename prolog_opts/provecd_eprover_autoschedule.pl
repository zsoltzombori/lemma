
provecd_options(tptp(eprover),
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [system_proof_format,
		      eprover_options='--auto-schedule --memory-limit=Auto'],
	LemmaOpts = [],
	GenDataOpts = [no_training_data].

