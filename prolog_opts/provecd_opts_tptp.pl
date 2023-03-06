
provecd_options(tptp(none),
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [system_proof_format],
	LemmaOpts = [],
	GenDataOpts = [no_training_data].

