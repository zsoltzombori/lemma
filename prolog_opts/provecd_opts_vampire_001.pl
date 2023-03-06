
provecd_options(tptp(vampire),
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	ProverOpts = [system_proof_format, vampire_options='--mode casc'],
	LemmaOpts = [],
	GenDataOpts = [no_training_data].

