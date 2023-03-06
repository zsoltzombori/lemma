%% CMProver options similar to CM-n3
%% http://cs.christophwernhard.com/pie/cmprover/evaluation_201803/tptp_neq.html
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
        ProverOpts = [cnf_trafo=fewsimp,
		      simp=[sort_lits],add_cm_options=[]-[r4],		      
		      cm_cfg=low],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].

