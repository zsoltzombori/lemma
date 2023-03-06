%% CMProver options similar to CM-n1
%% http://cs.christophwernhard.com/pie/cmprover/evaluation_201803/tptp_neq.html
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
        ProverOpts = [cnf_trafo=fewsimp,
		      simp=[sort_lits],
		      cm_cfg=lean1,
		      add_cm_options=[r8(al)]-[r4]],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].

