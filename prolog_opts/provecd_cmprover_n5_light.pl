%% CMProver options similar to CM-n5
%% http://cs.christophwernhard.com/pie/cmprover/evaluation_201803/tptp_neq.html
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
        ProverOpts = [cnf_trafo=fewsimp,
		      simp=[sort_lits],
		      cm_cfg=std,
		      add_cm_options=[hd]-[hd1,r4],
		      add_pr_options=[depth_timeout(2)]],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].

