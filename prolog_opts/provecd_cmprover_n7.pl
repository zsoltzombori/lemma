%% CMProver options similar to CM-n7
%% http://cs.christophwernhard.com/pie/cmprover/evaluation_201803/tptp_neq.html
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
        ProverOpts = [cnf_trafo=fewsimp,
		      add_cm_options=[r8(al)],add_pr_options=[depth_timeout(2)]],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].

