%% CMProver options similar to CM-n8
%% http://cs.christophwernhard.com/pie/cmprover/evaluation_201803/tptp_neq.html
provecd_options(cmprover,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
        ProverOpts = [cnf_trafo=fewsimp,
		      cm_cfg=std,add_cm_options=[hs]-[hd1,r8(al)]],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].

