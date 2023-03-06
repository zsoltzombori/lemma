%% SGCD options similar to SGCD-G1 (= opts22.pl)
%% http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_3.html

provecd_options(sgcd,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]],
	ProverOpts = [gen=inc_tsize1,
		      once=true,
		      pre_add_max=100000,
		      post_min=2].