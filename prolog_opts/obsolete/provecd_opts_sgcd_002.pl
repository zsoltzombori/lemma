
provecd_options(sgcd,
		SGCDOpts,
		LemmaOpts,
		GenDataOpts) :-
	%% SGCD options similar as in opts22.pl of the SGCD experiments
	%% Purely goal-driven, upon tsize
	SGCDOpts = [gen=inc_tsize1,
		    once=true,
		    pre_add_max=100000,
		    post_min=2],
	LemmaOpts = [],
	GenDataOpts = [write_spo,
		       lemma_methods=[subtree,
				      treerepair,
				      slemma_nonpure(10,50),
				      slemma_pure(10,50)]].

