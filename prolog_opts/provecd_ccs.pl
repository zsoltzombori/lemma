provecd_options(ccs,
		CCSOpts,
		LemmaOpts,
		GenDataOpts) :-
	%% Goal-driven upon compacted size
	CCSOpts = [format=cd, gen=[d(_,_)], once=once],
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]].
