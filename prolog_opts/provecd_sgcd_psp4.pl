%% SGCD options similar to PSP-4 (= opts34.pl)
%% http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_4.html

provecd_options(sgcd,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]],
	ProverOpts = [once=true,
		      gen=Gen,
		      test=[lt_fh(truncate(input*FactorH)+1),
			    lt_ft(truncate(input*FactorT)+1),
			    lt_subs],
		      max_level=1000000,
		      process_news=reg_first([],[],[])],
	Gen = inc_psp1,
	FactorH = 2,
	FactorT = 3.


