%% SGCD options similar to PSP-2 (= opts33.pl)
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
		      process_news=reg_first([],
					     [ipt_mgt,red_n],
					     [an_sort([kp_f_ht]),
					      an_trim(Trim)])],
	Gen = inc_psp1,
	FactorH = 5,
	FactorT = 5,
	Trim = 1000.
