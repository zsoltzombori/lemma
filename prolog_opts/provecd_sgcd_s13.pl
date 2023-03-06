%% SGCD options similar to SGCD-13 (= opts12.pl)
%% http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

provecd_options(sgcd,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]],
	ProverOpts = [once=true,
		      gen=Gen,
		      max_level=10000,
		      test=[lt_fh(input),
			    lt_ft(input),
			    lt_fv(input+1),
			    lt_dup,
			    lt_subs],
		      process_news=reg([],[],
				       [an_sort([kp_f_ht]),
					an_trim(Trim)]),
		      pre_add_max=PreAddMax
		     ],
	Gen = inc_tsize1,
	PreAddMax = 0,
	Trim = 3000.