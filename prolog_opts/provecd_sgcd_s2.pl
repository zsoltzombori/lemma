%% SGCD options similar to SGCD-2 (= opts20.pl)
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
		      test=[lt_fh(truncate(input*Factor)+1),
			    lt_ft(truncate(input*Factor)+1),
			    lt_fv(truncate(input*Factor)+1),
			    lt_dup,
			    lt_subs],
		      process_news=reg([],[],
				       [an_sort([kp_f_ht]),
					an_trim(Trim)]),
		      pre_add_max=PreAddMax
		     ],
	Gen = inc_height1,
	Factor = 5,
	PreAddMax = 1,
	Trim = 1000.
