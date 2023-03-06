%% SGCD options similar to SGCD-7 (= opts3.pl)
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
				       [an_sort([kp_f_hgt]),
					an_trim(Trim)]),
		      pre_add_max=PreAddMax,
		      pre_level_timeout=PreLevelTimeout
		     ],
	Gen = inc_tsize1,
	Factor = 2,
	PreAddMax = 1,
	PreLevelTimeout = inf(7.5),
	Trim = 3000.



