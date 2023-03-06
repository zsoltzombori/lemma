%% SGCD options similar to SGCD-1 (= opts17.pl)
%% http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

provecd_options(sgcd,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]],
	ProverOpts = [once=true,
		      gen=Gen,
		      max_level=1000000,
		      test=[lt_fh(10),
			    lt_ft(36),
			    lt_fv(10),
			    lt_dup,
			    lt_subs,
			    lt_orga1],
		      process_news=reg([],[],
				       [an_sort([kp_f_hgt]),
					an_trim(Trim)]),
		      pre_gen=PreGen,
		      pre_add_max=PreAddMax
		     ],
	PreGen = inc_height1,
	Gen = inc_psp1,
	PreAddMax = 0,
	Trim = 1500.
