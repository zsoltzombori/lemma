provecd_options(sgcd,
		ProverOpts,
		LemmaOpts,
		GenDataOpts) :-
	LemmaOpts = [],
	GenDataOpts = [write_spo, lemma_methods=[subtree]],
	ProverOpts = [once=true,
		      gen=Gen,
		      max_level=1000000,
		      test=[lt_fh(truncate(input*Factor)+1),
			    lt_ft(truncate(input*Factor*1.5)+1),
			    lt_fv(truncate(input*Factor)+1),
			    lt_dup,
			    lt_subs | Orga ],
		      process_news=reg([],[],
				       [an_sort([kp_f_hgt]),
					an_trim(Trim)]),
		      pre_gen=PreGen,
		      pre_add_max=PreAddMax,
		      pre_level_timeout=0.05
		     ],
	PreGen = inc_height1,
	Gen = inc_psp1,
	PreAddMax = 5,
	Trim = 1500,
	Factor = 3,
	Orga = [lt_orga1].


