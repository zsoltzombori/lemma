
provecd_options(sgcd,
		SGCDOpts,
		LemmaOpts,
		GenDataOpts) :-
	%% SGCD options similar as in opts17.pl of the SGCD experiments
	SGCDOpts = [once=true,
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
	Gen = inc_tsize1,
	Factor = 5,
	PreAddMax = 1,
	Trim = 1000,
	LemmaOpts = [lemma_levels=tsize], %% should match the generator Gen
	GenDataOpts = [write_spo, lemma_methods=[subtree]].
	


