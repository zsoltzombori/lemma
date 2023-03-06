lemgen_options([tlemma_sgcd([timeout=inf(15),
			     lemmas=[level_solution,
				     abandoned_level_solution]],
			    O)],
	       [write_spo,unique]) :-
	O = [gen=Gen,
	     test=[
		   lt_dup,
		   lt_subs],
	     process_news=reg([],[],
			      [an_sort([kp_f_ht]),
			       an_trim(Trim)])
	    ],
	Gen = inc_prime1,
	%% exhausts in about 10 seconds
	Trim = 2000.

