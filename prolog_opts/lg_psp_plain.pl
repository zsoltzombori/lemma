lemgen_options([tlemma_sgcd([timeout=inf(15),
			     lemmas=[level_solution,
				     abandoned_level_solution]],
			    O)],
	       [write_spo,unique]) :-
	O = [gen=inc_psp1,
	     test=[lt_dup, lt_subs]
	    ].


