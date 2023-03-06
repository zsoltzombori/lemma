%% SGCD options similar to SGCD-2 (= opts20.pl)
%% http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

lemgen_options([tlemma_sgcd([timeout=inf(15),
			     lemmas=[level_solution,
				     abandoned_level_solution]],
			    O)],
	       [write_spo,unique]) :-
	O = [gen=Gen,
	     test=[lt_fh(truncate(input*Factor)+1),
		   lt_ft(truncate(input*Factor)+1),
		   lt_fv(truncate(input*Factor)+1),
		   lt_dup,
		   lt_subs],
	     process_news=reg([],[],
			      [an_sort([kp_f_hgt]),
			       an_trim(Trim)])
	    ],
	Gen = inc_height1,
	Factor = 5,
	Trim = 1000.


