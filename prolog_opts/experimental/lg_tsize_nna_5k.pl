%% SGCD options similar to SGCD-1 (= opts17.pl)
%% http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

lemgen_options([tlemma_sgcd([timeout=inf(15),
			     lemmas=[new_level_solution,level_solution]],
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
	Gen = inc_tsize1,
	Factor = 5,
	Trim = 5000.

