
lemextract_options(O) :-
	O = [write_spo,
	     include_special_features=[lf_hb_organic],
	     lemma_methods=[subtree,
			    treerepair,
			    slemma_nonpure(10,50),
			    slemma_pure(10,50)]].

