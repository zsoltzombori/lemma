
%% slemmas just upto tree size 6, e.g. for proofs from Prover9

lemextract_options(O) :-
	O = [write_spo,
	     include_special_features=[lf_hb_organic],
	     lemma_methods=[subtree,
			    treerepair,
			    slemma_nonpure(6,50),
			    slemma_pure(6,50)]].

