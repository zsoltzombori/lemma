
%% SGCD with different settings

lemgen_options([tlemma_sgcd_default([timeout=6],inc_tsize1,2,5,1000),
		tlemma_sgcd_default([timeout=6],inc_height1,2,5,1000),
		tlemma_sgcd_default([timeout=6],inc_psp1,2,2,1000),
		tlemma_sgcd_default([timeout=6],inc_prime1,2,5,1000)],
	       [write_spo,unique]).
