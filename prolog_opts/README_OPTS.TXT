Note that the LEMGEN_OPTIONS_PATH environment variable can contain
several directories, separated by ":".

- subdirectories: experimental/

  Settings or ideas used in specific experiments but not considered of
  "general importance" or as "generally useful" settings.

- subdirectories: lcl073/

  Used for proving LCL073-1

------------------------------------------------------------------------------

These option settings for provecd.sh all have just "subtrees" as
lemma_methods.  Can be overridden with the -t flag, e.g., -t
[lemma_methods=[]] to generate no lemma_methods.

------------------------------------------------------------------------------
Single Configurations
------------------------------------------------------------------------------

provecd_sgcd_sX.pl
  SGCD with options similar to:
  http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

provecd_sgcd_gX.pl
  SGCD, purely goal-driven, with options similar to:
  http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

provecd_sgcd_sX_g.pl
  Like provecd_opts_sgcd_sX_g.pl but with different "news processing":
  kp_f_hgt instead of kp_f_ht

------------------------------------------------------------------------------
Multiple Configurations Invoked in Parallel
------------------------------------------------------------------------------

f_cmprover
  CMPROVER in 9 configurations, according to
  http://cs.christophwernhard.com/pie/cmprover/evaluation_201803/tptp_neq.html

f_sgcd_basic
  SGCD-1+2+3+7
  see: http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

f_sgcd_basic_g
   Like f_sgcd_basic but invokes provecd_sgcd_sX_g.pl veersions, which
   have a different "news processing" with kp_f_hgt instead of kp_f_ht

f_sgcd_tsize
  SGCD in settings that generate by treesize, from
  http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

f_sgcd_height
  SGCD in settings that generate by height, from
  http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html

f_sgcd_gd
  SGCD in purely goal-driven settings, by trees size and height, from
  http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_3.html

f_sgcd_psp
  SGCD in settings that generate by PSP, from
  http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_4.html

f_prover9
  just a single setting, to allow analog syntax to the other settings
  in scripts (with the -f flag)

------------------------------------------------------------------------------
EXPERIMENTAL
------------------------------------------------------------------------------

lg_xxx_na_Nk
  export just level_solution, not abandoned level solution
  Nk is the "trim" size
  
provecd_sgcd_ph_ABCD.pl
  All these use PSP as base generator and Height as goal-driven
  generator. Number Code:
    A: 0 or more goal-driven levels (more with short level timeout)
    B: trim size
    C: formula size limit factor
    D: 0/1: without/with lt_orga_1
    
