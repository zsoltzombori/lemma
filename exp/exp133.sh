#!/bin/bash

# parent: 87
# PROVECD_CONFIG: lemmas_not_in_proof,no_reproof -> reproof_timeout=[inf,30]
# UTILITY: lf_is_in_proof -> u_reproof


export EXPERIMENT=133
export PROBLEM_FILE="rated_problems.txt"
export PROVER_TIMEOUT="[inf,1800]"
export SECURITY_TIMEOUT=2000
export PROVER_CONFIG="f_sgcd_basic"
export PROVECD_CONFIG="[lemma_methods=[subtree],reproof_timeout=[inf,30]]"
export TRAIN_VALIDATION=0.2
export TRAIN_BS=128
export TRAIN_LR=0.001
export TRAIN_TOLERANCE=50
export TRAIN_CONVOLUTIONS=16
export TRAIN_CHANNELS=64
export TRAIN_HIDDEN=1024
export TRAIN_UTILITY="u_reproof"
export TRAIN_MODEL_TYPE="linear"
export LEMMA_COUNT=200
export LEMMA_UTILITY="u_reproof"
export LEMMA_FORMAT="dterm"
export LEMGEN_CONFIG="lg_tsize_optim.pl"
export DATAGEN_CONFIG="[timeout=[inf,5],processing=datagen,red=[subs,subt],head=10,lemma_methods=[subtree],lemmas_not_in_proof,no_reproof]"
export DATAGEN=0
export LEMSORT_DEFAULT="[lf_hb_name,lf_hb_nongoal_symbol_occs,lf_h_subterms_not_in_goal,lf_h_excluded_goal_subterms,lf_hb_double_negation_occs,lf_h_tsize,lf_h_height,lf_h_distinct_vars,lf_hb_singletons,lf_h_csize,-lf_d_major_minor_relation,-lf_d_tsize,lf_d_csize]"
export LEMSORT_TRAIN=1
export EXPDIR="out/exp${EXPERIMENT}"
export PROVER_CORENUM=100
export LEMGEN_CORENUM=20
export CUDA_VISIBLE_DEVICES=4

bash iterative.sh > log/exp${EXPERIMENT}.out 2> log/exp${EXPERIMENT}.err
