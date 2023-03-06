#!/bin/bash

export EXPERIMENT=8
export PROBLEM_FILE="rated_problems.txt"
export PROVER_TIMEOUT=200
export PROVER_CONFIG="provecd_opts_vampire_001.pl"
export TRAIN_VALIDATION=0.2
export TRAIN_BS=128
export TRAIN_LR=0.001
export TRAIN_TOLERANCE=500
export TRAIN_CONVOLUTIONS=16
export TRAIN_CHANNELS=64
export TRAIN_HIDDEN=1024
export TRAIN_UTILITY="all"
export TRAIN_MODEL_TYPE="gnn"
export LEMMA_COUNT=500
export LEMMA_UTILITY="u_tsize_reduction"
export LEMMA_FORMAT="dterm"
export LEMGEN_CONFIG="lg_tsize_optim.pl"
export DATAGEN_CONFIG="[timeout=5,processing=datagen,red=[subs,subt],lemma_methods=[subtree]]"
export DATAGEN=0
export LEMSORT_DEFAULT="[lf_hb_name,lf_hb_nongoal_symbol_occs,lf_h_subterms_not_in_goal,lf_h_excluded_goal_subterms,lf_hb_double_negation_occs,lf_h_tsize,lf_h_height,lf_h_distinct_vars,lf_hb_singletons,lf_h_csize,-lf_d_major_minor_relation,-lf_d_tsize,lf_d_csize]"
export LEMSORT_TRAIN=1
export EXPDIR="out/exp${EXPERIMENT}"
export PROVER_CORENUM=100
export LEMGEN_CORENUM=15
export CUDA_VISIBLE_DEVICES=4

bash iterative.sh > log/exp${EXPERIMENT}.out 2> log/exp${EXPERIMENT}.err
