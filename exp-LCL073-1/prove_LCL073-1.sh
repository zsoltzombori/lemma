#!/bin/bash

THISDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
EXPDIR="`dirname ${THISDIR}`"
PATH=${EXPDIR}/prolog_scripts:${PATH}
LEMGEN_OPTIONS_PATH=${EXPDIR}/prolog_opts/lcl073:${EXPDIR}/prolog_opts

SORTFEATURES="[lf_hb_nongoal_symbol_occs,\
    lf_h_height,\
    lf_h_excluded_goal_subterms,\
    lf_h_tsize,\
    lf_h_distinct_vars,\
    dcterm_hash\
   ]"

LEMMAS=/tmp/lemmas_LCL073-1_psp_optim_7.pl
INPUTLEMMAS=/tmp/input_lemmas_LCL073-1_psp_optim_7.pl
FINALOUT=/tmp/proof_LCL073.out

>&2 echo ==== GENERATING LEMMAS ====
lemgen.sh -o "[timeout=[inf,40]]" LCL073-1 lg_psp_optim_7.pl >${LEMMAS}

>&2 echo ==== SORTING LEMMAS ====
spo-lemmas.sh -s "$SORTFEATURES" ${LEMMAS} >${INPUTLEMMAS}

>&2 echo ==== PROVING ====
provecd.sh -i "${INPUTLEMMAS}" \
	   -l "[max_lemmas=2900]" \
	   -t "[no_training_data]" \
	   LCL073-1 100 provecd_sgcd_ph_0321.pl \
	   >${FINALOUT}


