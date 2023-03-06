#!/bin/bash

# ======================================================================
# Usage:
#
# provecd.sh [<cmdoptions>] <problem> <timeout> <optionsfile>
#
# CmdOptions:
#  -g additional arguments to GNU parallel
#  -i <lemmafile>
#  -p <options>  additional prover options
#  -l <options>  additional lemma processing options
#  -t <options>  additional training data generation options
#
# <optionsfile> is either a Prolog-readable file with extension .pl that
# defined the predicate provecd_options/4 or a file that lists the names of
# such Prolog files. In the latter case, the specified configurations
# are called in parallel until the first succeeds.
#
# <optionsfile> itself as well as the entries in <optionsfile> for parallel
# invocation are searched in the path LEMGEN_OPTIONS_PATH.
#
# <problem> is searched in the TPTP ($TPTPDirectory) and LEMGEN_PROBLEM_PATH.
#
# Both paths can specify mltiole directories separated with ":".
#
# Exit status:
# O: success, i.e., found proof
# 1: failure, i.e., found no proof
#
# ======================================================================
#
# Basic usage examples:
#
# $ provecd.sh LCL082-1 20 provecd_opts_sgcd_001.pl >/tmp/l1.pl
# $ provecd.sh LCL082-1 20 provecd_opts_ccs_001.pl >/tmp/l2.pl
# $ provecd.sh LCL082-1 20 provecd_opts_prover9_001.pl >/tmp/l3.pl
# $ provecd.sh LCL038-1 20 provecd_opts_sgcd_002.pl >/tmp/l4.pl
#   finds no proof
# $ provecd.sh LCL038-1 100 provecd_opts_sgcd_002.pl >/tmp/l5.pl
#   longer timeout, finds a proof
#
# Conversion to TPTP problem with lemmas:
# $ provecd.sh  -i ~/clouds/terms22/experiments/prolog_testfiles/lemmas1.pl -p "[file='/tmp/f1.p', signature=original]" LCL083-1 10 provecd_opts_tptp.pl 
#
# Calling EProver or Vampire with lemmas:
# $ provecd.sh  -i ~/clouds/terms22/experiments/prolog_testfiles/lemmas1.pl LCL083-1 10 provecd_opts_eprover_001.pl
#
# Too hard for CMProver in default setting:
# $ provecd.sh LCL083-1 10 provecd_opts_cmprover_003.pl
# But succeeds with some lemmas:
# $ provecd.sh -i ~/clouds/terms22/experiments/prolog_testfiles/lemmas1.pl LCL083-1 10 provecd_opts_cmprover_003.pl
# Also succeeds with a modified prover setting, supplied with -p:
# $ provecd.sh -p "[add_cm_options=[hs]-[hd1,r8(_)]]" LCL083-1 10 provecd_opts_cmprover_003.pl
#
# If there are s-lemmas in the proof, it can not be translated to a D-term. It is recommended to
# use then the 'system_proof_format' prover option. For CMProver the system detects this:
# $ provecd.sh -i ~/clouds/terms22/experiments/prolog_testfiles/lemmas2.pl LCL083-1 10 provecd_opts_cmprover_003.pl
#
# Also Prover9 works with lemmas:
# $ provecd.sh -i ~/clouds/terms22/experiments/prolog_testfiles/lemmas1.pl LCL083-1 10 provecd_opts_prover9_001.pl >/tmp/td1.pl
#
# SGCD can handle input lemmas in special ways, differently from axioms:
# $ provecd.sh -i ~/clouds/terms22/experiments/prolog_testfiles/some_lemmas_for_LCL038-1.pl LCL038-1 100 provecd_opts_sgcd_003.pl >/tmp/lx.pl
#

if [ -z ${TPTPDirectory+x} ]; then >&2 echo "TPTPDirectory not set"; exit 1; fi
if [ -z ${PIE+x} ]; then >&2 echo "PIE not set"; exit 1; fi
if [ -z ${CDTOOLS+x} ]; then >&2 echo "CDTOOLS not set"; exit 1; fi
if [ -z ${LEMGEN_OPTIONS_PATH+x} ]; then >&2 echo "LEMGEN_OPTIONS_PATH not set"; exit 1; fi

LEMMAS=none
OPTSP="[]"
OPTSL="[]"
OPTST="[]"
GPARGS=""

# quietly accept deprecated -f option
#
while getopts g:i:p:l:t:f o; do
    case $o in
	g) GPARGS=${OPTARG} ;;
	i) LEMMAS=${OPTARG} ;;
	p) OPTSP=${OPTARG} ;;
	l) OPTSL=${OPTARG} ;;
	t) OPTST=${OPTARG} ;;
    esac
done
shift "$((OPTIND - 1))"

PROBLEM=${1:?Argument 1 (problem) missing}
TIMEOUT=${2:?Argument 2 (prover timeout) missing}
OPTFILE=${3:?Argument 3 (options file or metafile) missing}

if [ "${OPTFILE: -3}" == ".pl" ]
then
    PLLL=0
else
    PLLL=1
fi

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"


if [ ${PLLL} -eq 0 ]
then
    swipl --quiet --no-tty --stack_limit=12g  \
	  -f ${PIE}/folelim/load.pl \
	  -s ${CDTOOLS}/src/cdtools/load.pl \
	  -g "consult('${PROLOGDIR}/provecd')" \
	  -g "provecd('${PROBLEM}',${TIMEOUT},'${OPTFILE}',\
      	  [halt,lemmas='${LEMMAS}',\
            opts_prover=${OPTSP},opts_lemmas=${OPTSL},opts_training=${OPTST}])"
else
    for f in `echo "$LEMGEN_OPTIONS_PATH" | tr ':' '\n'`
    do if [ -f $f/${OPTFILE} ]
       then
	   PLLLFILE=$f/${OPTFILE}
	   break
       fi
    done
    
    if [ -z ${PLLLFILE+x} ]
    then
	>&2 echo "Configuration file not found:" ${OPTFILE}
	exit 1
    fi

    >&2 echo '% Parallel configuration:' ${PLLLFILE}

    parallel ${GPARGS} --halt now,success=1 \
             ${SCRIPTDIR}/provecd.sh \
	         -i ${LEMMAS} -p "${OPTSP}" -l "${OPTSL}" -t "${OPTST}" \
	         ${PROBLEM} "${TIMEOUT}" {} \
	         :::: \
	         ${PLLLFILE}
fi

