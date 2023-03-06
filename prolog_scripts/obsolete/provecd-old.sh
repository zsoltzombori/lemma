#!/bin/bash

# ======================================================================
# Usage:
#
# provecd.sh [<cmdoptions>] <problem> <timeout> <optionsfile>
#
# CmdOptions:
#  -i <lemmafile>
#  -p <options>  additional prover options
#  -l <options>  additional lemma processing options
#  -t <options>  additional training data generation options
#
# Exit status:
# O: success, i.e., found proof
# 1: failure, i.e., found no proof
#
# ======================================================================
#
# Bassic usage examples:
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

if ! command -v TreeRePair &> /dev/null
then
    >&2 echo "ERROR: TreeRePair command not found, please install from https://github.com/dc0d32/TreeRePair"
    exit 1
fi

if ! command -v minisat &> /dev/null
then
    >&2 echo "ERROR: minisat command not found"
    exit 1
fi

if ! command -v prover9 &> /dev/null
then
    >&2 echo "ERROR: prover9 command not found"
    exit 1
fi

# Note on compiling Prover9 on recent (ca. 2021 and later) Linux systems: To
# avoid a compilation error change the positing of the -lm flag in
# provers.src/Makefile, for example 
# (CC) $(CFLAGS) -o prover9 prover9.o $(OBJECTS) ../ladr/libladr.a -lm
# instead of
# (CC) $(CFLAGS) -lm -o prover9 prover9.o $(OBJECTS) ../ladr/libladr.a

if ! command -v prooftrans &> /dev/null
then
    >&2 echo "ERROR: prooftrans command (comes with Prover9) not found"
    exit 1
fi

LEMMAS=none
OPTSP="[]"
OPTSL="[]"
OPTST="[]"

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"

while getopts i:p:l:t: o; do
    case $o in
	(i) LEMMAS=${OPTARG};;
	(p) OPTSP=${OPTARG};;
	(l) OPTSL=${OPTARG};;
	(t) OPTST=${OPTARG};;
    esac
done
shift "$((OPTIND - 1))"

PROBLEM=${1:?Argument 1 (problem) missing}
TIMEOUT=${2:?Argument 2 (prover timeout) missing}
OPTFILE=${3:?Argument 3 (options file) missing}

swipl --quiet --no-tty --stack_limit=12g  \
      -f ${PIE}/folelim/load.pl \
      -s ${CDTOOLS}/src/cdtools/load.pl \
      -g "consult('${PROLOGDIR}/provecd')" \
      -g "provecd('${PROBLEM}',${TIMEOUT},'${OPTFILE}',\
      [halt,lemmas='${LEMMAS}',\
      opts_prover=${OPTSP},opts_lemma=${OPTSL},opts_training=${OPTST}])"
