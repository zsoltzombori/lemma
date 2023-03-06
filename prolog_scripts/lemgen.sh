#!/bin/bash

# The -o command line options supplement or override settings
# in the options file:
# 
# Usage examples:
#
# $ lemgen.sh LCL038-1 lemgen_opts_001.pl >/tmp/l1.pl
# writes lemmas and their features as spo/3 facts
#
# $ lemgen.sh LCL082-1 lemgen_opts_001.pl >/tmp/l2.pl
# finds a proof during lemma generation, writes no lemmas, just the proof
# as a commented fact
#
# $ lemgen.sh -o "[timeout=1]" LCL038-1 lemgen_opts_001.pl >/tmp/l1.pl
# overrides the method specific timeout settings in the options file
# 
# $ lemgen.sh -o "[timeout=5,processing=datagen,red=[subs,subt],lemma_methods=[subtree]]" LCL038-1 lg_tsize_optim.pl >/tmp/o1
# 
#   processing=lemgen (default): lemma generation
#   processing=datagen: generation of training data instead
#   processing=none: just the first phase of lemma computation,
#     e.g. for debugging
# 
#   red=[subs,subt]: optional reductions applied to the set of found
#       lemmas:
#       subs: removal of lemmas that are subsumed by another lemma in the set
#       subt: removal of lemmas that have a strict subformula
#             that is itself a lemma in the set (inspired by the "organic" property)
#

if [ -z ${TPTPDirectory+x} ]; then >&2 echo "TPTPDirectory not set"; exit 1; fi
if [ -z ${PIE+x} ]; then >&2 echo "PIE not set"; exit 1; fi
if [ -z ${CDTOOLS+x} ]; then >&2 echo "CDTOOLS not set"; exit 1; fi

CMDOPTS="[]"

while getopts o: o; do
    case $o in
	o) CMDOPTS=${OPTARG} ;;
    esac
done
shift "$((OPTIND - 1))"


SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"

PROBLEM=${1:?Argument 1 (problem) missing}
OPTFILE=${2:?Argument 2 (options file) missing}

swipl --quiet --no-tty --stack_limit=12g  \
      -f ${PIE}/folelim/load.pl \
      -s ${CDTOOLS}/src/cdtools/load.pl \
      -g "consult('${PROLOGDIR}/lemgen')" \
      -g "lemgen('${PROBLEM}','${OPTFILE}',${CMDOPTS})" \
      -g halt
