#!/bin/bash

# Usage examples:
#
# $ lemextract.sh pdb_short lemextract_opts_basic.pl >/tmp/lemmas_short.pl

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

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"

PROOFDB=${1:?Argument 1 (proof db) missing}
OPTFILE=${2:?Argument 2 (options file) missing}

swipl --quiet --no-tty --stack_limit=12g  \
      -f ${PIE}/folelim/load.pl \
      -s ${CDTOOLS}/src/cdtools/load.pl \
      -g "consult('${PROLOGDIR}/lemextract')" \
      -g "lemextract('${PROOFDB}','${OPTFILE}')" \
      -g halt
