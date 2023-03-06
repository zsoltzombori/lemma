#!/bin/bash

# ============================================================================
# Usage:
#
# verifycd.sh <problem> <dterms>
#
# Verify CD proofs (Dterms) of a problem.
#
# <problem>: Problem specifier, e.g., TPTP problem name.
#
# <dterms>: Name of a file with D-terms or DC-terms, Prolog readable, terms
# concluded with ".".
#
# Exit status:
# O: success, i.e., all D-terms can be verified successfully
# 1: failure, i.e., at least one of the D-terms could not be
#             verified
#
# ============================================================================

if [ -z ${TPTPDirectory+x} ]; then >&2 echo "TPTPDirectory not set"; exit 1; fi
if [ -z ${PIE+x} ]; then >&2 echo "PIE not set"; exit 1; fi
if [ -z ${CDTOOLS+x} ]; then >&2 echo "CDTOOLS not set"; exit 1; fi

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"

PROBLEM=${1:?Argument 1 (problem) missing}
DTERMS=${2:?Argument 2 (file with proofs) missing}

swipl --quiet --no-tty --stack_limit=12g  \
      -f ${PIE}/folelim/load.pl \
      -s ${CDTOOLS}/src/cdtools/load.pl \
      -g "consult('${PROLOGDIR}/verifycd')" \
      -g "verify_cd_and_halt('${PROBLEM}','${DTERMS}')"


