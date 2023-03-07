#!/bin/bash
#
# This script checks prerequisites for provecd.sh and lemgen.sh an writes its
# findings to stdout.
#

check_cmd () {
    if CMD="$(command -v $1)"
    then
	echo Found command: ${1} as ${CMD}
    else
	echo Missing command: ${1} ${2}
    fi
}

check_var () {
    if ! [ -z "${1+x}" ]
    then
	echo Found variable: ${1} set to ${!1}
    else
	echo Missing variable: ${1}
    fi
}

check_file () {
    if [ -f ${!1}/${2} ]
    then
	echo Found file: '$'${1}/${2}
    else
	echo Missing file: ${1}/${2}
    fi
}

echo "=============================================================================="
echo "Necessary Minimal Requirements"
echo "=============================================================================="

check_cmd swipl
if command -v swipl &>/dev/null
then
    SWIV="$(swipl --version)"
    echo Found version: ${SWIV}
    if ! [ ${SWIV:19:1} == 9 ]
    then
	echo "Possible problem: swipl version >= 9.0.3 is required"
    fi
fi
check_var PIE
check_file PIE folelim/load.pl
check_var CDTOOLS
check_file CDTOOLS src/cdtools/load.pl
check_var LEMGEN_OPTIONS_PATH
check_var LEMGEN_PROBLEM_PATH
check_var TPTPDirectory

echo "=============================================================================="
echo "Suggested Further Programs, Required for Full Functionality"
echo "=============================================================================="

check_cmd prover9 "Note: possibly compiles only after moving '-lm' in provers.src/Makefile to end of lines"
check_cmd prooftrans "Note: comes with Prover9"
check_cmd tptp2X "Note: comes with the TPTP, must be configured to support at least the tptp format"
check_cmd eprover
check_cmd vampire
check_cmd leancop.sh
check_cmd minisat
check_cmd TreeRePair "Source: https://github.com/dc0d32/TreeRePair"

