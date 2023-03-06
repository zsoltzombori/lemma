#!/bin/bash
#
# Extracts the D-terms from all lemmas in an spo/3 file (as output by
# lemgen.sh) and writes them as terms suitable as input lemmas for provecd.sh.
# If available, it takes the DC-term instead of the D-term.
#
# CmdOptions:
#  -s <FeatureList>: Sort the output, according to the list of features (lexically,
#   based on SWI's standard term order.  For a numeric feature F, -F specifies that
#   its value times -1 is used.
#  -h <Number>: Return the <Number> first elements, or <Number>*-1 last elements if
#   <Number> is negative.
#
# Examples of usage:
# $ spo-lemmas.sh /tmp/spo1.pl >/tmp/d1.pl
# $ ./spo-lemmas.sh -s [lf_h_tsize] -h 20 /tmp/spo1.pl >/tmp/d2.pl
#

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"

LEMMAS=none
OPTSS="nil"
OPTSH="nil"

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROLOGDIR="$(dirname "${SCRIPTDIR}")/prolog"

while getopts s:h:t: o; do
    case $o in
	(s) OPTSS=${OPTARG};;
	(h) OPTSH=${OPTARG};;
    esac
done
shift "$((OPTIND - 1))"

FILES="'${1:?Argument 1 (file with spo/3 facts) missing}'"

shift 1

for point; do
    FILES="${FILES},'${point}'"
done

swipl --quiet --no-tty --stack_limit=12g  \
      -f ${PIE}/folelim/load.pl \
      -s ${CDTOOLS}/src/cdtools/load.pl \
      -g "consult('${PROLOGDIR}/spo_lemmas')" \
      -g "spo_lemmas([${FILES}],${OPTSS},${OPTSH})" \
      -g halt

