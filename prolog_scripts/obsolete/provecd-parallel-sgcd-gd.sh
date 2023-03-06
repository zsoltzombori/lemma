#!/bin/bash
#
# Purely goal-driven settings by trees size and height, from
# http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_3.html
#
# Requires LEMGEN_OPTIONS_PATH set up such that the option files will be found
#
# Usage examples:
# provecd-parallel-XXX.sh LCL083-1 100 >/tmp/spo.pl
# provecd-parallel-XXX.sh -i /tmp/x10.pl LCL083-1 100 >/tmp/spo.pl
#
parallel --halt now,success=1 provecd.sh $* {} ::: \
	 provecd_sgcd_g1.pl \
	 provecd_sgcd_g2.pl
