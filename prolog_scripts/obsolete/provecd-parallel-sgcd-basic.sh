#!/bin/bash
#
# SGCD-1+2+3+7, see:
# http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_2.html
#
# Requires LEMGEN_OPTIONS_PATH set up such that the option files will be found
#
# Usage examples:
# provecd-parallel-XXX.sh LCL083-1 100 >/tmp/spo.pl
# provecd-parallel-XXX.sh -i /tmp/x10.pl LCL083-1 100 >/tmp/spo.pl
#
parallel --halt now,success=1 provecd.sh $* {} ::: \
	 provecd_sgcd_s1.pl \
	 provecd_sgcd_s2.pl \
	 provecd_sgcd_s3.pl \
	 provecd_sgcd_s7.pl
