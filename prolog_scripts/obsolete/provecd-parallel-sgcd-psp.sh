#!/bin/bash
#
# Settings that generate by PSP, from
# http://cs.christophwernhard.com/cdtools/exp-tptpcd-2022-07/table_4.html
#
# Requires LEMGEN_OPTIONS_PATH set up such that the option files will be found
#
# Usage examples:
# provecd-parallel-XXX.sh LCL083-1 100 >/tmp/spo.pl
# provecd-parallel-XXX.sh -i /tmp/x10.pl LCL083-1 100 >/tmp/spo.pl
#
parallel --halt now,success=1 provecd.sh $* {} ::: \
	 provecd_sgcd_s9.pl \
	 provecd_sgcd_psp2.pl \
	 provecd_sgcd_psp3.pl \
	 provecd_sgcd_psp4.pl \
	 provecd_sgcd_psp5.pl
