#!/bin/bash
#
# Requires LEMGEN_OPTIONS_PATH set up such that the option files
# provecd_cmprover_n9.pl etc will be found
#
# Usage examples:
# provecd-parallel-cmprover.sh LCL083-1 100 >/tmp/spo.pl
# provecd-parallel-cmprover.sh -i /tmp/x10.pl LCL083-1 100 >/tmp/spo.pl
#
parallel --halt now,success=1 provecd.sh $* {} ::: \
	 provecd_cmprover_n9.pl \
	 provecd_cmprover_n6.pl \
	 provecd_cmprover_n3.pl \
	 provecd_cmprover_n1.pl \
	 provecd_cmprover_n2.pl \
	 provecd_cmprover_n8.pl \
	 provecd_cmprover_n5.pl \
	 provecd_cmprover_n7.pl \
	 provecd_cmprover_n4.pl
