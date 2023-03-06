#!/bin/bash
#
# Exists with error message if programs needed for full use of provecd.sh amd
# lemgen.sh are not found.
#

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

if ! command -v tptp2X &> /dev/null
then
    >&2 echo "ERROR: tptp2X command not found (must support at least the tptp format)"
    exit 1
fi
