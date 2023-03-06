import os
import re
import argparse
from glob import glob

expdict = {
    93: "CCS",
    87: "f_sgcd_basic",
    90: "f_cmprover",
    100: "vampire",
    103: "prover9",
    106: "eprover",
    141: "f_sgcd_basic-gnn",
    129: "leancop",
    
}

def parse_file(filename):
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if 'proof_dc' in line:
                return (True, line)
            elif "proof_external" in line:
                return (True, line)
            # elif "is a Theorem" in line:
            #     return (True, line)

        return (False, "")
    
def parse_experiment(expid, expdesc, outdir):
    print("EXP", expid, expdesc)
    base_files  = glob("out/exp" + str(expid) + '/0/train/**/*.pl', recursive=True)
    lemma_files  = glob("out/exp" + str(expid) + '/*/train/**/*.pl', recursive=True)
    expdesc2 = expdesc.replace(" ", "-")
    outfile = "{}/{}_{}.out".format(outdir, expid, expdesc)

    base_lines = []
    for f in base_files:
        success, line = parse_file(f)
        if success:
            base_lines.append(line)

    lemma_lines = []
    shortname_list = []
    for f in lemma_files:
        shortname = f.split("/train/")[1]
        if shortname not in shortname_list:
            success, line = parse_file(f)
            if success:
                lemma_lines.append(line)
                shortname_list.append(shortname)

    # base_lines = list(set(base_lines))
    # lemma_lines = list(set(lemma_lines))
    with open(outfile, 'w') as outf:
        print("experiment({},\"{}\").".format(expid, expdesc), file=outf)
        print("base({}).".format(len(base_lines)), file=outf)
        print("lemma({}).".format(len(lemma_lines)), file=outf)
        for line in sorted(base_lines):
            print("% BASE " + line, file=outf)
        for line in sorted(lemma_lines):
            print("% LEMMA " + line, file=outf)

              

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--outdir', dest="outdir", default="explogs", help="")
    args = parser.parse_args()

    
    if not os.path.exists(args.outdir):
        os.makedirs(args.outdir)

    for expid in expdict:
        parse_experiment(expid, expdict[expid], args.outdir)
