import re
import argparse

def parse_logs(expids):
    for expid in expids:
        print("EXP", expid)
        logfile = "log/exp{}.out".format(expid)
        with open(logfile, 'r') as f:
            even=True
            for line in f:
                line = line.strip()
                match = re.search(r'\%.*', line)
                if match:
                    print("   ", line)
                match = re.search(r'SOLVED ([0-9]+)', line)
                if match:
                    solved = match.group(1)
                    even=False
                match = re.search(r'TOTAL PROBLEMS SOLVED: ([0-9]+)', line)
                if match and not even:
                    total = match.group(1)
                    print("   SOLVED: {}, TOTAL: {}".format(solved, total))
                    even=True

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--exp', dest="exp", default=None, help="comma separated list of exp ids")
    args = parser.parse_args()

    expids = args.exp.split(",")
    
    parse_logs(expids)
