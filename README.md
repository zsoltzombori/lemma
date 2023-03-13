# Lemmas: Generation, Selection, Application

## Overview

Noting that lemmas are a key feature of mathematics, we engage in an
investigation of the role of lemmas in automated theorem proving. This
repository contains a combined system involving learning technology
that generates useful lemmas for automated theorem provers,
demonstrating improvement for several representative systems and
solving a hard problem not solved by any system for twenty years. By
focusing on condensed detachment problems, we simplify the setting
considerably, allowing us to get at the essence of lemmas and their
role in proof search.

## Related Material

- Preprint: [Michael Rawson, Christoph Wernhard, Zsolt Zombori and Wolfgang Bibel:
  Lemmas: Generation, Selection, Application](https://arxiv.org/abs/2303.05854), 2023
- [Result table](http://cs.christophwernhard.com/cdtools/exp-lemmas/lemmas.html)

## Installation

To download the repository:

`git clone git@github.com:zsoltzombori/lemma.git`

The theorem proving components are written in SWI-Prolog and require the following openly available software components:

- [CD Tools](http://cs.christophwernhard.com/cdtools/)
  - Set the environment variable CDTOOLS to the root folder, e.g. `export CDTOOLS=${HOME}/tools/cdtools`
- [PIE](http://cs.christophwernhard.com/pie/)
  - Set the environment variable PIE to the /src/ folder, e.g. `export PIE=${HOME}/tools/pie/src/`
- [TPTP](https://tptp.org)
  - Set the environment variable TPTPDirectory to the root folder, e.g. `export TPTPDirectory=${HOME}/tools/tptp/TPTP-v8.1.2`

The script ./prolog_scripts/check_prerequisites.sh prints information about found and missing installation prerequisites.

It is recommended to add the prolog_scripts/ directory to PATH.

The learning components are written in Python and require pytorch. To create a conda environment with all necessary packages:

`conda env create -f environment.yml`

`conda activate lemma`

## Datasets
The problems sets used for all experiments are listed in `rated_problems.txt` The 196 TPTP problems are available at (https://tptp.org/cgi-bin/SeeTPTP?Category=Problems&Domain=LCL), and the remaining problems are in the `problems_01/tc-problems/` folder.

## Usage

### Candidate lemma generation

`./prolog_scripts/lemgen.sh <problem> <config_file> > <output_file>`

Writes lemmas and their features as spo/3 facts. E.g.:

`./prolog_scripts/lemgen.sh LCL038-1 lemgen_opts_001.pl >/tmp/l1.pl`

### Pretraining data extraction

`./prolog_scripts/lemgen.sh -o "[processing=datagen]" <problem> <config_file> > <output_file>`

E.g.:

`./prolog_scripts/lemgen.sh -o "[processing=datagen]" LCL038-1 lg_tsize_optim.pl >/tmp/o1`

### Invoking the theorem prover

`./prolog_scripts/provecd.sh [<cmdoptions>] <problem> <timeout> <optionsfile>

E.g.:

`./prolog_scripts/provecd.sh LCL082-1 20 provecd_opts_sgcd_001.pl >/tmp/l1.pl`

### Heuristic lemma ordering

`./prolog_scripts/spo-lemmas.sh -s <feature_list_for_ordering> -h <num_lemmas_to_select> <lemma_file> > output_file>

E.g.:

`./prolog_scripts/spo-lemmas.sh -s [lf_h_tsize] -h 20 /tmp/spo1.pl >/tmp/d2.pl`


### Experiments with learning

Each experiment that uses learning has a corresponding config bash file in the `exp\` directory. To reproduce an experiment, just launch this file, e.g.:

`bash exp/exp87.sh`



## Original Paper

```
@misc{rwzb:lemma,
  Author = {Michael Rawson, Christoph Wernhard, Zsolt Zombori, Wolfgang Bibel},
  Title = {Lemmas: Generation, Selection, Application},
  Year = {2023},
  note = {Preprint: \url{https://arxiv.org/abs/2303.05854}},
}
```
