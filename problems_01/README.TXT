------------------------------------------------------------------------------
tc-problems

116 CD problems with a single axiom that are not in the TPTP but were
derived from TPTPCD problems by packing multiple axioms into a single
one with the Tarski/Rezus method.
------------------------------------------------------------------------------
special

90 CD problems with a single axiom:
(a) special_luk_...  Lukasiewicz as axiom (as in LCL038-1, LCL082-1, LCL083-1);
    all "theses" with only implication as theorems
    27 problems, in a sense easy: all could be solved by SGCD in 3600s
    timeout
(b) special_mer_cn_1953... Meredith's axiom (as in LCL073-1, LCL074-1,
    LCL075-1); all "theses" as theorems
    71 problems, some seem really hard
    some are discussed by [Wos2001] "Conquering the Meredith Axiom"

"Theses" refers to the 68 theses + 3 axioms from Lukasiewicz's 1929
book.  For these theses there are problems with syll,clavius,scotus as
theorems in the TPTP. In CD Tools they are registered in
named_axioms_cd.pl.

These were originally 98 problems, but 8 of them already appear in
the TPTP:

LCL038-1 : luk |- syll   thesis_01
LCL081-1 : luk |- id     thesis_16
LCL082-1 : luk |- simp   thesis_18
LCL083-1 : luk |- peirce thesis_24
LCL085-1 : luk |- pon    thesis_20
LCL075-1 : mer_cn_1953 |- scotus  thesis_03
LCL074-1 : mer_cn_1953 |- clavius thesis_02
LCL073-1 : mer_cn_1953 |- syll    thesis_01

------------------------------------------------------------------------------
