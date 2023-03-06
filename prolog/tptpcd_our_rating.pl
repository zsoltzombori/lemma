:- module(tptpcd_our_rating,
	  [rated_problem/3]).

:- use_module(tptpcd_problem_lists).


%! rated_problem(?Problem, ?OurRating, ?NumberOfAxioms) is nondet.
%
% Enumerate TPTPCD and single basis problems together with a rating
% value and their number of axioms.
%
% Enumeration order is by increasing rating.
%
% OurRating is a float between 0.0 and 1.0. It represents a difficulty rating
% in the context of our experiments. See code of
% rating_features_to_our_rating/2. It has the property that OurRating =< 0.5
% iff solved(_, Problem) suceeds, i.e., iff some proof of the problem could be
% among the training data.
%
rated_problem(Problem, OurRating, NumberOfAxioms) :-
	findall(R-P, (problem(P), problem_our_rating(P, R, _)), Ps),
	sort(Ps, Ps1),
	member(OurRating-Problem, Ps1),
	problem_number_of_axioms(Problem, NumberOfAxioms).

problem(Problem) :-
	( tptpcd_problem(Problem, _, _)
	; tptpcd_singlebasis(Problem)
	).

problem_our_rating(P, R, F) :-
	findall(F1, rating_feature(P, F1), F),
	rating_features_to_our_rating(F, R).

rating_features_to_our_rating(F, R) :-
	( memberchk(tptp_one, F) ->
	  R = 1.0
	; memberchk(solvable_sgcd_goaldriven, F) ->
	  R = 0.0
	; memberchk(solvable_sgcd, F) ->
	  R = 0.25
	; memberchk(solvable_prover9, F) ->
	  R = 0.5
	; memberchk(singlebasis_solvable_vampire, F) ->
	  R = 0.6
	; R = 0.75
	).

rating_feature(P, solvable_sgcd_goaldriven) :-
	( solved(sgcd_goaldriven_tsize, P) ->
	  true
	; solved(sgcd_goaldriven_height, P) ->
	  true
	; solved(singlebasis_goaldriven, P) ->
	  true
	).
rating_feature(P, solvable_prover9) :-
	( solved(prover9, P) ->
	  true
	).
rating_feature(P, solvable_sgcd) :-
	( solved(sgcd, P) ->
	  true
	; solved(singlebasis, P) ->
	  true
	).
%% In TPTP 8.1.0 there is a CD problem at 1.0 that was 0.75 before.
%% None of the CD problems that were 1.0 before is lower rated in TPTP 8.1.0.
%% So we use TPTP 8.0.0 to determine the 1.0 rated problems.
rating_feature(P, tptp_one) :-
	( tptpcd_problem(P, R, _), R >= 1 ->
	  true
	; tptpcd_singlebasis(P),
	  singlebasis_to_source_problem(P, P1),
	  tptpcd_problem(P1, R, _), R >= 1 ->
	  true
	).
rating_feature(P, singlebasis_solvable_vampire) :-
	( tptpcd_singlebasis(P),
	  singlebasis_to_source_problem(P, P1),
	  solved_singlebasis_vampire(P1, basis_to_theorem_std, V),
	  number(V) ->
	  true
	).

problem_number_of_axioms(Problem, N) :-
	( tptpcd_problem(Problem, _, N) ->
	  true
	; tptpcd_singlebasis(Problem) ->
	  N=1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

singlebasis_to_source_problem(P, P1) :-
	atom_concat('tc-problems/', P2, P),
	atom_concat(P1, '_basis_to_theorem_std.p', P2).

source_to_singlebasis_problem(P, P1) :-
	format(atom(P1), 'tc-problems/~w__basis_to_theorem_std.p', [P]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Problems with results in the files in proof_collection_01/
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


solved(sgcd, 'LCL006-1').
solved(sgcd, 'LCL007-1').
solved(sgcd, 'LCL008-1').
solved(sgcd, 'LCL009-1').
solved(sgcd, 'LCL010-1').
solved(sgcd, 'LCL011-1').
solved(sgcd, 'LCL012-1').
solved(sgcd, 'LCL013-1').
solved(sgcd, 'LCL014-1').
solved(sgcd, 'LCL015-1').
solved(sgcd, 'LCL016-1').
solved(sgcd, 'LCL017-1').
solved(sgcd, 'LCL018-1').
solved(sgcd, 'LCL019-1').
solved(sgcd, 'LCL021-1').
solved(sgcd, 'LCL022-1').
solved(sgcd, 'LCL023-1').
solved(sgcd, 'LCL024-1').
solved(sgcd, 'LCL025-1').
solved(sgcd, 'LCL026-1').
solved(sgcd, 'LCL027-1').
solved(sgcd, 'LCL028-1').
solved(sgcd, 'LCL029-1').
solved(sgcd, 'LCL030-1').
solved(sgcd, 'LCL031-1').
solved(sgcd, 'LCL032-1').
solved(sgcd, 'LCL033-1').
solved(sgcd, 'LCL034-1').
solved(sgcd, 'LCL035-1').
solved(sgcd, 'LCL036-1').
solved(sgcd, 'LCL038-1').
solved(sgcd, 'LCL040-1').
solved(sgcd, 'LCL041-1').
solved(sgcd, 'LCL042-1').
solved(sgcd, 'LCL043-1').
solved(sgcd, 'LCL044-1').
solved(sgcd, 'LCL045-1').
solved(sgcd, 'LCL046-1').
solved(sgcd, 'LCL047-1').
solved(sgcd, 'LCL048-1').
solved(sgcd, 'LCL049-1').
solved(sgcd, 'LCL050-1').
solved(sgcd, 'LCL051-1').
solved(sgcd, 'LCL052-1').
solved(sgcd, 'LCL053-1').
solved(sgcd, 'LCL054-1').
solved(sgcd, 'LCL055-1').
solved(sgcd, 'LCL056-1').
solved(sgcd, 'LCL057-1').
solved(sgcd, 'LCL058-1').
solved(sgcd, 'LCL059-1').
solved(sgcd, 'LCL060-1').
solved(sgcd, 'LCL061-1').
solved(sgcd, 'LCL062-1').
solved(sgcd, 'LCL064-1').
solved(sgcd, 'LCL064-2').
solved(sgcd, 'LCL065-1').
solved(sgcd, 'LCL066-1').
solved(sgcd, 'LCL067-1').
solved(sgcd, 'LCL068-1').
solved(sgcd, 'LCL069-1').
solved(sgcd, 'LCL070-1').
solved(sgcd, 'LCL071-1').
solved(sgcd, 'LCL072-1').
solved(sgcd, 'LCL074-1').
solved(sgcd, 'LCL075-1').
solved(sgcd, 'LCL076-1').
solved(sgcd, 'LCL076-2').
solved(sgcd, 'LCL077-1').
solved(sgcd, 'LCL079-1').
solved(sgcd, 'LCL080-1').
solved(sgcd, 'LCL080-2').
solved(sgcd, 'LCL081-1').
solved(sgcd, 'LCL082-1').
solved(sgcd, 'LCL083-1').
solved(sgcd, 'LCL083-2').
solved(sgcd, 'LCL084-2').
solved(sgcd, 'LCL084-3').
solved(sgcd, 'LCL085-1').
solved(sgcd, 'LCL086-1').
solved(sgcd, 'LCL087-1').
solved(sgcd, 'LCL088-1').
solved(sgcd, 'LCL089-1').
solved(sgcd, 'LCL090-1').
solved(sgcd, 'LCL091-1').
solved(sgcd, 'LCL092-1').
solved(sgcd, 'LCL093-1').
solved(sgcd, 'LCL094-1').
solved(sgcd, 'LCL095-1').
solved(sgcd, 'LCL096-1').
solved(sgcd, 'LCL097-1').
solved(sgcd, 'LCL098-1').
solved(sgcd, 'LCL099-1').
solved(sgcd, 'LCL100-1').
solved(sgcd, 'LCL101-1').
solved(sgcd, 'LCL102-1').
solved(sgcd, 'LCL103-1').
solved(sgcd, 'LCL104-1').
solved(sgcd, 'LCL106-1').
solved(sgcd, 'LCL107-1').
solved(sgcd, 'LCL108-1').
solved(sgcd, 'LCL110-1').
solved(sgcd, 'LCL111-1').
solved(sgcd, 'LCL112-1').
solved(sgcd, 'LCL113-1').
solved(sgcd, 'LCL114-1').
solved(sgcd, 'LCL115-1').
solved(sgcd, 'LCL116-1').
solved(sgcd, 'LCL117-1').
solved(sgcd, 'LCL118-1').
solved(sgcd, 'LCL120-1').
solved(sgcd, 'LCL121-1').
solved(sgcd, 'LCL122-1').
solved(sgcd, 'LCL123-1').
solved(sgcd, 'LCL126-1').
solved(sgcd, 'LCL127-1').
solved(sgcd, 'LCL128-1').
solved(sgcd, 'LCL129-1').
solved(sgcd, 'LCL130-1').
solved(sgcd, 'LCL131-1').
solved(sgcd, 'LCL166-1').
solved(sgcd, 'LCL167-1').
solved(sgcd, 'LCL256-1').
solved(sgcd, 'LCL257-1').
solved(sgcd, 'LCL355-1').
solved(sgcd, 'LCL356-1').
solved(sgcd, 'LCL357-1').
solved(sgcd, 'LCL358-1').
solved(sgcd, 'LCL359-1').
solved(sgcd, 'LCL360-1').
solved(sgcd, 'LCL361-1').
solved(sgcd, 'LCL362-1').
solved(sgcd, 'LCL363-1').
solved(sgcd, 'LCL364-1').
solved(sgcd, 'LCL365-1').
solved(sgcd, 'LCL366-1').
solved(sgcd, 'LCL367-1').
solved(sgcd, 'LCL368-1').
solved(sgcd, 'LCL369-1').
solved(sgcd, 'LCL370-1').
solved(sgcd, 'LCL371-1').
solved(sgcd, 'LCL372-1').
solved(sgcd, 'LCL373-1').
solved(sgcd, 'LCL374-1').
solved(sgcd, 'LCL375-1').
solved(sgcd, 'LCL376-1').
solved(sgcd, 'LCL377-1').
solved(sgcd, 'LCL378-1').
solved(sgcd, 'LCL379-1').
solved(sgcd, 'LCL380-1').
solved(sgcd, 'LCL381-1').
solved(sgcd, 'LCL382-1').
solved(sgcd, 'LCL383-1').
solved(sgcd, 'LCL384-1').
solved(sgcd, 'LCL385-1').
solved(sgcd, 'LCL386-1').
solved(sgcd, 'LCL387-1').
solved(sgcd, 'LCL388-1').
solved(sgcd, 'LCL389-1').
solved(sgcd, 'LCL390-1').
solved(sgcd, 'LCL391-1').
solved(sgcd, 'LCL392-1').
solved(sgcd, 'LCL393-1').
solved(sgcd, 'LCL394-1').
solved(sgcd, 'LCL395-1').
solved(sgcd, 'LCL396-1').
solved(sgcd, 'LCL397-1').
solved(sgcd, 'LCL398-1').
solved(sgcd, 'LCL399-1').
solved(sgcd, 'LCL400-1').
solved(sgcd, 'LCL401-1').
solved(sgcd, 'LCL402-1').
solved(sgcd, 'LCL403-1').
solved(sgcd, 'LCL404-1').
solved(sgcd, 'LCL405-1').
solved(sgcd, 'LCL416-1').

solved(sgcd_goaldriven_tsize, 'LCL006-1').
solved(sgcd_goaldriven_tsize, 'LCL007-1').
solved(sgcd_goaldriven_tsize, 'LCL008-1').
solved(sgcd_goaldriven_tsize, 'LCL009-1').
solved(sgcd_goaldriven_tsize, 'LCL010-1').
solved(sgcd_goaldriven_tsize, 'LCL011-1').
solved(sgcd_goaldriven_tsize, 'LCL013-1').
solved(sgcd_goaldriven_tsize, 'LCL014-1').
solved(sgcd_goaldriven_tsize, 'LCL022-1').
solved(sgcd_goaldriven_tsize, 'LCL023-1').
solved(sgcd_goaldriven_tsize, 'LCL024-1').
solved(sgcd_goaldriven_tsize, 'LCL025-1').
solved(sgcd_goaldriven_tsize, 'LCL027-1').
solved(sgcd_goaldriven_tsize, 'LCL029-1').
solved(sgcd_goaldriven_tsize, 'LCL033-1').
solved(sgcd_goaldriven_tsize, 'LCL035-1').
solved(sgcd_goaldriven_tsize, 'LCL040-1').
solved(sgcd_goaldriven_tsize, 'LCL041-1').
solved(sgcd_goaldriven_tsize, 'LCL042-1').
solved(sgcd_goaldriven_tsize, 'LCL043-1').
solved(sgcd_goaldriven_tsize, 'LCL044-1').
solved(sgcd_goaldriven_tsize, 'LCL045-1').
solved(sgcd_goaldriven_tsize, 'LCL046-1').
solved(sgcd_goaldriven_tsize, 'LCL064-1').
solved(sgcd_goaldriven_tsize, 'LCL064-2').
solved(sgcd_goaldriven_tsize, 'LCL065-1').
solved(sgcd_goaldriven_tsize, 'LCL066-1').
solved(sgcd_goaldriven_tsize, 'LCL069-1').
solved(sgcd_goaldriven_tsize, 'LCL072-1').
solved(sgcd_goaldriven_tsize, 'LCL075-1').
solved(sgcd_goaldriven_tsize, 'LCL076-1').
solved(sgcd_goaldriven_tsize, 'LCL076-2').
solved(sgcd_goaldriven_tsize, 'LCL077-1').
solved(sgcd_goaldriven_tsize, 'LCL079-1').
solved(sgcd_goaldriven_tsize, 'LCL080-1').
solved(sgcd_goaldriven_tsize, 'LCL080-2').
solved(sgcd_goaldriven_tsize, 'LCL081-1').
solved(sgcd_goaldriven_tsize, 'LCL082-1').
solved(sgcd_goaldriven_tsize, 'LCL083-1').
solved(sgcd_goaldriven_tsize, 'LCL083-2').
solved(sgcd_goaldriven_tsize, 'LCL086-1').
solved(sgcd_goaldriven_tsize, 'LCL087-1').
solved(sgcd_goaldriven_tsize, 'LCL088-1').
solved(sgcd_goaldriven_tsize, 'LCL090-1').
solved(sgcd_goaldriven_tsize, 'LCL091-1').
solved(sgcd_goaldriven_tsize, 'LCL092-1').
solved(sgcd_goaldriven_tsize, 'LCL095-1').
solved(sgcd_goaldriven_tsize, 'LCL096-1').
solved(sgcd_goaldriven_tsize, 'LCL097-1').
solved(sgcd_goaldriven_tsize, 'LCL098-1').
solved(sgcd_goaldriven_tsize, 'LCL101-1').
solved(sgcd_goaldriven_tsize, 'LCL102-1').
solved(sgcd_goaldriven_tsize, 'LCL104-1').
solved(sgcd_goaldriven_tsize, 'LCL106-1').
solved(sgcd_goaldriven_tsize, 'LCL107-1').
solved(sgcd_goaldriven_tsize, 'LCL108-1').
solved(sgcd_goaldriven_tsize, 'LCL110-1').
solved(sgcd_goaldriven_tsize, 'LCL111-1').
solved(sgcd_goaldriven_tsize, 'LCL112-1').
solved(sgcd_goaldriven_tsize, 'LCL117-1').
solved(sgcd_goaldriven_tsize, 'LCL118-1').
solved(sgcd_goaldriven_tsize, 'LCL120-1').
solved(sgcd_goaldriven_tsize, 'LCL126-1').
solved(sgcd_goaldriven_tsize, 'LCL130-1').
solved(sgcd_goaldriven_tsize, 'LCL257-1').
solved(sgcd_goaldriven_tsize, 'LCL355-1').
solved(sgcd_goaldriven_tsize, 'LCL356-1').
solved(sgcd_goaldriven_tsize, 'LCL357-1').
solved(sgcd_goaldriven_tsize, 'LCL358-1').
solved(sgcd_goaldriven_tsize, 'LCL359-1').
solved(sgcd_goaldriven_tsize, 'LCL360-1').
solved(sgcd_goaldriven_tsize, 'LCL361-1').
solved(sgcd_goaldriven_tsize, 'LCL362-1').
solved(sgcd_goaldriven_tsize, 'LCL363-1').
solved(sgcd_goaldriven_tsize, 'LCL364-1').
solved(sgcd_goaldriven_tsize, 'LCL365-1').
solved(sgcd_goaldriven_tsize, 'LCL366-1').
solved(sgcd_goaldriven_tsize, 'LCL397-1').
solved(sgcd_goaldriven_tsize, 'LCL398-1').
solved(sgcd_goaldriven_tsize, 'LCL399-1').
solved(sgcd_goaldriven_tsize, 'LCL416-1').

solved(sgcd_goaldriven_height, 'LCL006-1').
solved(sgcd_goaldriven_height, 'LCL007-1').
solved(sgcd_goaldriven_height, 'LCL008-1').
solved(sgcd_goaldriven_height, 'LCL009-1').
solved(sgcd_goaldriven_height, 'LCL010-1').
solved(sgcd_goaldriven_height, 'LCL011-1').
solved(sgcd_goaldriven_height, 'LCL013-1').
solved(sgcd_goaldriven_height, 'LCL022-1').
solved(sgcd_goaldriven_height, 'LCL023-1').
solved(sgcd_goaldriven_height, 'LCL027-1').
solved(sgcd_goaldriven_height, 'LCL029-1').
solved(sgcd_goaldriven_height, 'LCL030-1').
solved(sgcd_goaldriven_height, 'LCL033-1').
solved(sgcd_goaldriven_height, 'LCL035-1').
solved(sgcd_goaldriven_height, 'LCL041-1').
solved(sgcd_goaldriven_height, 'LCL042-1').
solved(sgcd_goaldriven_height, 'LCL043-1').
solved(sgcd_goaldriven_height, 'LCL044-1').
solved(sgcd_goaldriven_height, 'LCL045-1').
solved(sgcd_goaldriven_height, 'LCL046-1').
solved(sgcd_goaldriven_height, 'LCL047-1').
solved(sgcd_goaldriven_height, 'LCL059-1').
solved(sgcd_goaldriven_height, 'LCL064-2').
solved(sgcd_goaldriven_height, 'LCL069-1').
solved(sgcd_goaldriven_height, 'LCL070-1').
solved(sgcd_goaldriven_height, 'LCL072-1').
solved(sgcd_goaldriven_height, 'LCL076-2').
solved(sgcd_goaldriven_height, 'LCL079-1').
solved(sgcd_goaldriven_height, 'LCL081-1').
solved(sgcd_goaldriven_height, 'LCL082-1').
solved(sgcd_goaldriven_height, 'LCL096-1').
solved(sgcd_goaldriven_height, 'LCL097-1').
solved(sgcd_goaldriven_height, 'LCL098-1').
solved(sgcd_goaldriven_height, 'LCL101-1').
solved(sgcd_goaldriven_height, 'LCL102-1').
solved(sgcd_goaldriven_height, 'LCL103-1').
solved(sgcd_goaldriven_height, 'LCL104-1').
solved(sgcd_goaldriven_height, 'LCL106-1').
solved(sgcd_goaldriven_height, 'LCL107-1').
solved(sgcd_goaldriven_height, 'LCL108-1').
solved(sgcd_goaldriven_height, 'LCL110-1').
solved(sgcd_goaldriven_height, 'LCL111-1').
solved(sgcd_goaldriven_height, 'LCL117-1').
solved(sgcd_goaldriven_height, 'LCL118-1').
solved(sgcd_goaldriven_height, 'LCL120-1').
solved(sgcd_goaldriven_height, 'LCL123-1').
solved(sgcd_goaldriven_height, 'LCL126-1').
solved(sgcd_goaldriven_height, 'LCL130-1').
solved(sgcd_goaldriven_height, 'LCL257-1').
solved(sgcd_goaldriven_height, 'LCL355-1').
solved(sgcd_goaldriven_height, 'LCL356-1').
solved(sgcd_goaldriven_height, 'LCL357-1').
solved(sgcd_goaldriven_height, 'LCL358-1').
solved(sgcd_goaldriven_height, 'LCL359-1').
solved(sgcd_goaldriven_height, 'LCL360-1').
solved(sgcd_goaldriven_height, 'LCL361-1').
solved(sgcd_goaldriven_height, 'LCL362-1').
solved(sgcd_goaldriven_height, 'LCL363-1').
solved(sgcd_goaldriven_height, 'LCL364-1').
solved(sgcd_goaldriven_height, 'LCL365-1').
solved(sgcd_goaldriven_height, 'LCL366-1').
solved(sgcd_goaldriven_height, 'LCL367-1').
solved(sgcd_goaldriven_height, 'LCL384-1').
solved(sgcd_goaldriven_height, 'LCL397-1').
solved(sgcd_goaldriven_height, 'LCL398-1').

solved(ccs_pure_dterms, 'LCL006-1').
solved(ccs_pure_dterms, 'LCL007-1').
solved(ccs_pure_dterms, 'LCL008-1').
solved(ccs_pure_dterms, 'LCL009-1').
solved(ccs_pure_dterms, 'LCL010-1').
solved(ccs_pure_dterms, 'LCL011-1').
solved(ccs_pure_dterms, 'LCL013-1').
solved(ccs_pure_dterms, 'LCL014-1').
solved(ccs_pure_dterms, 'LCL022-1').
solved(ccs_pure_dterms, 'LCL023-1').
solved(ccs_pure_dterms, 'LCL024-1').
solved(ccs_pure_dterms, 'LCL025-1').
solved(ccs_pure_dterms, 'LCL027-1').
solved(ccs_pure_dterms, 'LCL029-1').
solved(ccs_pure_dterms, 'LCL030-1').
solved(ccs_pure_dterms, 'LCL033-1').
solved(ccs_pure_dterms, 'LCL035-1').
solved(ccs_pure_dterms, 'LCL036-1').
solved(ccs_pure_dterms, 'LCL040-1').
solved(ccs_pure_dterms, 'LCL041-1').
solved(ccs_pure_dterms, 'LCL042-1').
solved(ccs_pure_dterms, 'LCL043-1').
solved(ccs_pure_dterms, 'LCL044-1').
solved(ccs_pure_dterms, 'LCL045-1').
solved(ccs_pure_dterms, 'LCL046-1').
solved(ccs_pure_dterms, 'LCL064-1').
solved(ccs_pure_dterms, 'LCL064-2').
solved(ccs_pure_dterms, 'LCL065-1').
solved(ccs_pure_dterms, 'LCL066-1').
solved(ccs_pure_dterms, 'LCL067-1').
solved(ccs_pure_dterms, 'LCL069-1').
solved(ccs_pure_dterms, 'LCL070-1').
solved(ccs_pure_dterms, 'LCL072-1').
solved(ccs_pure_dterms, 'LCL075-1').
solved(ccs_pure_dterms, 'LCL076-1').
solved(ccs_pure_dterms, 'LCL076-2').
solved(ccs_pure_dterms, 'LCL077-1').
solved(ccs_pure_dterms, 'LCL079-1').
solved(ccs_pure_dterms, 'LCL080-1').
solved(ccs_pure_dterms, 'LCL080-2').
solved(ccs_pure_dterms, 'LCL081-1').
solved(ccs_pure_dterms, 'LCL082-1').
solved(ccs_pure_dterms, 'LCL083-1').
solved(ccs_pure_dterms, 'LCL083-2').
solved(ccs_pure_dterms, 'LCL086-1').
solved(ccs_pure_dterms, 'LCL087-1').
solved(ccs_pure_dterms, 'LCL088-1').
solved(ccs_pure_dterms, 'LCL089-1').
solved(ccs_pure_dterms, 'LCL090-1').
solved(ccs_pure_dterms, 'LCL091-1').
solved(ccs_pure_dterms, 'LCL092-1').
solved(ccs_pure_dterms, 'LCL094-1').
solved(ccs_pure_dterms, 'LCL095-1').
solved(ccs_pure_dterms, 'LCL096-1').
solved(ccs_pure_dterms, 'LCL097-1').
solved(ccs_pure_dterms, 'LCL098-1').
solved(ccs_pure_dterms, 'LCL101-1').
solved(ccs_pure_dterms, 'LCL102-1').
solved(ccs_pure_dterms, 'LCL103-1').
solved(ccs_pure_dterms, 'LCL104-1').
solved(ccs_pure_dterms, 'LCL106-1').
solved(ccs_pure_dterms, 'LCL107-1').
solved(ccs_pure_dterms, 'LCL108-1').
solved(ccs_pure_dterms, 'LCL110-1').
solved(ccs_pure_dterms, 'LCL111-1').
solved(ccs_pure_dterms, 'LCL112-1').
solved(ccs_pure_dterms, 'LCL117-1').
solved(ccs_pure_dterms, 'LCL118-1').
solved(ccs_pure_dterms, 'LCL120-1').
solved(ccs_pure_dterms, 'LCL121-1').
solved(ccs_pure_dterms, 'LCL122-1').
solved(ccs_pure_dterms, 'LCL123-1').
solved(ccs_pure_dterms, 'LCL126-1').
solved(ccs_pure_dterms, 'LCL128-1').
solved(ccs_pure_dterms, 'LCL129-1').
solved(ccs_pure_dterms, 'LCL130-1').
solved(ccs_pure_dterms, 'LCL131-1').
solved(ccs_pure_dterms, 'LCL257-1').
solved(ccs_pure_dterms, 'LCL355-1').
solved(ccs_pure_dterms, 'LCL356-1').
solved(ccs_pure_dterms, 'LCL357-1').
solved(ccs_pure_dterms, 'LCL358-1').
solved(ccs_pure_dterms, 'LCL359-1').
solved(ccs_pure_dterms, 'LCL360-1').
solved(ccs_pure_dterms, 'LCL361-1').
solved(ccs_pure_dterms, 'LCL362-1').
solved(ccs_pure_dterms, 'LCL363-1').
solved(ccs_pure_dterms, 'LCL364-1').
solved(ccs_pure_dterms, 'LCL365-1').
solved(ccs_pure_dterms, 'LCL366-1').
solved(ccs_pure_dterms, 'LCL384-1').
solved(ccs_pure_dterms, 'LCL397-1').
solved(ccs_pure_dterms, 'LCL398-1').
solved(ccs_pure_dterms, 'LCL416-1').

solved(prover9, 'LCL006-1').
solved(prover9, 'LCL007-1').
solved(prover9, 'LCL008-1').
solved(prover9, 'LCL009-1').
solved(prover9, 'LCL010-1').
solved(prover9, 'LCL011-1').
solved(prover9, 'LCL012-1').
solved(prover9, 'LCL013-1').
solved(prover9, 'LCL014-1').
solved(prover9, 'LCL015-1').
solved(prover9, 'LCL016-1').
solved(prover9, 'LCL017-1').
solved(prover9, 'LCL018-1').
solved(prover9, 'LCL019-1').
solved(prover9, 'LCL020-1').
solved(prover9, 'LCL021-1').
solved(prover9, 'LCL022-1').
solved(prover9, 'LCL023-1').
solved(prover9, 'LCL024-1').
solved(prover9, 'LCL025-1').
solved(prover9, 'LCL026-1').
solved(prover9, 'LCL027-1').
solved(prover9, 'LCL028-1').
solved(prover9, 'LCL029-1').
solved(prover9, 'LCL030-1').
solved(prover9, 'LCL031-1').
solved(prover9, 'LCL032-1').
solved(prover9, 'LCL033-1').
solved(prover9, 'LCL034-1').
solved(prover9, 'LCL035-1').
solved(prover9, 'LCL036-1').
solved(prover9, 'LCL037-1').
solved(prover9, 'LCL038-1').
solved(prover9, 'LCL040-1').
solved(prover9, 'LCL041-1').
solved(prover9, 'LCL042-1').
solved(prover9, 'LCL043-1').
solved(prover9, 'LCL044-1').
solved(prover9, 'LCL045-1').
solved(prover9, 'LCL046-1').
solved(prover9, 'LCL047-1').
solved(prover9, 'LCL048-1').
solved(prover9, 'LCL049-1').
solved(prover9, 'LCL050-1').
solved(prover9, 'LCL051-1').
solved(prover9, 'LCL052-1').
solved(prover9, 'LCL053-1').
solved(prover9, 'LCL054-1').
solved(prover9, 'LCL055-1').
solved(prover9, 'LCL056-1').
solved(prover9, 'LCL057-1').
solved(prover9, 'LCL058-1').
solved(prover9, 'LCL059-1').
solved(prover9, 'LCL060-1').
solved(prover9, 'LCL064-1').
solved(prover9, 'LCL064-2').
solved(prover9, 'LCL065-1').
solved(prover9, 'LCL066-1').
solved(prover9, 'LCL067-1').
solved(prover9, 'LCL068-1').
solved(prover9, 'LCL069-1').
solved(prover9, 'LCL070-1').
solved(prover9, 'LCL071-1').
solved(prover9, 'LCL072-1').
solved(prover9, 'LCL075-1').
solved(prover9, 'LCL076-1').
solved(prover9, 'LCL076-2').
solved(prover9, 'LCL077-1').
solved(prover9, 'LCL079-1').
solved(prover9, 'LCL080-1').
solved(prover9, 'LCL080-2').
solved(prover9, 'LCL081-1').
solved(prover9, 'LCL082-1').
solved(prover9, 'LCL083-1').
solved(prover9, 'LCL083-2').
solved(prover9, 'LCL084-2').
solved(prover9, 'LCL084-3').
solved(prover9, 'LCL085-1').
solved(prover9, 'LCL086-1').
solved(prover9, 'LCL087-1').
solved(prover9, 'LCL088-1').
solved(prover9, 'LCL089-1').
solved(prover9, 'LCL090-1').
solved(prover9, 'LCL091-1').
solved(prover9, 'LCL092-1').
solved(prover9, 'LCL093-1').
solved(prover9, 'LCL094-1').
solved(prover9, 'LCL095-1').
solved(prover9, 'LCL096-1').
solved(prover9, 'LCL097-1').
solved(prover9, 'LCL098-1').
solved(prover9, 'LCL100-1').
solved(prover9, 'LCL101-1').
solved(prover9, 'LCL102-1').
solved(prover9, 'LCL103-1').
solved(prover9, 'LCL104-1').
solved(prover9, 'LCL105-1').
solved(prover9, 'LCL106-1').
solved(prover9, 'LCL107-1').
solved(prover9, 'LCL108-1').
solved(prover9, 'LCL110-1').
solved(prover9, 'LCL111-1').
solved(prover9, 'LCL112-1').
solved(prover9, 'LCL113-1').
solved(prover9, 'LCL114-1').
solved(prover9, 'LCL115-1').
solved(prover9, 'LCL116-1').
solved(prover9, 'LCL117-1').
solved(prover9, 'LCL118-1').
solved(prover9, 'LCL119-1').
solved(prover9, 'LCL120-1').
solved(prover9, 'LCL121-1').
solved(prover9, 'LCL122-1').
solved(prover9, 'LCL123-1').
solved(prover9, 'LCL125-1').
solved(prover9, 'LCL126-1').
solved(prover9, 'LCL127-1').
solved(prover9, 'LCL128-1').
solved(prover9, 'LCL129-1').
solved(prover9, 'LCL130-1').
solved(prover9, 'LCL131-1').
solved(prover9, 'LCL166-1').
solved(prover9, 'LCL167-1').
solved(prover9, 'LCL256-1').
solved(prover9, 'LCL257-1').
solved(prover9, 'LCL355-1').
solved(prover9, 'LCL356-1').
solved(prover9, 'LCL357-1').
solved(prover9, 'LCL358-1').
solved(prover9, 'LCL359-1').
solved(prover9, 'LCL360-1').
solved(prover9, 'LCL361-1').
solved(prover9, 'LCL362-1').
solved(prover9, 'LCL363-1').
solved(prover9, 'LCL364-1').
solved(prover9, 'LCL366-1').
solved(prover9, 'LCL367-1').
solved(prover9, 'LCL368-1').
solved(prover9, 'LCL369-1').
solved(prover9, 'LCL370-1').
solved(prover9, 'LCL371-1').
solved(prover9, 'LCL373-1').
solved(prover9, 'LCL378-1').
solved(prover9, 'LCL379-1').
solved(prover9, 'LCL380-1').
solved(prover9, 'LCL381-1').
solved(prover9, 'LCL382-1').
solved(prover9, 'LCL384-1').
solved(prover9, 'LCL385-1').
solved(prover9, 'LCL386-1').
solved(prover9, 'LCL387-1').
solved(prover9, 'LCL388-1').
solved(prover9, 'LCL389-1').
solved(prover9, 'LCL390-1').
solved(prover9, 'LCL391-1').
solved(prover9, 'LCL392-1').
solved(prover9, 'LCL393-1').
solved(prover9, 'LCL396-1').
solved(prover9, 'LCL397-1').
solved(prover9, 'LCL398-1').
solved(prover9, 'LCL399-1').
solved(prover9, 'LCL400-1').
solved(prover9, 'LCL401-1').
solved(prover9, 'LCL402-1').
solved(prover9, 'LCL403-1').
solved(prover9, 'LCL404-1').
solved(prover9, 'LCL405-1').
solved(prover9, 'LCL416-1').

% Results for 'singlebasis' were obtained with SGCD with options:
% opts/opts13.pl
% opts/opts17.pl
% opts/opts20.pl
% opts/opts33.pl
% opts/opts3.pl
% opts/opts6.pl

solved(singlebasis, 'tc-problems/LCL025-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL026-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL027-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL028-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL029-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL030-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL031-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL040-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL041-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL042-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL043-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL044-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL045-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL046-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL047-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL048-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL049-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL050-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL051-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL052-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL053-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL054-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL055-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL056-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL057-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL058-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL059-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL060-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL061-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL064-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL064-2_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL065-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL066-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL067-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL068-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL069-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL070-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL071-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL072-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL076-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL076-2_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL077-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL079-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL080-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL080-2_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL083-2_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL084-2_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL084-3_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL110-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL111-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL112-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL113-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL114-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL115-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL116-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL256-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL355-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL356-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL357-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL358-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL359-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL360-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL361-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL362-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL363-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL364-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL365-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL366-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL367-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL368-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL369-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL370-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL371-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL372-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL373-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL374-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL375-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL376-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL377-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL378-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL379-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL380-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL381-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL382-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL383-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL384-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL385-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL386-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL387-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL388-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL389-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL390-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL391-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL392-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL393-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL395-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL396-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL397-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL398-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL399-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL400-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL401-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL402-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL403-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL404-1_basis_to_theorem_std.p').
solved(singlebasis, 'tc-problems/LCL405-1_basis_to_theorem_std.p').

solved(singlebasis_goaldriven, 'tc-problems/LCL027-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL046-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL047-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL048-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL076-2_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL360-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL361-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL362-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL363-1_basis_to_theorem_std.p').
solved(singlebasis_goaldriven, 'tc-problems/LCL367-1_basis_to_theorem_std.p').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Results by Vampire on the singlebasis problems, including
%%%% related problems.
%%%% 
%%%% Vampire version:
%%%% Vampire 4.5.1 (commit 57a6f78c on 2020-07-15 11:59:04 +0200)
%%%% On the HPC System with 'Intel(R) Xeon(R) Platinum 9242 CPU @ 2.30GHz'
%%%% Timout 2400 sec
%%%% 
solved_singlebasis_vampire('LCL006-1', axioms_to_d, 0.776 ).
solved_singlebasis_vampire('LCL025-1', axioms_to_basis_with_kd, 0.185 ).
solved_singlebasis_vampire('LCL025-1', axioms_to_d, 1.928 ).
solved_singlebasis_vampire('LCL025-1', axioms_to_k, 0.025 ).
solved_singlebasis_vampire('LCL025-1', basis_to_axiom_1_std, 0.027 ).
solved_singlebasis_vampire('LCL025-1', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL025-1', basis_to_axiom_3_std, 0.03 ).
solved_singlebasis_vampire('LCL025-1', basis_to_theorem_std, 7.104 ).
solved_singlebasis_vampire('LCL026-1', basis_to_theorem_std, 7.137 ).
solved_singlebasis_vampire('LCL027-1', basis_to_theorem_std, 0.041 ).
solved_singlebasis_vampire('LCL028-1', basis_to_theorem_std, 288.734 ).
solved_singlebasis_vampire('LCL029-1', axioms_to_basis_with_kd, 32.87 ).
solved_singlebasis_vampire('LCL029-1', axioms_to_d, 1.842 ).
solved_singlebasis_vampire('LCL029-1', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL029-1', basis_to_axiom_1_std, 0.043 ).
solved_singlebasis_vampire('LCL029-1', basis_to_axiom_2_std, 0.025 ).
solved_singlebasis_vampire('LCL029-1', basis_to_axiom_3_std, 0.028 ).
solved_singlebasis_vampire('LCL029-1', basis_to_axiom_4_std, 0.029 ).
solved_singlebasis_vampire('LCL029-1', basis_to_theorem_std, 1.858 ).
solved_singlebasis_vampire('LCL030-1', basis_to_theorem_std, 254.797 ).
solved_singlebasis_vampire('LCL031-1', basis_to_theorem_std, 228.019 ).
solved_singlebasis_vampire('LCL040-1', axioms_to_basis_with_kd, 216.619 ).
solved_singlebasis_vampire('LCL040-1', axioms_to_d, 1.922 ).
solved_singlebasis_vampire('LCL040-1', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL040-1', basis_to_axiom_1_std, 0.025 ).
solved_singlebasis_vampire('LCL040-1', basis_to_axiom_2_std, 1.944 ).
solved_singlebasis_vampire('LCL040-1', basis_to_axiom_3_std, 0.082 ).
solved_singlebasis_vampire('LCL040-1', basis_to_axiom_4_std, 0.027 ).
solved_singlebasis_vampire('LCL040-1', basis_to_axiom_5_std, 0.031 ).
solved_singlebasis_vampire('LCL040-1', basis_to_theorem_std, 231.222 ).
solved_singlebasis_vampire('LCL041-1', axioms_to_basis_with_kd, 41.126 ).
solved_singlebasis_vampire('LCL041-1', axioms_to_d, 0.036 ).
solved_singlebasis_vampire('LCL041-1', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL041-1', basis_to_axiom_1_std, 0.237 ).
solved_singlebasis_vampire('LCL041-1', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL041-1', basis_to_axiom_3_std, 0.089 ).
solved_singlebasis_vampire('LCL041-1', basis_to_axiom_4_std, 0.027 ).
solved_singlebasis_vampire('LCL041-1', basis_to_axiom_5_std, 0.03 ).
solved_singlebasis_vampire('LCL041-1', basis_to_theorem_std, 0.292 ).
solved_singlebasis_vampire('LCL042-1', basis_to_theorem_std, 36.83 ).
solved_singlebasis_vampire('LCL043-1', basis_to_theorem_std, 0.125 ).
solved_singlebasis_vampire('LCL044-1', basis_to_theorem_std, 0.221 ).
solved_singlebasis_vampire('LCL045-1', basis_to_theorem_std, 37.418 ).
solved_singlebasis_vampire('LCL046-1', axioms_to_basis_with_kd, 0.171 ).
solved_singlebasis_vampire('LCL046-1', axioms_to_d, 7.674 ).
solved_singlebasis_vampire('LCL046-1', axioms_to_k, 6.408 ).
solved_singlebasis_vampire('LCL046-1', basis_to_axiom_1_std, 0.126 ).
solved_singlebasis_vampire('LCL046-1', basis_to_axiom_2_std, 0.028 ).
solved_singlebasis_vampire('LCL046-1', basis_to_axiom_3_std, 0.029 ).
solved_singlebasis_vampire('LCL046-1', basis_to_theorem_std, 0.143 ).
solved_singlebasis_vampire('LCL047-1', basis_to_theorem_std, 0.027 ).
solved_singlebasis_vampire('LCL048-1', basis_to_theorem_std, 0.122 ).
solved_singlebasis_vampire('LCL049-1', basis_to_theorem_std, 7.208 ).
solved_singlebasis_vampire('LCL050-1', basis_to_theorem_std, 15.033 ).
solved_singlebasis_vampire('LCL051-1', basis_to_theorem_std, 8.936 ).
solved_singlebasis_vampire('LCL052-1', basis_to_theorem_std, 7.71 ).
solved_singlebasis_vampire('LCL053-1', basis_to_theorem_std, 8.678 ).
solved_singlebasis_vampire('LCL055-1', basis_to_theorem_std, 8.217 ).
solved_singlebasis_vampire('LCL056-1', basis_to_theorem_std, 7.8 ).
solved_singlebasis_vampire('LCL057-1', basis_to_theorem_std, 7.849 ).
solved_singlebasis_vampire('LCL058-1', basis_to_theorem_std, 16.192 ).
solved_singlebasis_vampire('LCL059-1', basis_to_theorem_std, 15.801 ).
solved_singlebasis_vampire('LCL060-1', basis_to_theorem_std, 16.971 ).
solved_singlebasis_vampire('LCL064-1', axioms_to_basis_with_kd, 0.16 ).
solved_singlebasis_vampire('LCL064-1', axioms_to_d, 0.283 ).
solved_singlebasis_vampire('LCL064-1', axioms_to_k, 0.025 ).
solved_singlebasis_vampire('LCL064-1', basis_to_axiom_1_std, 0.027 ).
solved_singlebasis_vampire('LCL064-1', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL064-1', basis_to_axiom_3_std, 0.038 ).
solved_singlebasis_vampire('LCL064-1', basis_to_theorem_std, 30.882 ).
solved_singlebasis_vampire('LCL064-2', axioms_to_basis_std, 37.964 ).
solved_singlebasis_vampire('LCL064-2', axioms_to_basis_with_kd, 0.034 ).
solved_singlebasis_vampire('LCL064-2', axioms_to_d, 1.925 ).
solved_singlebasis_vampire('LCL064-2', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL064-2', basis_to_axiom_1_std, 0.025 ).
solved_singlebasis_vampire('LCL064-2', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL064-2', basis_to_theorem_std, 6.636 ).
solved_singlebasis_vampire('LCL065-1', basis_to_theorem_std, 0.39 ).
solved_singlebasis_vampire('LCL066-1', basis_to_theorem_std, 0.085 ).
solved_singlebasis_vampire('LCL067-1', axioms_to_basis_with_kd, 0.195 ).
solved_singlebasis_vampire('LCL067-1', axioms_to_d, 12.921 ).
solved_singlebasis_vampire('LCL067-1', axioms_to_k, 0.025 ).
solved_singlebasis_vampire('LCL067-1', basis_to_axiom_1_std, 0.65 ).
solved_singlebasis_vampire('LCL067-1', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL067-1', basis_to_axiom_3_std, 0.029 ).
solved_singlebasis_vampire('LCL067-1', basis_to_theorem_std, 7.088 ).
solved_singlebasis_vampire('LCL068-1', basis_to_theorem_std, 8.259 ).
solved_singlebasis_vampire('LCL069-1', basis_to_theorem_std, 1.754 ).
solved_singlebasis_vampire('LCL070-1', axioms_to_basis_with_kd, 0.145 ).
solved_singlebasis_vampire('LCL070-1', axioms_to_d, 13.891 ).
solved_singlebasis_vampire('LCL070-1', axioms_to_k, 0.025 ).
solved_singlebasis_vampire('LCL070-1', basis_to_axiom_1_std, 0.651 ).
solved_singlebasis_vampire('LCL070-1', basis_to_axiom_2_std, 0.028 ).
solved_singlebasis_vampire('LCL070-1', basis_to_axiom_3_std, 0.031 ).
solved_singlebasis_vampire('LCL070-1', basis_to_theorem_std, 16.067 ).
solved_singlebasis_vampire('LCL071-1', basis_to_theorem_std, 13.875 ).
solved_singlebasis_vampire('LCL072-1', basis_to_theorem_std, 11.43 ).
solved_singlebasis_vampire('LCL076-1', basis_to_theorem_std, 1.733 ).
solved_singlebasis_vampire('LCL076-2', axioms_to_basis_with_kd, 32.381 ).
solved_singlebasis_vampire('LCL076-2', axioms_to_d, 1.927 ).
solved_singlebasis_vampire('LCL076-2', axioms_to_k, 0.025 ).
solved_singlebasis_vampire('LCL076-2', basis_to_axiom_1_std, 0.027 ).
solved_singlebasis_vampire('LCL076-2', basis_to_axiom_2_std, 0.072 ).
solved_singlebasis_vampire('LCL076-2', basis_to_axiom_3_std, 0.027 ).
solved_singlebasis_vampire('LCL076-2', basis_to_axiom_4_std, 0.031 ).
solved_singlebasis_vampire('LCL076-2', basis_to_theorem_std, 0.029 ).
solved_singlebasis_vampire('LCL077-1', basis_to_theorem_std, 0.374 ).
solved_singlebasis_vampire('LCL079-1', axioms_to_basis_with_kd, 40.215 ).
solved_singlebasis_vampire('LCL079-1', axioms_to_d, 1.928 ).
solved_singlebasis_vampire('LCL079-1', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL079-1', basis_to_axiom_1_std, 0.026 ).
solved_singlebasis_vampire('LCL079-1', basis_to_axiom_2_std, 0.334 ).
solved_singlebasis_vampire('LCL079-1', basis_to_axiom_3_std, 0.089 ).
solved_singlebasis_vampire('LCL079-1', basis_to_axiom_4_std, 0.026 ).
solved_singlebasis_vampire('LCL079-1', basis_to_axiom_5_std, 0.028 ).
solved_singlebasis_vampire('LCL079-1', basis_to_theorem_std, 0.336 ).
solved_singlebasis_vampire('LCL080-1', axioms_to_basis_std, 37.716 ).
solved_singlebasis_vampire('LCL080-1', axioms_to_basis_with_kd, 0.235 ).
solved_singlebasis_vampire('LCL080-1', axioms_to_d, 1.844 ).
solved_singlebasis_vampire('LCL080-1', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL080-1', basis_to_axiom_1_std, 0.026 ).
solved_singlebasis_vampire('LCL080-1', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL080-1', basis_to_axiom_3_std, 0.03 ).
solved_singlebasis_vampire('LCL080-1', basis_to_theorem_std, 7.619 ).
solved_singlebasis_vampire('LCL080-2', axioms_to_basis_std, 42.12 ).
solved_singlebasis_vampire('LCL080-2', axioms_to_basis_with_kd, 0.309 ).
solved_singlebasis_vampire('LCL080-2', axioms_to_d, 1.83 ).
solved_singlebasis_vampire('LCL080-2', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL080-2', basis_to_axiom_1_std, 0.034 ).
solved_singlebasis_vampire('LCL080-2', basis_to_axiom_2_std, 0.026 ).
solved_singlebasis_vampire('LCL080-2', basis_to_axiom_3_std, 0.028 ).
solved_singlebasis_vampire('LCL080-2', basis_to_axiom_4_std, 0.031 ).
solved_singlebasis_vampire('LCL080-2', basis_to_theorem_std, 7.903 ).
solved_singlebasis_vampire('LCL083-2', axioms_to_basis_std, 1540.73 ).
solved_singlebasis_vampire('LCL083-2', axioms_to_basis_with_kd, 0.034 ).
solved_singlebasis_vampire('LCL083-2', axioms_to_d, 742.637 ).
solved_singlebasis_vampire('LCL083-2', axioms_to_k, 0.029 ).
solved_singlebasis_vampire('LCL083-2', basis_to_axiom_1_std, 0.03 ).
solved_singlebasis_vampire('LCL083-2', basis_to_axiom_2_std, 0.031 ).
solved_singlebasis_vampire('LCL083-2', basis_to_theorem_std, 0.176 ).
solved_singlebasis_vampire('LCL084-2', basis_to_theorem_std, 700.466 ).
solved_singlebasis_vampire('LCL084-3', axioms_to_basis_std, 40.023 ).
solved_singlebasis_vampire('LCL084-3', axioms_to_basis_with_kd, 0.196 ).
solved_singlebasis_vampire('LCL084-3', axioms_to_d, 734.593 ).
solved_singlebasis_vampire('LCL084-3', axioms_to_k, 0.028 ).
solved_singlebasis_vampire('LCL084-3', basis_to_axiom_1_std, 0.067 ).
solved_singlebasis_vampire('LCL084-3', basis_to_axiom_2_std, 0.027 ).
solved_singlebasis_vampire('LCL084-3', basis_to_axiom_3_std, 0.03 ).
solved_singlebasis_vampire('LCL084-3', basis_to_theorem_std, 706.102 ).
solved_singlebasis_vampire('LCL110-1', axioms_to_basis_std, 1945.07 ).
solved_singlebasis_vampire('LCL110-1', axioms_to_basis_with_kd, 32.857 ).
solved_singlebasis_vampire('LCL110-1', axioms_to_d, 1.76 ).
solved_singlebasis_vampire('LCL110-1', axioms_to_k, 0.026 ).
solved_singlebasis_vampire('LCL110-1', basis_to_axiom_1_std, 0.027 ).
solved_singlebasis_vampire('LCL110-1', basis_to_axiom_2_std, 0.089 ).
solved_singlebasis_vampire('LCL110-1', basis_to_axiom_3_std, 0.027 ).
solved_singlebasis_vampire('LCL110-1', basis_to_axiom_4_std, 0.032 ).
solved_singlebasis_vampire('LCL110-1', basis_to_theorem_std, 0.384 ).
solved_singlebasis_vampire('LCL111-1', basis_to_theorem_std, 6.548 ).
solved_singlebasis_vampire('LCL112-1', basis_to_theorem_std, 0.193 ).
solved_singlebasis_vampire('LCL113-1', basis_to_theorem_std, 69.668 ).
solved_singlebasis_vampire('LCL114-1', basis_to_theorem_std, 105.458 ).
solved_singlebasis_vampire('LCL115-1', basis_to_theorem_std, 0.507 ).
solved_singlebasis_vampire('LCL116-1', basis_to_theorem_std, 227.781 ).
solved_singlebasis_vampire('LCL256-1', basis_to_theorem_std, 7.926 ).
solved_singlebasis_vampire('LCL355-1', basis_to_theorem_std, 7.22 ).
solved_singlebasis_vampire('LCL356-1', basis_to_theorem_std, 35.35 ).
solved_singlebasis_vampire('LCL357-1', basis_to_theorem_std, 32.164 ).
solved_singlebasis_vampire('LCL358-1', basis_to_theorem_std, 38.254 ).
solved_singlebasis_vampire('LCL359-1', basis_to_theorem_std, 1101.54 ).
solved_singlebasis_vampire('LCL360-1', basis_to_theorem_std, 0.12 ).
solved_singlebasis_vampire('LCL361-1', basis_to_theorem_std, 0.139 ).
solved_singlebasis_vampire('LCL362-1', basis_to_theorem_std, 0.027 ).
solved_singlebasis_vampire('LCL363-1', basis_to_theorem_std, 0.027 ).
solved_singlebasis_vampire('LCL364-1', basis_to_theorem_std, 36.219 ).
solved_singlebasis_vampire('LCL365-1', basis_to_theorem_std, 34.736 ).
solved_singlebasis_vampire('LCL366-1', basis_to_theorem_std, 2.109 ).
solved_singlebasis_vampire('LCL367-1', basis_to_theorem_std, 0.027 ).
solved_singlebasis_vampire('LCL368-1', basis_to_theorem_std, 35.633 ).
solved_singlebasis_vampire('LCL369-1', basis_to_theorem_std, 1747.07 ).
solved_singlebasis_vampire('LCL370-1', basis_to_theorem_std, 16.027 ).
solved_singlebasis_vampire('LCL371-1', basis_to_theorem_std, 15.985 ).
solved_singlebasis_vampire('LCL372-1', basis_to_theorem_std, 420.335 ).
solved_singlebasis_vampire('LCL373-1', basis_to_theorem_std, 17.064 ).
solved_singlebasis_vampire('LCL378-1', basis_to_theorem_std, 7.877 ).
solved_singlebasis_vampire('LCL379-1', basis_to_theorem_std, 8.029 ).
solved_singlebasis_vampire('LCL380-1', basis_to_theorem_std, 7.965 ).
solved_singlebasis_vampire('LCL381-1', basis_to_theorem_std, 34.754 ).
solved_singlebasis_vampire('LCL382-1', basis_to_theorem_std, 16.848 ).
solved_singlebasis_vampire('LCL383-1', basis_to_theorem_std, 735.892 ).
solved_singlebasis_vampire('LCL384-1', basis_to_theorem_std, 38.642 ).
solved_singlebasis_vampire('LCL385-1', basis_to_theorem_std, 17.141 ).
solved_singlebasis_vampire('LCL386-1', basis_to_theorem_std, 16.293 ).
solved_singlebasis_vampire('LCL387-1', basis_to_theorem_std, 239.96 ).
solved_singlebasis_vampire('LCL388-1', basis_to_theorem_std, 732.31 ).
solved_singlebasis_vampire('LCL389-1', basis_to_theorem_std, 734.079 ).
solved_singlebasis_vampire('LCL390-1', basis_to_theorem_std, 17.572 ).
solved_singlebasis_vampire('LCL392-1', basis_to_theorem_std, 234.288 ).
solved_singlebasis_vampire('LCL396-1', basis_to_theorem_std, 8.111 ).
solved_singlebasis_vampire('LCL397-1', basis_to_theorem_std, 0.148 ).
solved_singlebasis_vampire('LCL398-1', basis_to_theorem_std, 0.135 ).
solved_singlebasis_vampire('LCL399-1', basis_to_theorem_std, 9.808 ).
solved_singlebasis_vampire('LCL400-1', basis_to_theorem_std, 8.386 ).
solved_singlebasis_vampire('LCL401-1', basis_to_theorem_std, 8.354 ).
solved_singlebasis_vampire('LCL402-1', basis_to_theorem_std, 8.395 ).
solved_singlebasis_vampire('LCL403-1', basis_to_theorem_std, 16.746 ).
solved_singlebasis_vampire('LCL404-1', basis_to_theorem_std, 16.917 ).
solved_singlebasis_vampire('LCL405-1', basis_to_theorem_std, 7.034 ).
solved_singlebasis_vampire('LCL425-1', axioms_to_d, 0.028 ).
solved_singlebasis_vampire('LCL426-1', axioms_to_d, 0.028 ).
solved_singlebasis_vampire('LCL428-1', axioms_to_basis_std, 1967.73 ).
solved_singlebasis_vampire('LCL428-1', axioms_to_basis_with_kd, 32.861 ).
solved_singlebasis_vampire('LCL428-1', axioms_to_d, 0.152 ).
solved_singlebasis_vampire('LCL428-1', axioms_to_k, 0.027 ).
solved_singlebasis_vampire('LCL428-1', basis_to_axiom_1_std, 0.028 ).
solved_singlebasis_vampire('LCL428-1', basis_to_axiom_2_std, 0.104 ).
solved_singlebasis_vampire('LCL428-1', basis_to_axiom_3_std, 0.029 ).
solved_singlebasis_vampire('LCL428-1', basis_to_axiom_4_std, 0.031 ).
solved_singlebasis_vampire('LCL428-1', basis_to_theorem_std, 1875.43 ).
solved_singlebasis_vampire('LCL875-1', axioms_to_d, 0.029 ).
solved_singlebasis_vampire('LCL876+1', axioms_to_basis_with_kd, 32.863 ).
solved_singlebasis_vampire('LCL876+1', axioms_to_d, 1.763 ).
solved_singlebasis_vampire('LCL876+1', axioms_to_k, 0.027 ).
solved_singlebasis_vampire('LCL876+1', basis_to_axiom_1_std, 0.097 ).
solved_singlebasis_vampire('LCL876+1', basis_to_axiom_2_std, 0.042 ).
solved_singlebasis_vampire('LCL876+1', basis_to_axiom_3_std, 0.028 ).
solved_singlebasis_vampire('LCL876+1', basis_to_axiom_4_std, 0.027 ).

solved_singlebasis_vampire('LCL006-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL025-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL029-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL040-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL041-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL046-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL054-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL061-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL062-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL063-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL064-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL067-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL070-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL076-2', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL079-1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL096-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL096-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL097-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL097-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL100-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL100-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL101-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL101-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL102-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL102-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL103-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL103-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL104-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL104-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL105-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL105-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL106-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL106-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL109-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL125-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL125-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL126-1', axioms_to_d, timeout).
solved_singlebasis_vampire('LCL126-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL374-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL375-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL376-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL377-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL391-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL393-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL394-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL395-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL419-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL420-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL421-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL422-1', basis_to_theorem_std, timeout).
solved_singlebasis_vampire('LCL425-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL426-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL875-1', axioms_to_k, timeout).
solved_singlebasis_vampire('LCL876+1', axioms_to_basis_std, timeout).
solved_singlebasis_vampire('LCL876+1', basis_to_theorem_std, timeout).

solved_singlebasis_vampire('LCL063-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL109-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL377-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL395-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL419-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL420-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL421-1', original_problem, timeout ).
solved_singlebasis_vampire('LCL876+1', original_problem, timeout ).
solved_singlebasis_vampire('LCL047-1', original_problem, 13.929 ).
solved_singlebasis_vampire('LCL048-1', original_problem, 14.181 ).
solved_singlebasis_vampire('LCL025-1', original_problem, 1.493 ).
solved_singlebasis_vampire('LCL026-1', original_problem, 7.513 ).
solved_singlebasis_vampire('LCL027-1', original_problem, 0.057 ).
solved_singlebasis_vampire('LCL028-1', original_problem, 489.267 ).
solved_singlebasis_vampire('LCL029-1', original_problem, 5.676 ).
solved_singlebasis_vampire('LCL030-1', original_problem, 61.674 ).
solved_singlebasis_vampire('LCL031-1', original_problem, 21.317 ).
solved_singlebasis_vampire('LCL040-1', original_problem, 17.453 ).
solved_singlebasis_vampire('LCL041-1', original_problem, 0.068 ).
solved_singlebasis_vampire('LCL042-1', original_problem, 0.357 ).
solved_singlebasis_vampire('LCL043-1', original_problem, 0.067 ).
solved_singlebasis_vampire('LCL044-1', original_problem, 0.077 ).
solved_singlebasis_vampire('LCL045-1', original_problem, 0.27 ).
solved_singlebasis_vampire('LCL046-1', original_problem, 0.056 ).
solved_singlebasis_vampire('LCL049-1', original_problem, 19.692 ).
solved_singlebasis_vampire('LCL050-1', original_problem, 16.543 ).
solved_singlebasis_vampire('LCL051-1', original_problem, 21.169 ).
solved_singlebasis_vampire('LCL052-1', original_problem, 10.669 ).
solved_singlebasis_vampire('LCL053-1', original_problem, 11.275 ).
solved_singlebasis_vampire('LCL054-1', original_problem, 21.506 ).
solved_singlebasis_vampire('LCL055-1', original_problem, 11.25 ).
solved_singlebasis_vampire('LCL056-1', original_problem, 10.528 ).
solved_singlebasis_vampire('LCL057-1', original_problem, 10.681 ).
solved_singlebasis_vampire('LCL058-1', original_problem, 16.041 ).
solved_singlebasis_vampire('LCL059-1', original_problem, 16.203 ).
solved_singlebasis_vampire('LCL060-1', original_problem, 19.933 ).
solved_singlebasis_vampire('LCL061-1', original_problem, 20.699 ).
solved_singlebasis_vampire('LCL062-1', original_problem, 21.748 ).
solved_singlebasis_vampire('LCL064-1', original_problem, 0.172 ).
solved_singlebasis_vampire('LCL064-2', original_problem, 0.149 ).
solved_singlebasis_vampire('LCL065-1', original_problem, 0.824 ).
solved_singlebasis_vampire('LCL066-1', original_problem, 0.084 ).
solved_singlebasis_vampire('LCL067-1', original_problem, 0.11 ).
solved_singlebasis_vampire('LCL068-1', original_problem, 6.377 ).
solved_singlebasis_vampire('LCL069-1', original_problem, 0.081 ).
solved_singlebasis_vampire('LCL070-1', original_problem, 20.331 ).
solved_singlebasis_vampire('LCL071-1', original_problem, 1.808 ).
solved_singlebasis_vampire('LCL072-1', original_problem, 0.074 ).
solved_singlebasis_vampire('LCL076-1', original_problem, 0.359 ).
solved_singlebasis_vampire('LCL076-2', original_problem, 0.066 ).
solved_singlebasis_vampire('LCL077-1', original_problem, 0.121 ).
solved_singlebasis_vampire('LCL079-1', original_problem, 0.087 ).
solved_singlebasis_vampire('LCL080-1', original_problem, 25.931 ).
solved_singlebasis_vampire('LCL080-2', original_problem, 27.011 ).
solved_singlebasis_vampire('LCL083-2', original_problem, 0.073 ).
solved_singlebasis_vampire('LCL084-2', original_problem, 204.315 ).
solved_singlebasis_vampire('LCL084-3', original_problem, 203.638 ).
solved_singlebasis_vampire('LCL110-1', original_problem, 0.074 ).
solved_singlebasis_vampire('LCL111-1', original_problem, 0.063 ).
solved_singlebasis_vampire('LCL112-1', original_problem, 0.107 ).
solved_singlebasis_vampire('LCL113-1', original_problem, 1.246 ).
solved_singlebasis_vampire('LCL114-1', original_problem, 20.495 ).
solved_singlebasis_vampire('LCL115-1', original_problem, 5.565 ).
solved_singlebasis_vampire('LCL116-1', original_problem, 20.433 ).
solved_singlebasis_vampire('LCL256-1', original_problem, 10.799 ).
solved_singlebasis_vampire('LCL355-1', original_problem, 0.267 ).
solved_singlebasis_vampire('LCL356-1', original_problem, 0.095 ).
solved_singlebasis_vampire('LCL357-1', original_problem, 0.03 ).
solved_singlebasis_vampire('LCL358-1', original_problem, 0.139 ).
solved_singlebasis_vampire('LCL359-1', original_problem, 0.285 ).
solved_singlebasis_vampire('LCL360-1', original_problem, 0.086 ).
solved_singlebasis_vampire('LCL361-1', original_problem, 0.116 ).
solved_singlebasis_vampire('LCL362-1', original_problem, 0.076 ).
solved_singlebasis_vampire('LCL363-1', original_problem, 0.109 ).
solved_singlebasis_vampire('LCL364-1', original_problem, 2.1 ).
solved_singlebasis_vampire('LCL365-1', original_problem, 1.478 ).
solved_singlebasis_vampire('LCL366-1', original_problem, 19.905 ).
solved_singlebasis_vampire('LCL367-1', original_problem, 10.394 ).
solved_singlebasis_vampire('LCL368-1', original_problem, 17.112 ).
solved_singlebasis_vampire('LCL369-1', original_problem, 23.577 ).
solved_singlebasis_vampire('LCL370-1', original_problem, 20.258 ).
solved_singlebasis_vampire('LCL371-1', original_problem, 19.981 ).
solved_singlebasis_vampire('LCL372-1', original_problem, 20.91 ).
solved_singlebasis_vampire('LCL373-1', original_problem, 20.107 ).
solved_singlebasis_vampire('LCL374-1', original_problem, 1785.52 ).
solved_singlebasis_vampire('LCL375-1', original_problem, 24.06 ).
solved_singlebasis_vampire('LCL376-1', original_problem, 86.977 ).
solved_singlebasis_vampire('LCL378-1', original_problem, 10.752 ).
solved_singlebasis_vampire('LCL379-1', original_problem, 10.667 ).
solved_singlebasis_vampire('LCL380-1', original_problem, 10.911 ).
solved_singlebasis_vampire('LCL381-1', original_problem, 11.446 ).
solved_singlebasis_vampire('LCL382-1', original_problem, 19.833 ).
solved_singlebasis_vampire('LCL383-1', original_problem, 21.076 ).
solved_singlebasis_vampire('LCL384-1', original_problem, 20.919 ).
solved_singlebasis_vampire('LCL385-1', original_problem, 16.306 ).
solved_singlebasis_vampire('LCL386-1', original_problem, 16.019 ).
solved_singlebasis_vampire('LCL387-1', original_problem, 16.188 ).
solved_singlebasis_vampire('LCL388-1', original_problem, 16.444 ).
solved_singlebasis_vampire('LCL389-1', original_problem, 17.001 ).
solved_singlebasis_vampire('LCL390-1', original_problem, 19.878 ).
solved_singlebasis_vampire('LCL391-1', original_problem, 25.681 ).
solved_singlebasis_vampire('LCL392-1', original_problem, 20.307 ).
solved_singlebasis_vampire('LCL393-1', original_problem, 23.194 ).
solved_singlebasis_vampire('LCL394-1', original_problem, 24.026 ).
solved_singlebasis_vampire('LCL396-1', original_problem, 11.016 ).
solved_singlebasis_vampire('LCL397-1', original_problem, 0.074 ).
solved_singlebasis_vampire('LCL398-1', original_problem, 0.066 ).
solved_singlebasis_vampire('LCL399-1', original_problem, 1.713 ).
solved_singlebasis_vampire('LCL400-1', original_problem, 12.583 ).
solved_singlebasis_vampire('LCL401-1', original_problem, 13.132 ).
solved_singlebasis_vampire('LCL402-1', original_problem, 12.61 ).
solved_singlebasis_vampire('LCL403-1', original_problem, 47.65 ).
solved_singlebasis_vampire('LCL404-1', original_problem, 20.372 ).
solved_singlebasis_vampire('LCL405-1', original_problem, 10.991 ).
solved_singlebasis_vampire('LCL422-1', original_problem, 216.055 ).
solved_singlebasis_vampire('LCL428-1', original_problem, 155.622 ).
