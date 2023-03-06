:- module(spo_lemmas,
	  [ spo_lemmas/3,
	    pl_load_spo_lemmas/1,
	    pl_sort_spo_lemmas/2 ]).

:- use_module(swilib(info)).
:- use_module(swilib(load_gzipped_file)).
:- use_module(cdtools(dc_representation_cd)).

:- multifile spo/3.
:- dynamic spo/3.
:- dynamic seen/1.

spo_lemmas(Files, Sort, Head) :-
	pl_load_spo_lemmas(Files),
	pl_sort_spo_lemmas(Sort, Head).

pl_load_spo_lemmas(Files) :-	
	retractall( spo(_, _, _) ),
	style_check(-singleton),
	( member(File, Files),
	  info(10, 'Reading ~w', [File]),
	  ( sub_atom(File, _, _, 0, '.gz') ->
	    load_gzipped_file(File)
	  ; consult(File)
	  ),
	  fail
	; true
	),
	style_check(+singleton).

pl_sort_spo_lemmas(Sort, Head) :-
	retractall( seen(_) ),
	info(10, 'Processing', []),
	check_features(Sort, Sort1),
	findall(X, spo(X, type, lemma), Xs),
	sort_lemmas(Sort1, Xs, Xs1),
	local_list_prefix(Head, Xs1, Xs2),
	info(10, 'Writing output', []),
	( member(X, Xs2),
	  ( spo(X, dcterm, DC) ->
	    true
	  ; spo(X, dterm, D) ->
	    d_to_dc(D, DC)
	  ),
	  variant_sha1(DC, HDC),
	  ( seen(HDC) ->
	    true
	  ; assertz( seen(HDC) ),
	    numbervars(DC, 0, _),
	    writeq(DC),
	    writeln('.')
	  ),
	  fail
	; true
	).

sort_lemmas(nil, Xs, Xs) :-
	!.
sort_lemmas(random, Xs, Ys) :-
	!,
	findall(K-X, (member(X, Xs), K is random(10000000)), Xs1),
	keysort(Xs1, Xs2),
	local_map_val(Xs2, Ys).
sort_lemmas(random(Seed), Xs, Ys) :-
	!,
	set_random(seed(Seed)),
	findall(K-X, (member(X, Xs), K is random(10000000)), Xs1),
	keysort(Xs1, Xs2),
	local_map_val(Xs2, Ys).
sort_lemmas([], Xs, Xs) :-
	!.
sort_lemmas(F, Xs, Ys) :-
	findall(K-X, (member(X, Xs), mk_key(F, X, K)), Xs1),
	keysort(Xs1, Xs2),
	local_map_val(Xs2, Ys).


check_features(nil, nil) :-
	!.
check_features(random, random) :-
	!.
check_features(random(X), random(X)) :-
	!.
check_features(Fs, Fs1) :-
	findall(F, ( member(F, Fs),
		     ( F = -(F1) ->
		       ( spo(_, F1, V), \+ number(V) ->
			 info(0, 'Warning: ignoring feature ~q which has a non-numerical value', [F]),
			 fail
		       ; true
		       )
		     ; F1 = F
		     ),
		     ( memberchk(F1, [random, random(_), dcterm_hash]) ->
		       true
		     ; spo(_, F1, _) ->
		       true
		     ; info(0, 'Warning: ignoring feature ~q which has no value', [F]),
		       fail
		     )
		   ),
		Fs1).

local_list_prefix(nil, X, X) :-
	!.
local_list_prefix(N, X, Y) :-
	N < 0,
	!,
	reverse(X, X1),
	N1 is N * -1,
	local_list_prefix_1(N1, X1, X2),
	reverse(X2, Y).
local_list_prefix(N, X, Y) :-
	local_list_prefix_1(N, X, Y).

local_list_prefix_1(0, _, []) :-
	!.
local_list_prefix_1(N, [X|Xs], [X|Ys]) :-
	N1 is N-1,
	local_list_prefix_1(N1, Xs, Ys).

local_map_val([_-X|Xs], [X|Xs1]) :-
	local_map_val(Xs, Xs1).
local_map_val([], []).

mk_key(Fs, X, K) :-
	map_feature_value(Fs, X, Vs),
	K =.. [k|Vs].

map_feature_value([X|Xs], Y1, [X1|Xs1]) :-
	feature_value(X, Y1, X1),
	map_feature_value(Xs, Y1, Xs1).
map_feature_value([], _, []).

feature_value(random, _, V) :-
	!,
	V is random(10000000).
feature_value(dcterm_hash, X, V) :-
	!,
	spo(X, dcterm, DC),
	variant_sha1(DC, V).
feature_value(-(F), X, V) :-
	!,
	feature_value(F, X, V1),
	V is V1 * -1.
feature_value(F, X, V) :-
	( spo(X, F, V) ->
	  true
	; V = 0
	).