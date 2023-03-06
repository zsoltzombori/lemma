:- module(lemma_extraction,
	  [gen_lemma/4]).

:- use_module(lemma_features, [number_of_incoming_edges/3]).
:- use_module(swilib(err)).
:- use_module(cdtools(dterm_cd)).
:- use_module(cdtools(compress_cd)).
:- use_module(cdtools(sk_cd)).
:- use_module(cdtools(callutils_cd)).
:- use_module(cdtools(miscutils_cd), [sub_term_u/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Methods to Extract Lemmas From a Given Proof
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_lemma(subtree, D, D1, Opts) :-
	!,
	gen_lemma_subtree(D, D1, Opts).
gen_lemma(slemma_nonpure(Limit, K), D, D1, _Opts) :-
	!,
	( number(Limit), Limit < 0 ->
	  Pred = gen_slemma_nonpure_h_2,
	  Limit1 = Limit
	; Limit = h(Limit1) ->
	  Pred = gen_slemma_nonpure_h_2
	; Limit1 = Limit,
	  Pred = gen_slemma_nonpure_t_2
	),
	gen_lemma_slemma(Pred, D, D1, Limit1, K).
gen_lemma(slemma_pure(Limit, K), D, D1, _Opts) :-
	!,
	( number(Limit), Limit < 0 ->
	  Pred = gen_slemma_pure_h_2,
	  Limit1 = Limit
	; Limit = h(Limit1) ->
	  Pred = gen_slemma_pure_h_2
	; Limit1 = Limit,
	  Pred = gen_slemma_pure_t_2
	),
	gen_lemma_slemma(Pred, D, D1, Limit1, K).
gen_lemma(treerepair, D, D1, Opts) :-
	!,
	gen_lemma_treerepair(D, D1, Opts).
gen_lemma(M, _, _, _) :-
	err('Unknown lemma generation method: ~q', [M]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_lemma_subtree(D, D1, _) :-
	sub_term_u(D1, D),
	D1 \== D,
	\+ atomic(D1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


gen_lemma_slemma(GenSlemmaPred, D, D1, Limit, K) :-
	K > 0,
	Counter = counter(0),
	gen_lemma_slemma_1(GenSlemmaPred, D, D1, Limit, N),
 	( N = 1 -> !, fail ; true ),
	\+ atomic(D1),
	\+ var(D1),
	\+ variant(D1, d(_,_)),
	\+ D1 == D,
	arg(1, Counter, N0),
	N1 is N0 + 1,
	( N1 < K ->
	  nb_setarg(1, Counter, N1)
	; !
	).

gen_lemma_slemma_1(GenSlemmaPred, D, D1, Limit, N) :-
	d_tsize(D, TSize),
	nsols((sub_term_u(X, D), atomic(X)), NAxioms),
	info(10, 'Generating slemmas with ~w for tsize: ~w axioms: ~w.',
	     [GenSlemmaPred, TSize, NAxioms]),
	findall(D2,
		(sub_term_u(D3, D),
		 call(GenSlemmaPred, D3, Limit, D2)),
		Ds),
	sort(Ds, Ds1),
	length(Ds1, L),
	info(10, 'Found ~D slemmas.', [L]),
	findall(D2, (member(D3, Ds1), d_0_to_var(D3, D2)), Ds2),
	info(10, 'Determining number of matches of slemmas.'),
	findall(N1-D2, (member(D2, Ds2), occurrences_of_term(D2, D, N1)), Ds3),
	reverse(Ds3, Ds4),
	keysort(Ds4, Ds5),
	reverse(Ds5, Ds6),
	member(N-D1, Ds6).

gen_slemma_nonpure_h_2(N, _, N) :-
	atomic(N),
	!.
gen_slemma_nonpure_h_2(d(_,_), _, 0).
gen_slemma_nonpure_h_2(d(D1,D2), L, d(D3,D4)) :-
	L \= 0,
	L1 is L-1,
	gen_slemma_nonpure_h_2(D1, L1, D3),
	gen_slemma_nonpure_h_2(D2, L1, D4).

gen_slemma_pure_h_2(N, _, 0) :-
	atomic(N),
	!.
gen_slemma_pure_h_2(d(_,_), _, 0).
gen_slemma_pure_h_2(d(D1,D2), L, d(D3,D4)) :-
	L \= 0,
	L1 is L-1,
	gen_slemma_pure_h_2(D1, L1, D3),
	gen_slemma_pure_h_2(D2, L1, D4).

gen_slemma_nonpure_t_2(X, S, Y) :-
	gen_slemma_nonpure_t_2(X, S, _, Y).

gen_slemma_pure_t_2(X, S, Y) :-
	gen_slemma_pure_t_2(X, S, _, Y).

gen_slemma_nonpure_t_2(N, S, S, N) :-
	atomic(N),
	!.
gen_slemma_nonpure_t_2(d(_,_), S, S, 0).
gen_slemma_nonpure_t_2(d(D1,D2), S, S1, d(D3,D4)) :-
	S > 0,
	S2 is S-1,
	gen_slemma_nonpure_t_2(D1, S2, S3, D3),
	gen_slemma_nonpure_t_2(D2, S3, S1, D4).

gen_slemma_pure_t_2(N, S, S, 0) :-
	atomic(N),
	!.
gen_slemma_pure_t_2(d(_,_), S, S, 0).
gen_slemma_pure_t_2(d(D1,D2), S, S1, d(D3,D4)) :-
	S > 0,
	S2 is S-1,
	gen_slemma_pure_t_2(D1, S2, S3, D3),
	gen_slemma_pure_t_2(D2, S3, S1, D4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_lemma_treerepair(D, D1, Opts) :-
	d_to_gr(D, G),
	% pp(G), nl,
	gr_to_sk(G, SK, [lambda_optim=1,simp=red]),
	term_factorized(SK, _, SKF),
	map_bind(SKF),
	member(_=SK1, SKF),
	( memberchk(lfp_d_incoming=Incoming, Opts) ->
	  number_of_incoming_edges(SK, SK1, Incoming)
	; true
	),
	sk_red_extend(SK1, D1, _Args).

map_bind([X=X|Y]) :- map_bind(Y).
map_bind([]).

sk_red_extend(D1, D2, []) :-
	sk_red_li(D1, D2), combinator_free(D2), !.
sk_red_extend(D1, D2, [X]) :-
	sk_red_li(d(D1,X), D2), combinator_free(D2), !.
sk_red_extend(D1, D2, [X,Y]) :-
	sk_red_li(d(d(D1,X),Y), D2), combinator_free(D2), !.
sk_red_extend(D1, D2, [X,Y,Z]) :-
	sk_red_li(d(d(d(D1,X),Y),Z), D2), combinator_free(D2), !.
sk_red_extend(D1, D2, [X,Y,Z,U]) :-
	sk_red_li(d(d(d(d(D1,X),Y),Z),U), D2), combinator_free(D2), !.
sk_red_extend(D1, D2, [X,Y,Z,U,V]) :-
	sk_red_li(d(d(d(d(d(D1,X),Y),Z),U),V), D2), combinator_free(D2), !.
sk_red_extend(D, _, _) :-
	err('Can not handle sk term: ~q', [D]).

combinator_free(X) :-
	\+ ( sub_term(Y, X),
	     atomic(Y),
	     \+ number(Y) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d_0_to_var(X, X) :-
	var(X),
	!.
d_0_to_var(0, _) :-
	!.
d_0_to_var(N, N) :-
	atomic(N),
	!.
d_0_to_var(d(D1,D2), d(D3,D4)) :-
	d_0_to_var(D1, D3),
	d_0_to_var(D2, D4).
