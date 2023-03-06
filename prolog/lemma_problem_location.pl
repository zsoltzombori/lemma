:- module(lemma_problem_location,
	  [problem_absname/2,
	   problem_atomic_name/2,
	   optionsfile_absname/2]).

:- use_module(swilib(err)).

problem_atomic_name(X, Y) :-
	atomic(X),
	!,
	Y = X.
problem_atomic_name(X, Y) :-
	variant_sha1(X, Y1),
	atom_concat('problem_', Y1, Y).

problem_absname(ProblemSpec, AbsName) :-
	( atom(ProblemSpec),
	  ( sub_atom(ProblemSpec, _, _, _, '/')
	  ; sub_atom(ProblemSpec, _, _, 0, '.p')
	  ) ->
	  ( getenv('LEMGEN_PROBLEM_PATH', Path) ->
	    true
	  ; err('Environment variable LEMGEN_PROBLEM_PATH to directories with problem files not set')
	  ),
	  atomic_list_concat(Dirs, ':', Path),
	  ( member(Dir, Dirs),
	    Dir \= '',
	    format(atom(AbsName), '~w/~w', [Dir, ProblemSpec]),
	    exists_file(AbsName) ->
	    true
	  ; err('TPTP file ~w not found', [ProblemSpec])
	  )
	; AbsName = ProblemSpec
	).

optionsfile_absname(FileSpec, AbsName) :-
	( exists_file(FileSpec) ->
	  AbsName = FileSpec
	; getenv('LEMGEN_OPTIONS_PATH', Path) ->
	  atomic_list_concat(Dirs, ':', Path),
	  ( member(Dir, Dirs),
	    Dir \= '',
	    format(atom(AbsName), '~w/~w', [Dir, FileSpec]),
	    exists_file(AbsName) ->
	    true
	  ; err('Options file ~w not found in $LEMGEN_OPTIONS_PATH', [FileSpec])
	  )
	; err('Options file ~w not found', [FileSpec])
	).
