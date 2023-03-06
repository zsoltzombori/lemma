%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(G,H),I),f(H,I)),f(f(f(f(J,K),L),f(n(J),L)),M)),M),f(f(f(N,f(n(O),P)),f(N,f(f(Q,P),f(f(O,Q),P)))),R)),R),S)),S)) )).

fof(f3,conjecture,(
    thm(f(f(n(a),a),a)) )).
%------------------------------------------------------------------------------
