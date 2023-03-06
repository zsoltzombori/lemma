%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(G,H),I),f(H,I)),f(f(f(f(J,K),L),f(n(J),L)),M)),M),f(f(f(n(N),O),f(f(P,O),f(f(N,P),O))),Q)),Q),R)),R)) )).

fof(f3,conjecture,(
    thm(f(f(n(a),a),a)) )).
%------------------------------------------------------------------------------
