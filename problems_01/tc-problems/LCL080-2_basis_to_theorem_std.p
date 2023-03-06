%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(G,G),f(f(H,f(I,H)),J)),J),f(f(f(f(K,L),K),K),M)),M),f(f(f(N,O),f(f(O,P),f(N,P))),Q)),Q),R)),R)) )).

fof(f3,conjecture,(
    thm(f(f(f(a,b),c),f(f(c,a),f(e,a)))) )).
%------------------------------------------------------------------------------
