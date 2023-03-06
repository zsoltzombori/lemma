%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(f(G,H),f(f(H,I),f(G,I))),f(f(J,f(K,J)),L)),L),f(f(f(f(M,N),M),M),O)),O),f(f(falsehood,P),Q)),Q),R)),R)) )).

fof(f3,conjecture,(
    thm(f(f(f(a,falsehood),falsehood),a)) )).
%------------------------------------------------------------------------------
