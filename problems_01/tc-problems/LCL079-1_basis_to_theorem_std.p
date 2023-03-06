%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(f(f(G,f(H,G)),f(f(f(I,f(J,K)),f(f(I,J),f(I,K))),L)),L),f(f(f(M,N),f(n(N),n(M))),O)),O),f(f(a,b),P)),P),f(f(b,c),Q)),Q),R)),R)) )).

fof(f3,conjecture,(
    thm(f(a,c)) )).
%------------------------------------------------------------------------------
