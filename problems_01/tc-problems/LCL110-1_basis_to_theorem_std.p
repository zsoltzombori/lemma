%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(G,f(H,G)),f(f(f(I,J),f(f(J,K),f(I,K))),L)),L),f(f(f(f(M,N),N),f(f(N,M),M)),O)),O),f(f(f(n(P),n(Q)),f(Q,P)),R)),R),S)),S)) )).

fof(f3,conjecture,(
    thm(f(n(n(a)),a)) )).
%------------------------------------------------------------------------------
