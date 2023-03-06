%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(f(f(G,f(H,G)),f(f(f(I,f(J,K)),f(f(I,J),f(I,K))),L)),L),f(f(n(n(M)),M),N)),N),f(f(O,n(n(O))),P)),P),f(f(f(Q,R),f(n(R),n(Q))),S)),S),T)),T)) )).

fof(f3,conjecture,(
    thm(f(f(a,f(b,c)),f(b,f(a,c)))) )).
%------------------------------------------------------------------------------
