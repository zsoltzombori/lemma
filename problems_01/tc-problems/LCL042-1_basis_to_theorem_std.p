%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(f(f(G,f(n(G),H)),f(f(I,f(J,I)),K)),K),f(f(f(L,f(M,N)),f(M,f(L,N))),O)),O),f(f(f(P,Q),f(f(R,P),f(R,Q))),S)),S),f(f(f(T,U),f(f(n(T),U),U)),V)),V),W)),W)) )).

fof(f3,conjecture,(
    thm(f(f(a,f(b,c)),f(f(a,b),f(a,c)))) )).
%------------------------------------------------------------------------------
