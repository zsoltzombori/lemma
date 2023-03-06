%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(f(f(G,H),H),f(f(H,G),G)),f(f(f(n(I),n(J)),f(J,I)),K)),K),f(f(f(L,M),f(f(M,N),f(L,N))),O)),O),f(f(P,f(Q,P)),R)),R),S)),S)) )).

fof(f3,conjecture,(
    thm(f(f(f(sk16,sk15),f(sk15,sk16)),f(sk15,sk16))) )).
%------------------------------------------------------------------------------
