%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(f(G,f(H,G)),f(f(f(I,J),f(f(J,K),f(I,K))),L)),L),f(f(f(f(M,N),N),f(f(N,M),M)),O)),O),f(f(f(f(P,Q),f(Q,P)),f(Q,P)),R)),R),S)),S)) )).

fof(f3,conjecture,(
    thm(f(f(f(x,f(y,x)),f(f(f(f(f(f(f(f(f(z,u),f(f(v,z),f(v,u))),f(f(w,f(v6,w)),v7)),v7),f(f(f(f(v8,v9),v9),f(f(v9,v8),v8)),v10)),v10),f(f(f(f(v11,v12),f(v12,v11)),f(v12,v11)),v13)),v13),f(f(v14,f(v15,v14)),v16))),v16)) )).
%------------------------------------------------------------------------------
