%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(G,f(H,G)),f(f(f(f(I,falsehood),falsehood),I),J)),J),f(f(f(K,f(L,M)),f(f(K,L),f(K,M))),N)),N),O)),O)) )).

fof(f3,conjecture,(
    thm(f(f(f(a,b),a),a)) )).
%------------------------------------------------------------------------------
