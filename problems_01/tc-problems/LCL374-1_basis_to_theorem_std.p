%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ~ thm(f(A,B))
      | ~ thm(A)
      | thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] : thm(f(f(f(f(f(A,f(B,A)),f(f(C,f(D,f(E,D))),F)),F),f(f(f(f(f(f(f(G,H),f(f(H,I),f(G,I))),f(f(f(n(J),J),J),K)),K),f(f(L,f(n(L),M)),N)),N),O)),O)) )).

fof(f3,conjecture,(
    thm(f(f(x,y),f(f(f(x,z),u),f(f(y,u),u)))) )).
%------------------------------------------------------------------------------
