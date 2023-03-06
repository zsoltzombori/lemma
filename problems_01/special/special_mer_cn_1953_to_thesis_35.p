%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ( thm(f(A,B))
        & thm(A) )
     => thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D,E] : thm(f(f(f(f(f(A,B),f(n(C),n(D))),C),E),f(f(E,A),f(D,A)))) )).

fof(f3,conjecture,(
    thm(f(f(p,f(q,r)),f(f(p,q),f(p,r)))) )).
%------------------------------------------------------------------------------