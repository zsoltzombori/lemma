%------------------------------------------------------------------------------
fof(f1,axiom,(
    ! [A,B] : 
      ( ( thm(f(A,B))
        & thm(A) )
     => thm(B) ) )).

fof(f2,axiom,(
    ! [A,B,C,D] : thm(f(f(f(A,B),C),f(f(C,A),f(D,A)))) )).

fof(f3,conjecture,(
    thm(f(f(p,q),f(f(f(p,r),s),f(f(q,s),s)))) )).
%------------------------------------------------------------------------------
