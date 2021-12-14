remove(X,[X|T],T).
remove(X,[Y|T],[Y|T1]):-remove(X,T,T1).

permute([],[]).
permute(L,[X|T]):-remove(X,L,R),permute(R,T).

data1(_, M, N) :- N = kassir, !, M = schetovod.
data1(_,_,_).

data2(_, M, N) :- N = schetovod, !, M = buhgalter.
data2(_,_,_).

data3(L, M, _) :- not(M = kassir), !, not(L = schetovod).
data3(_,_,_).

data4(L, _, N) :- L = buhgalter, !, N = schetovod.
data4(_,_,_).

ans :-
    permute([L, M, N], [buhgalter, kassir, schetovod]),
    data1(L, M, N), data2(L, M, N),
    data3(L, M, N), data4(L, M, N), 
    write("Левин "), write(L), nl, 
    write("Митерев "), write(M), nl, 
    write("Набатов "), write(N), nl.
