length([], 0). 
length([_|Y], N) :- length(Y, N1), N is N1 + 1.

append([], X, X).
append([X|M], K, [X|L]):- append(M, K, L).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

is_ball(w).
is_ball(b).
near1([w,e|T],[e,w|T]).
near1([e,b|T],[b,e|T]).
jump1([w,X,e|T],[e,X,w|T]) :- is_ball(X).
jump1([e,X,b|T],[b,X,e|T]) :- is_ball(X).
near2([e,w|T],[w,e|T]).
near2([b,e|T],[e,b|T]).
jump2([e,X,w|T],[w,X,e|T]) :- is_ball(X).
jump2([b,X,e|T],[e,X,b|T]) :- is_ball(X).

move(L1,L2) :- near1(L1,L2).
move(L1,L2) :- jump1(L1,L2).
move(L1,L2) :- near2(L1,L2).
move(L1,L2) :- jump2(L1,L2).
move([X|T1],[X|T2]) :- move(T1,T2).

printlist([]) :- !.
printlist([A|L]):- write(A), nl, printlist(L).

prolong([X|T],[Y,X|T]) :- move(X,Y), not(member(Y,[X|T])).

/* Поиск в глубину */
dpth([X|T],X,R) :- reverse([X|T], R).
dpth(P,Y,R) :- prolong(P,P1), dpth(P1,Y,R).
dfs(X,Y,R) :- dpth([X],Y,R). 

/* Поиск в ширину*/
bdth([[X|T]|_],X,R) :- reverse([X|T], R).
bdth([P|QI],Y,R) :- findall(Z,prolong(P,Z),T), 
    append(QI,T,QO), !, 
    bdth(QO,Y,R).
bdth([_|T],Y,R) :- bdth(T,Y,R).
bfs(X,Y,R) :- bdth([[X]],Y,R).

/* Поиск с итерационным заглублением*/
num(1).
num(N):-num(M), N is M + 1.

depth_id([Finish|Tail], Finish, R, 0) :- reverse([Finish|Tail], R).
depth_id(TempPath, Finish, Path, N):-
    N > 0, N1 is N - 1,
    prolong(TempPath, NewPath),
    depth_id(NewPath, Finish,Path, N1).

iter_search(Start,Finish,Path):-
    num(Level), 
    (Level > 15, !; 
    depth_id([Start],Finish,Path,Level)).

solve_d :- dfs([w,w,w,e,b,b,b],[b,b,b,e,w,w,w],P), length(P, Len), write(Len), nl, printlist(P), nl.
solve_b :- bfs([w,w,w,e,b,b,b],[b,b,b,e,w,w,w],P), length(P, Len), write(Len), nl, printlist(P), nl.
solve_i :- iter_search([w,w,w,e,b,b,b],[b,b,b,e,w,w,w],P), length(P, Len), write(Len), nl, printlist(P), nl.

?- solve_d.
