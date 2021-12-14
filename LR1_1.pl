length([], 0). 
length([_|Y], N) :- length(Y, N1), N is N1 + 1.

members(A,[A|_]).
members(A,[_|Z]) :- members(A, Z).

append([],X,X).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).

remove(X,[X|T],T).
remove(X,[Y|T],[Y|T1]) :- remove(X,T,T1).

sublist(S,L) :- append(_,L1,L), append(S,_,L1), !.

permute([],[]) :- !.
permute(L,[X|T]) :- remove(X,L,R),permute(R,T), !.

%del_n(0,T,T).
del_n(1,[_|T],T):-!.
del_n(N,[_,Y|T],L):- N1 is N - 1, del_n(N1,[Y|T],L).

deletelast(_,[],[]) :- !.
deletelast(N,_,[]):- N < 1, !.
deletelast(N,[H|T],[H|TT]):- N1 is N - 1, deleteLast(N1,T,TT). 

?-deletelast(2,[1,2,3],X), write(X), nl.

remove_n(N,L,X) :- 
	length(L, M), K is M - N, K > -1,
	append(_,X,L), length(X, K), !.



compare([X|_],[Y|_]) :-  X > Y, write("First more than second"), nl, !.
compare([X|_],[Y|_]) :- X < Y, write("First less than second"), nl, !.

compare([],[]) :- write("First equal second"), nl, !.
compare([],[_|_]) :- write("First more than second"), nl, !.
compare([_|_],[]) :- write("First less than second"), nl, !.

compare([X|A],[Y|B]) :- X = Y, compare(A, B), !.

cmp([],[]) :- writeln("First equal second"), !.
cmp([],[_|_]) :- writeln("First more than second"), !.
cmp([_|_],[]) :- writeln("First less than second"), !.

cmp(A,B) :- append(M,[X|_],A), append(N,[Y|_],B),
length(M,L1), length(N,L2), L1 = L2, (  X > Y, writeln("First more than second"); 
X < Y, writeln("First less than second") ), !.

cmp(A,B) :- append(M,[X|_],A), append(N,[Y|_],B),
length(M,L1), length(N,L2), X = Y, L1 = L2, length(A, S1), length(B,S2),
S1 = S2, writeln("First equal second"), !.
cmp(A,B) :- append(M,[X|_],A), append(N,[Y|_],B),
length(M,L1), length(N,L2), X = Y, L1 = L2, length(A, S1), length(B,S2),
S1 < S2, writeln("First more than second"), !.
cmp(A,B) :- append(M,[X|_],A), append(N,[Y|_],B),
length(M,L1), length(N,L2), X = Y, L1 = L2, length(A, S1), length(B,S2),
S1 > S2, writeln("First less than second"), !.

eql(A,B) :- length(A,N), length(B,M), N > M, K is N - M, remove_n(K,A,L), cmp(L,B), !.
eql(A,B) :- length(A,N), length(B,M), N < M, K is M - N, remove_n(K,B,L), cmp(A,L), !.
eql(A,B) :- length(A,N), length(B,M), N = M, cmp(A,B), !.