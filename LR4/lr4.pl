append([],X,X).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).

member(A,[A|_]).
member(A,[_|Z]) :- member(A, Z).

adverb(every).

particle(that).
particle(those).

verb(lives).
verb(loves).

article(a).
article(the).

object(man).
object(woman).
object(brother).
object(sister).


get_obj([Art,Obj|T], Res, T) :- 
	article(Art),
	object(Obj),
	Res = Obj.
get_obj([Obj|T], Res, T) :- 
	object(Obj),
	Res = Obj.

get_verb([Verb|T], Res, T) :-
	verb(Verb),
	Res = Verb.

check_part([Part|T], T) :-
	particle(Part).

check_adv([Adv|T], T) :-
	adverb(Adv).

test(Ask, Res):-
	check_adv(Ask, Ask0),
	get_obj(Ask0, Obj1, Ask1),
	check_part(Ask1, Ask2),
	get_verb(Ask2, Verb_1, Ask3),
	get_verb(Ask3, Verb_2, Ask4),
	get_obj(Ask4, Obj2, _),
	Early_char = 'X',
	After_char = 'Y',
	Early_obj=..[Obj1,Early_char],
	Later_obj=..[Obj2,After_char],
	V1=..[Verb_1, Early_char],
	V2=..[Verb_2, Early_char, After_char],
	Exist_part=..[exist, After_char, Later_obj, V2],
	Res=..[all,Early_char,Early_obj,'&',V1,'=>',Exist_part], !.


