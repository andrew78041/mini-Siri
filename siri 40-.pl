
%-------------------------------------------------------------
% res(-Sentence)
%-------------------------------------------------------------

res([FirstWord|RestOfSentence]) :-
  reSe([FirstWord|RestOfSentence]).

reSe([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord(Char,FirstWord,NextChar),
  readRestOfSentence(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res -------------------------
   readRestOfSentence(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence(_,Char,[NextWord|RestOfSentence]) :-
     readWord(Char,NextWord,NextChar),
     readRestOfSentence(NextWord,NextChar,RestOfSentence).

   readWord(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord(Char,Word,NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord(_,Word,NextChar) :-
     get0(TempChar),
     readWord(TempChar,Word,NextChar).

   restWord(Char,[NewChar|RestWord],NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar).
     restWord(Char,[],Char).

   singleCharWord(44).  /* , */
   singleCharWord(59).  /* ; */
   singleCharWord(58).  /* : */
   singleCharWord(63).  /* ? */
   singleCharWord(33).  /* ! */
   singleCharWord(46).  /* . */

   componentChar(Char,Char) :- Char>96,Char<123.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar(Char,Char) :- Char>47,Char<58.
   componentChar(39,39).  /* ' */
   componentChar(45,45).  /* - */
   componentChar(95,95).  /* _ */

   endOfSentenceWord('.').
   endOfSentenceWord('!').
   endOfSentenceWord('?').

%-------------------------------------------------------------
% res_pc(-Sentence)
%-------------------------------------------------------------

res_pc([FirstWord|RestOfSentence]) :-
  reSe_pc([FirstWord|RestOfSentence]).

reSe_pc([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord_pc(Char,FirstWord,NextChar),
  readRestOfSentence_pc(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res_pc -------------------------
   readRestOfSentence_pc(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence_pc(_,Char,[NextWord|RestOfSentence]) :-
     readWord_pc(Char,NextWord,NextChar),
     readRestOfSentence_pc(NextWord,NextChar,RestOfSentence).

   readWord_pc(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord_pc(Char,Word,NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord_pc(_,Word,NextChar) :-
     get0(TempChar),
     readWord_pc(TempChar,Word,NextChar).

   restWord_pc(Char,[NewChar|RestWord],NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar).
     restWord_pc(Char,[],Char).

   componentChar_pc(Char,Char) :- Char>96,Char<123.

   componentChar_pc(Char,Char) :- Char>64,Char<91.

   componentChar_pc(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar_pc(Char,Char) :- Char>47,Char<58.
   componentChar_pc(39,39).  /* ' */
   componentChar_pc(45,45).  /* - */
   componentChar_pc(95,95).  /* _ */

%-------------------------------------------------------------
% ws(+Sentence)
%-------------------------------------------------------------

ws([F|R]) :-
   write(F),
   wrs(R).

   %--- ancillaries to ws ------------------------
   wrs([F|R]) :-
     write(' '),
     write(F),
     wrs(R).
   wrs([]).

%-------------------------------------------------------------
% space/0
%-------------------------------------------------------------

space :- write(' ').

%-------------------------------------------------------------
% rs(-String)
%-------------------------------------------------------------

rs(S) :-
   get0(C),
   (
      C == -1,  S = [], !, fail;
      C == 10,  S = [], ! ;
      C == 32, !, rs(S);
      !, rs(C,S)
   ).

rs(C,[C|Cs]) :-
   get0(D),
   (
      D == -1,  Cs = [], !, fail;
      D == 10,  Cs = [], ! ;
      D == 32,  Cs = [], ! ;
      !, rs(D,Cs)
   ).


%-------------------------------------------------------------
% wrst(+String)
%-------------------------------------------------------------

wrst([]) :- !.
wrst([C|Cs]) :- put(C), wrst(Cs).



:-discontiguous(prop/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% VEGETABLES %%%%

prop(tomato,is,vegetable).
prop(onion,is,vegetable).
prop(bell_pepper,is,vegetable).
prop(chili_pepper,is,vegetable).
prop(carrot,is,vegetable).
prop(pea,is,vegetable).
prop(artichoke,is,vegetable).
prop(eggplant,is,vegetable).
prop(cucumber,is,vegetable).
prop(lettuce,is,vegetable).
prop(okra,is,vegetable).
prop(cauliflower,is,vegetable).
prop(cabbage,is,vegetable).
prop(broccoli,is,vegetable).
prop(mushroom,is,vegetable).
prop(potato,is,vegetable).
prop(zucchini,is,vegetable).
prop(broccoli,is,vegetable).
prop(spinach,is,vegetable).
prop(corn,is,vegetable).

%%%% FRUITS %%%%

prop(strawberry,is,fruit).
prop(blackberry,is,fruit).
prop(blueberry,is,fruit).
prop(banana,is,fruit).
prop(orange,is,fruit).
prop(grape,is,fruit).
prop(pineapple,is,fruit).
prop(apple,is,fruit).
prop(kiwi,is,fruit).
prop(peaches,is,fruit).
prop(guava,is,fruit).
prop(pear,is,fruit).
prop(mango,is,fruit).
prop(apricot,is,fruit).
prop(avocado,is,fruit).
prop(cherry,is,fruit).
prop(fig,is,fruit).
prop(coconut,is,fruit).
prop(lemon,is,fruit).
prop(watermelon,is,fruit).
prop(cantaloupe,is,fruit).

%%%% DIARY %%%%

prop(cheese,is,diary).
prop(milk,is,diary).
prop(yogurt,is,diary).

%%%% CARBS %%%%

prop(flour,is,carb).
prop(rice,is,carb).
prop(pasta,is,carb).
prop(chocolate,is,carb).

%%%% FATS %%%%

prop(oil,is,fat).
prop(butter,is,fat).

%%%% PROTEINS %%%%

prop(egg,is,protein).
prop(fish,is,protein).
prop(chicken,is,protein).
prop(meat,is,protein).
prop(shrimp,is,protein).
prop(minced_meat,is,protein).

%%%% DRESSING %%%%

prop(mayonnaise,is,dressing).
prop(vinegar,is,dressing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(chicken_caesar_salad,contain,chicken).
prop(chicken_caesar_salad,contain,oil).
prop(chicken_caesar_salad,contain,lettuce).
prop(chicken_caesar_salad,contain,cheese).
prop(chicken_caesar_salad,contain,mayonnaise).
prop(chicken_caesar_salad,contain,vinegar).
prop(chicken_caesar_salad,contain,bread).

prop(green_salad,contain,carrot).
prop(green_salad,contain,bell_pepper).
prop(green_salad,contain,lettuce).
prop(green_salad,contain,onion).
prop(green_salad,contain,tomato).
prop(green_salad,contain,cucumber).

prop(coleslaw_salad,contain,carrot).
prop(coleslaw_salad,contain,cabbage).
prop(coleslaw_salad,contain,mayonnaise).
prop(coleslaw_salad,contain,oil).

prop(pasta_salad,contain,bell_pepper).
prop(pasta_salad,contain,mayonnaise).
prop(pasta_salad,contain,pasta).
prop(pasta_salad,contain,corn).

prop(fruit_salad,contain,strawberry).
prop(fruit_salad,contain,banana).
prop(fruit_salad,contain,orange).
prop(fruit_salad,contain,apple).

prop(croissant,contain,butter).
prop(croissant,contain,flour).
prop(croissant,contain,milk).
prop(croissant,contain,oil).
prop(croissant,contain,egg).

prop(spanish_omelette,contain,egg).
prop(spanish_omelette,contain,oil).
prop(spanish_omelette,contain,potato).

prop(boiled_egg,contain,egg).

prop(grilled_chicken,contain,chicken).
prop(grilled_chicken,contain,lemon).
prop(grilled_chicken,contain,onion).

prop(fried_chicken,contain,chicken).
prop(fried_chicken,contain,oil).
prop(fried_chicken,contain,onion).
prop(fried_chicken,contain,flour).

prop(cake,contain,flour).
prop(cake,contain,butter).
prop(cake,contain,milk).
prop(cake,contain,egg).

prop(chocolate_cake,contain,cake).
prop(chocolate_cake,contain,chocolate).

prop(white_rice,contain,rice).
prop(white_rice,contain,butter).

prop(mexican_rice,contain,rice).
prop(mexican_rice,contain,oil).
prop(mexican_rice,contain,onion).
prop(mexican_rice,contain,tomato).

prop(ratatouille,contain,zucchini).
prop(ratatouille,contain,eggplant).
prop(ratatouille,contain,tomato).
prop(ratatouille,contain,bell_pepper).
prop(ratatouille,contain,onion).
prop(ratatouille,contain,lemon).
prop(ratatouille,contain,oil).
prop(ratatouille,contain,vinegar).

prop(lasagne,contain,pasta).
prop(lasagne,contain,milk).
prop(lasagne,contain,flour).
prop(lasagne,contain,butter).
prop(lasagne,contain,minced_meat).
prop(lasagne,contain,cheese).

prop(pasta_white_sauce,contain,pasta).
prop(pasta_white_sauce,contain,milk).
prop(pasta_white_sauce,contain,flour).
prop(pasta_white_sauce,contain,butter).

prop(pasta_red_sauce,contain,pasta).
prop(pasta_red_sauce,contain,tomato).
iprop(pasta_red_sauce,contain,oil).

prop(pasta_alfredo,contain,pasta).
prop(pasta_alfredo,contain,milk).
prop(pasta_alfredo,contain,flour).
prop(pasta_alfredo,contain,butter).
prop(pasta_alfredo,contain,chicken).

prop(pasta_negresco,contain,pasta).
prop(pasta_negresco,contain,milk).
prop(pasta_negresco,contain,flour).
prop(pasta_negresco,contain,butter).
prop(pasta_negresco,contain,chicken).
prop(pasta_negresco,contain,cheese).

prop(shrimp_pasta,contain,pasta).
prop(shrimp_pasta,contain,shrimp).
prop(shrimp_pasta,contain,butter).
prop(shrimp_pasta,contain,milk).

prop(pizza,contain,tomato).
prop(pizza,contain,cheese).
prop(pizza,contain,flour).
prop(pizza,contain,oil).

prop(bread,contain,milk).
prop(bread,contain,flour).
prop(bread,contain,butter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(tomato,contain,11,cal).
prop(onion,contain,44,cal).
prop(cheese,contain,431,cal).
prop(egg,contain,78,cal).
prop(pasta,contain,131,cal).
prop(bell_pepper,contain,24,cal).
prop(chili_pepper,contain,18,cal).
prop(carrot,contain,25,cal).
prop(pea,contain,81,cal).
prop(artichoke,contain,120,cal).
prop(eggplant,contain,25,cal).
prop(cucumber,contain,32,cal).
prop(lettuce,contain,15,cal).
prop(okra,contain,33,cal).
prop(cauliflower,contain,25,cal).
prop(cabbage,contain,25,cal).
prop(broccoli,contain,31,cal).
prop(mushroom,contain,5,cal).
prop(potato,contain,163,cal).
prop(zucchini,contain,33,cal).
prop(spinach,contain,23,cal).
prop(corn,contain,86,cal).
prop(strawberry,contain,33,cal).
prop(blackberry,contain,43,cal).
prop(blueberry,contain,57,cal).
prop(banana,contain,89,cal).
prop(orange,contain,47,cal).
prop(grape,contain,62,cal).
prop(pineapple,contain,42,cal).
prop(apple,contain,92,cal).
prop(kiwi,contain,42,cal).
prop(peaches,contain,59,cal).
prop(guava,contain,38,cal).
prop(pear,contain,85,cal).
prop(mango,contain,99,cal).
prop(apricot,contain,48,cal).
prop(avocado,contain,160,cal).
prop(cherry,contain,50,cal).
prop(fig,contain,107,cal).
prop(coconut,contain,283,cal).
prop(lemon,contain,24,cal).
prop(watermelon,contain,30,cal).
prop(cantaloupe,contain,34,cal).
prop(milk,contain,124,cal).
prop(yogurt,contain,218,cal).
prop(flour,contain,364,cal).
prop(rice,contain,150,cal).
prop(oil,contain,240,cal).
prop(butter,contain,204,cal).
prop(fish,contain,305,cal).
prop(chicken,contain,335,cal).
prop(meat,contain,250,cal).
prop(shrimp,contain,85,cal).
prop(minced_meat,contain,332,cal).
prop(mayonnaise,contain,188,cal).
prop(vinegar,contain,3,cal).
prop(chocolate,contain,137,cal).
%prop(,contain,,cal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(cheese,not,lunch).
prop(yogurt,not,lunch).
prop(boiled_egg,not,lunch).
prop(boiled_egg,not,dinner).
prop(spanish_omelette,not,lunch).
prop(spanish_omelette,not,dinner).
prop(croissant,not,lunch).
prop(chicken_caesar_salad,not,breakfast).
prop(chicken_caesar_salad,not,dinner).
prop(pizza,not,breakfast).
prop(shrimp_pasta,not,breakfast).
prop(shrimp_pasta,not,dinner).
prop(pasta_negresco,not,breakfast).
prop(pasta_negresco,not,dinner).
prop(pasta_alfredo,not,breakfast).
prop(pasta_alfredo,not,dinner).
prop(pasta_red_sauce,not,breakfast).
prop(pasta_red_sauce,not,dinner).
prop(pasta_white_sauce,not,breakfast).
prop(pasta_white_sauce,not,dinner).
prop(fried_chicken,not,breakfast).
prop(fried_chicken,not,dinner).
prop(grilled_chicken,not,breakfast).
prop(grilled_chicken,not,dinner).
prop(lasagne,not,breakfast).
prop(lasagne,not,dinner).
prop(ratatouille,not,breakfast).
prop(ratatouille,not,dinner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% When the food category or the food name is unknown.


		
		
%%bta3na

	
readInputTillQuit:-
    write("welcome to your personal assistant"),
    res(Q),
    joly(Q,[],[]).
	
joly([quit,.],PQ,PR):-
	write("bye"),nl,
	getmealB(PQ,B),
	getmealL(PQ,L),
	getmealD(PQ,D),
	write("you had " ),
	write(B),write("for breakfast"),nl,
	write("you had " ),
	write(L),write("for lunch"),nl,
	write("you had " ),
	write(D),write("for dinner").
	
joly(Q,[],[]):-
	Q\=[quit,.],
	isValid(Q),
	mydelete(Q,Q2),
	response(Q2,[],[],R),
	addtopreviousQ(Q2,[],H),
	ws(R),
	addtopreviousR(R,[],Z),
	res(N),
	joly(N,H,Z).
	
joly(Q,PQ,PR):-
	Q\=[quit,.],
	isValid(Q),
	mydelete(Q,Q2),
	PQ\=[],
	PR\=[],
	response(Q2,PQ,PR,A),
	ws(A),
	addtopreviousQ(Q2,PQ,H),
	addtopreviousR(A,PR,Z),
	res(N),
	joly(N,H,Z).
	
joly(Q,PQ,PR):-
		Q\=[quit,.],
		\+isValid(Q),
		write("i can not understand you"),
		res(N),
		joly(N,H,Z).


mydelete(Q,Q2):-
		delete(Q,.,Q2).
mydelete(Q,Q2):-
		delete(Q,?,Q2).	
getmealB([],[]).
getmealB([[i,ate,F,for,breakfast]|T],[F|T2]):-
	getmealB(T,T2).
getmealB([H|T],T2):-
	H\=[i,ate,F,for,breakfast],
	getmealB(T,T2).

getmealL([],[]).
getmealL([[i,ate,F,for,lunch]|T],[F|T2]):-
	getmealL(T,T2).
getmealL([H|T],T2):-
	H\=[i,ate,F,for,lunch],
	getmealL(T,T2).
	
getmealD([],[]).
getmealD([[i,ate,F,for,dinner]|T],[F|T2]):-
	getmealD(T,T2).
getmealD([H|T],T2):-
	H\=[i,ate,F,for,dinner],
	getmealD(T,T2).	
	
addtopreviousQ(H,T,[H|T]).
addtopreviousR(H,T,[H|T]).	


isValid([i,do,not,eat,I,.]).
isValid([how,many,calories,does,F,contain,?]).
isValid([what,does,F,contain,?]).
isValid([can,I,have,F,for,MT,?]).
isValid([what,is,I,?]).
isValid([how,many,calories,do,I,have,left,?]).
isValid([what,kind,of,FC,does,F,contain,?]).
isValid([is,Ij,a,FC,in,F,?]).
isValid([what,can,I,have,for,MT,that,contains,Ij,?]).
isValid([i,ate,F,for,MT,.]).



filterProp(is,L):-
      bagof((A,B),prop(A,is,B),L).

filterProp(not,L):-
     bagof((A,B),prop(A,not,B),L).


filterProp(contain,L):-
     bagof((A,B),prop(A,contain,B),L).


matchF(T1,[],[]).
matchF(T1,[(T1,B)|T],[B-Occ|T2]):-
	Occ = 1,
	matchF(T1,T,T2).
	
matchF(T1,[(A,B)|T],[B-Occ|T2]):-
	Occ=0,
	T1\=A,
	matchF(T1,T,T2).
	
matchSecond(T1,[],[]).
matchSecond(T1,[(B,T1)|T],[B-Occ|T2]):-
	Occ = 1,
	matchSecond(T1,T,T2).
	
matchSecond(T1,[(B,A)|T],[B-Occ|T2]):-
	Occ=0,
	T1\=A,
	matchSecond(T1,T,T2).	

	
mergeMatchLists([],[],[]).
mergeMatchLists([],T,T).
mergeMatchLists(T,[],T).
mergeMatchLists(L,L2,L1):-
		append(L,L2,L3),
		andrew(L3,L1).
	
andrew([],[]).	
andrew([H1-Occ|T],[H1New|T3]):-
		andrew2(T,H1-Occ,H1New),
		delete([H1-Occ|T],H1-_,L),
		andrew(L,T3).



andrew2([],Acc,Acc).		
andrew2([H-Occ1|T],H-Occ,Res):-
			Occ2 is Occ+Occ1,
			andrew2(T,H-Occ2,Res)
			.
andrew2([H1-Occ1|T],H-Occ,Res):-
			H\=H1,
			andrew2(T,H-Occ,Res).
						
	


foodCal(F,C):-
  prop(F,contain,C,cal).	
foodCalF([],0).
foodCalF([H|T],Sum):-
  foodCal(H,C),
  foodCalF(T,S),
  Sum is C+S.

foodCal(F,C):-
  bagof(I,prop(F,contain,I),L),
  foodCalF(L,C).



foodCalL([],0).
foodCalL([H|T],C):-
		foodCal(H,C1),
		foodCalL(T,C2),
		C is C1+C2.

eated([],[],[]).

eated([Q|T],[R|T3],[F|T2]):-
  Q=[i,ate,F,for,_],
  eated(T,T3,T2).

eated([Q|T],[R|T3],T2):-
  Q\=[i,ate,F,for,_],
  Q\=[can,i,have,F,for,_],
  eated(T,T3,T2).

eated([Q|T],[R|T2],[F|T3]):-
  Q=[can,i,have,F,for,_],
  R=["You",can,have,F,for,_],
  eated(T,T2,T3).

eated([Q|T],[R|T2],T3):-
  Q=[can,i,have,F,for,_],
  R\=["You",can,have,F,for,_],
  eated(T,T2,T3).

calcCalories(F,PQ,PR,C):-
  eated(PQ,PR,L),
	foodCalL(L,A),
  foodCal(F,Z),
	C is 1800-A-Z.


getDiffAnswer(H,[],[],[H2|T2],H2). 

getDiffAnswer(H,[H|T],[[H1]|T1],CR,L2):-
	delete(CR,H1,[R|T2]),
  getDiffAnswer(H,T,T1,[R|T2],L2).

getDiffAnswer(Q,[H|T],[H1|T1],CR,L):-	  
	Q\=H,
	getDiffAnswer(Q,T,T1,CR,L).
	



bestmatches([],[]).
bestmatches([E-Occ|T],BL):-
	max1(T,Occ,M),
	helper3([E-Occ|T],M,BL).

max1([],A,A).
max1([E-Occ|T],A,R):-
	Occ > A,
	max1(T,Occ,R).
max1([E-Occ|T],A,R):-
	Occ =< A,
	max1(T,A,R).

helper3([],M,[]).
helper3([E-M|T],M,[E|T2]):-
	helper3(T,M,T2).
helper3([E-Occ|T],M,L):-
	Occ\=M,
	helper3(T,M,L).

listOrderDesc([],[]).
listOrderDesc(R,L):-
  insertion_sort(R,L).

insertion_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],X,X).
i_sort([H|T],Accumulator,Sorted):-insert(H,Accumulator,N),i_sort(T,N,Sorted).
insert(X-Occ,[Y-Occ2|T],[Y-Occ2|NT]):- Occ<Occ2,insert(X-Occ,T,NT).
insert(X-Occ,[Y-Occ2|T],[X-Occ,Y-Occ2|T]):- Occ>=Occ2.
insert(X,[],[X]).	



checkFood(F,prop(F,_,_)).

foodFromHistory([],[]).
foodFromHistory([H|T],[F|T1]):-
  H=[i,ate,F,for,MT],
  checkFood(F,prop(F,_,_)),
  foodFromHistory(T,T1).
foodFromHistory([H|T],[F|T1]):-
  H=[you,can,have,F,for,MT],
  checkFood(F,prop(F,_,_)),
  foodFromHistory(T,T1).
foodFromHistory([H|T],T1):-
  H\=[you,can,have,F,for,MT],
  H\=[i,ate,F,for,MT],
  foodFromHistory(T,T1).

response(Q,_,_,["I",do,not,know]) :-
    Q = [what,kind,of,FC,does,F,contain],
    ((\+ prop(_,_,FC));
    (\+prop(F,_,_))).


isDIFFRENT(F,[how,many,calories,does,Q,contain]):-
        F\=Q.


newQuestion(Q,[]).         
newQuestion([how,many,calories,does,Q,contain],[H|T]):-
    [how,many,calories,does,Q,contain]=H,
    isDIFFRENT(Q,H),
    newQuestion([how,many,calories,does,Q,contain],T).
	
newQuestion([how,many,calories,does,Q,contain],[H|T]):-
    [how,many,calories,does,Q,contain]\=H,
    newQuestion([how,many,calories,does,Q,contain],T).


response(Q,PQ,PR,[N,"calories"]):-
 Q=[how,many,calories,does,F,contain],
 foodCal(F,N),
 newQuestion(Q,PQ).
  


response(Q,PQ,PR,["I",told,you,that,before]) :-
    Q=[how,many,calories,does,F,contain],
    \+response(Q,PQ,PR,["I",do,not,know]),
    \+response(Q,PQ,PR,[N,"calories"]),
    \+newQuestion(Q,PQ).
    
response([how,many,calories,does,F,contain],PQ,PR,["I",do,not,know]):-
    \+prop(F,_,_),
     newQuestion(Q,PQ).
	 
response(Q,PQ,PR,[R]):-
  Q=[what,does,F,contain],
  bagof(I,prop(F,contain,I),L),
  getDiffAnswer(Q,PQ,PR,L,R).

response(Q,PQ,PR,["I",do,not,know]):-
  Q=[what,does,F,contain],
  \+prop(F,contain,_).

response(Q,PQ,PR,["I",told,you,that,before]):-
  Q=[what,does,F,contain],
  \+response(Q,PQ,PR,[R]),
  \+response(Q,PQ,PR,["I",do,not,know]).

response([can,i,have,F,for,M],PQ,PR,[F,"is",not,suitable,for,M]):-
  prop(F,not,M),
  newQuestion2([can,i,have,F,for,M],PQ).
  
response([can,i,have,F,for,M],PQ,PR,["You",can,have,F,for,M]):-
  \+response([can,i,have,F,for,M],PQ,PR,[F,"is",not,suitable,for,M]),
  calcCalories(F,PQ,PR,C),
  \+response([can,i,have,F,for,M],PQ,PR,["I",told,you,that,before]),
  C>=0.
response([can,i,have,F,for,M],PQ,PR,["No"]):-
  \+response([can,i,have,F,for,M],PQ,PR,[F,"is",not,suitable,for,M]),
  calcCalories(F,PQ,PR,C),
  \+response([can,i,have,F,for,M],PQ,PR,["I",told,you,that,before]),
  C<0.

response([can,i,have,F,for,M],PQ,PR,["I",do,not,know]):-
  \+prop(F,contain,_),
  \+prop(F,contain,_,_),
  newQuestion2([can,i,have,F,for,M],PQ).
  
response([can,i,have,F,for,M],PQ,PR,["I",told,you,that,before]):-
    prop(F,_,_),
    \+newQuestion2([can,i,have,F,for,M],PQ).



isDIFFRENT2(F,M2,[can,i,have,Q,for,M]):-
        F\=Q.
isDIFFRENT2(F,M2,[can,i,have,Q,for,M]):-
        M2\=M.
newQuestion2(Q,[]).         
newQuestion2([can,i,have,F,for,M],[H|T]):-
    [can,i,have,F,for,M]=H,
    isDIFFRENT2(F,M,H),
    newQuestion2([can,i,have,Q,for,M],T).
newQuestion2([can,i,have,Q,for,M],[H|T]):-
    [can,i,have,Q,for,M]\=H,
    newQuestion2([can,i,have,Q,for,M],T).
 

 
 
 
isDIFFRENT3(F,[what,is,Q]):-
		F\=Q.
newQuestion3(Q,[]).		
newQuestion3([what,is,F],[H|T]):-
	[what,is,F]=H,
	isDIFFRENT3(F,H),
	newQuestion3([What,is,F],T).
newQuestion3([what,is,F],[H|T]):-
	[what,is,F]\=H,
	newQuestion3([What,is,F],T).	
	
response([what,is,F],PQ,PR,S):-
		prop(F,is,S),
		newQuestion3([what,is,F],PQ).	
	
response([what,is,F],PQ,PR,[i,do,not,know]):-
		\+prop(F,is,_),
		newQuestion3([what,is,F],PQ).
		
response([what,is,F],PQ,PR,S):-
		prop(F,is,S),
		newQuestion3([what,is,F],PQ).
		
response([what,is,F],PQ,PR,[i,told,you,before]):-
		\+newQuestion3([what,is,F],PQ).
		

	
response([is,F,a,FC,in,FT],PQ,PR,["Yes"]):-
	prop(F,is,FC),
	prop(FT,contain,F),
	bagof(F,prop(FT,contain,F),L),
	member(F,L),
	newQuestion9(Q,PQ).

	

isDIFFRENT9(Q,W,E,[is,F,a,FC,in,FT]):-
        F\=Q.
isDIFFRENT9(Q,W,E,[is,F,a,FC,in,FT]):-
        W\=FC.
isDIFFRENT9(Q,W,E,[is,F,a,FC,in,FT]):-
        E\=FT.	
newQuestion9(Q,[]).         
newQuestion9([is,F,a,FC,in,FT],[H|T]):-
    [is,F,a,FC,in,FT]=H,
    isDIFFRENT9(F,FC,FT,H),
    newQuestion9([is,F,a,FC,in,FT],T).
newQuestion9([is,F,a,FC,in,FT],[H|T]):-
    [is,F,a,FC,in,FT]\=H,
    newQuestion9([is,F,a,FC,in,FT],T).
	
response([is,F,a,FC,in,FT],PQ,PR,[i,do,not,know]):-
	\+prop(F,is,FC);
	bagof(F,prop(FT,contain,F),L),
	\+member(F,L),
	newQuestion9(Q,PQ).
	
response([is,F,a,FC,in,FT],PQ,PR,["No"]):-	
	\+response([is,F,a,FC,in,FT],PQ,PR,["Yes"]),
	\+response([is,F,a,FC,in,FT],PQ,PR,[i,do,not,know]),
	newQuestion9(Q,PQ).

	
response([is,F,a,FC,in,FT],PQ,PR,[i,told,you,before]):-
	\+newQuestion9(Q,PQ).

validQuestion([i,ate,F,for,_],[]).
validQuestion([i,ate,F,for,_],[H|T]):-
		[i,ate,F,for,_]=H,
		prop(F,_,_),
		validQuestion([i,ate,F,for,_],T).
validQuestion([i,ate,F,for,_],[H|T]):-
		[i,ate,F,for,_]\=H,
		validQuestion([i,ate,F,for,_],T).		
			
	
response([how,many,calories,do,i,have,left],[],[],[1800,"calories"]).

response([how,many,calories,do,i,have,left],PQ,PR,[Z,calories]):-
		PQ\=[],
		PR\=[],
		validQuestion(Q,PQ),
		eated(PQ,PR,L),
		foodCalF(L,A),
		Z is 1800-A.
		
response([how,many,calories,do,i,have,left],PQ,PR,[i,do,not,know]):-	
		\+validQuestion(Q,PQ).
		

		
response([i,ate,F,for,MT],PQ,PR,["ok"]):-
		prop(F,_,_),
		prop(_,_,MT).
response([i,do,not,eat,FT],PQ,PR,["ok"]):-
		prop(_,_,FT).
		
		
response(Q,PQ,PR,[R]):-
  Q=[what,kind,of,CT,does,F,contain],
  bagof(I,prop(F,contain,I),L),
  foodCat(L,CT,L2),
  getDiffAnswer(Q,PQ,PR,L2,R).


  foodCat([],_,[]).
foodCat([H|T],CT,[H|T2]):-
  prop(H,is,CT),
  foodCat(T,CT,T2).
foodCat([H|T],CT,T2):-
  \+prop(H,is,CT),
  foodCat(T,CT,T2).

response(Q,PQ,PR,["Nothing", from, what, i, know]):-
  Q=[what,kind,of,CT,does,F,contain],
  bagof(I,prop(F,contain,I),L),
  foodCat(L,CT,L2),
  length(L2,Len),
  Len=0.

response(Q,PQ,PR,["I", told, you, that, before]):-
  Q=[what,kind,of,CT,does,F,contain],
  \+response(Q,PQ,PR,[R]),
  \+response(Q,PQ,PR,["Nothing", from, what, i, know]),
  \+response(Q,PQ,PR,["I",do,not,know]).

response(Q,_,_,["I",do,not,know]) :-
    Q = [what,kind,of,FC,does,F,contain],
    ((\+ prop(_,_,FC));
    (\+prop(F,_,_))).




isDIFFRENT5(F,CT,[what,kind,of,CT,does,Q,contain]):-
        F\=Q.
isDIFFRENT5(F,M,[what,kind,of,CT,does,Q,contain]):-
        CT\=M.


hateL([],[]).

hateL([Q|T2],[I|T1]):-
  Q=[i,do,not,eat,I],
  hateL(T2,T1).

hateL([Q|T2],T1):-
  Q\=[i,do,not,eat,I],
  hateL(T2,T1).

hatedFood(Hated,[Ing|T2]):-
  member(Ing,Hated).
hatedFood(Hated,[Ing|T2]):-
  \+member(Ing,Hated),
  hatedFood(Hated,T2).


hated([],H,[]).

hated([F-S|T],H,[F-S|T2]):-
  bagof(I,prop(F,contain,I),L),
  hatedFood(H,L),
  hated(T,H,T2).

hated([F-S|T],H,[F-S2|T2]):-
  bagof(I,prop(F,contain,I),L),
  \+hatedFood(H,L),
  S2 is S+1,
  hated(T,H,T2).
suitable([],_,[]). 
suitable([F-S|T],M,[F-S|T2]):-
  prop(F,not,M),
  suitable(T,M,T2).

suitable([F-S|T],M,[F-S2|T2]):-
  \+prop(F,not,M),
  S2 is S+1,
  suitable(T,M,T2).

responseO(Q,PQ,PR,LR):-
  Q=[what,can,i,have,for,M,that,contains,I],
  bagof(F-1,prop(F,contain,I),L),
  hateL(PQ,Hated),
  hated(L,Hated,L2),
  suitable(L2,M,L3),
  response0(L3,PQ,PR,LR).



response(Q,PQ,PR,[R]):-
  Q=[what,can,i,have,for,M,that,contains,I],
  responseO(Q,PQ,PR,LR),
  bestmatches(LR,CR),
  getDiffAnswer(Q,PQ,PR,CR,R),
  \+response(Q,PQ,PR,["Nothing", from, what, i, know]).

suitableMeal([],_,[]).

suitableMeal([F|T],M,T2):-
  prop(F,not,M),
  suitableMeal(T,M,T2).
suitableMeal([F|T],M,[F|T2]):-
  \+prop(F,not,M),
  suitableMeal(T,M,T2).


response(Q,PQ,PR,["Nothing", from, what, i, know]):-
  Q=[what,can,i,have,for,M,that,contains,I],
  bagof(F,prop(F,contain,I),L),
  suitableMeal(L,M,L2),
  length(L2,0),
  newQuestion7(Q,PQ).

isDIFFRENT7(F,M2,[what,can,i,have,for,M,that,contains,I]):-
        F\=I.
isDIFFRENT7(F,M2,[what,can,i,have,for,M,that,contains,I]):-
        M2\=M.
newQuestion7(Q,[]).         
newQuestion7([what,can,i,have,for,M,that,contains,I],[H|T]):-
   [what,can,i,have,for,M,that,contains,I]=H,
    isDIFFRENT7(F,M,H),
    newQuestion7([what,can,i,have,for,M,that,contains,I],T).

newQuestion7([what,can,i,have,for,M,that,contains,I],[H|T]):-
    [what,can,i,have,for,M,that,contains,I]\=H,
    newQuestion7([what,can,i,have,for,M,that,contains,I],T).


response(Q,PQ,PR,["I", do, not, know]):-
  Q=[what,can,i,have,for,M,that,contains,I],
  \+prop(I,_,_),
  \+prop(I,_,_,_),
  newQuestion7(Q,PQ).

response([what,can,i,have,for,M,that,contains,I],PQ,PR,["I", told, you, that, before]):-
  \+response(Q,PQ,PR,["I", do, not, know]),
  \+response(Q,PQ,PR,["Nothing", from, what, i, know]),
  \+response(Q,PQ,PR,[R]).



  
response0([],PQ,PR,[]).
response0([H-Occ1|T],PQ,PR,[H-Occ2|T1]):-
  calcCalories(H,PQ,PR,C),
  C>=0,
  Occ2 is Occ1+1,
  response0(T,PQ,PR,T1).
response0([H-Occ1|T],PQ,PR,[H-Occ1|T1]):-
  calcCalories(H,PQ,PR,C),
  C<0,
  response0(T,PQ,PR,T1).


		