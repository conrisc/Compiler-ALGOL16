%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	                    %%
%%  Konrad Cielecki         %%
%%	Pracownia 3	    %%
%%	                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Implementacja podstawowej wersji jezyka Algol16 (nie zawierajaca
%	procedur)
%
%	IF <wyrazenie logiczne> then <instrukcja zlozona> FI
%            nie jest zaimplementowane, zamiast tego nalezy uzyc
%	IF <wyrazenie logiczne> then <instrukcja zlozona> ELSE FI
%
%

% Tokeny
token(60,62,'<>').
token(58,61,':=').
token(62,61,'>=').
token(60,61,'<=').
token(32,' ').
token(40,'(').
token(41,')').
token(44,',').
token(59,';').
token(43,'+').
token(45,'-').
token(42,'*').
token(60,'<').
token(62,'>').
token(61,'=').
token(58,':').

% Operatory arytmetyczne (OPERATOR,PRIORYTET)
operator('+',1).
operator('-',2).
operator('*',3).
operator(div,3).
operator(mod,3).
operator('!',4).

% Operatory wyrazen logicznych
oprLog(or,1).
oprLog(and,2).
oprLog(not,3).
oprLog('<',4).
oprLog('<=',4).
oprLog('>',4).
oprLog('>=',4).
oprLog('=',4).
oprLog('<>',4).

%%%%%%%%%%%%PROGRAM%%%%%%%%%%%%
algol16(Source,Result):-
	normalize(Source,R1),
	tokens(R1,R2),
	program(R2,Result).

%%%%%%%%%%%LEKSER%%%%%%%%%%%%%%%
% Zamiana tabulatorow i znakow nowej lini na spacje(przydatne przy
% rozdzielaniu na tokeny)
normalize([],[]).
normalize([H|Source],R):-
	(H=9;
	H=10),!,
	normalize([32|Source],R).
normalize([H|Source],[H|R]):-
	normalize(Source,R).

% Ciecie listy znakow na tokeny
tokens([],[]):-!.
tokens([32|Source],Tokens):-!,
	tokens(Source,Tokens).
tokens([H1,H2|Source],[TokenString|Tokens]):-
	token(H1,H2,TokenString),!,
	tokens(Source,Tokens).
tokens([H|Source],[TokenString|Tokens]):-
	token(H,TokenString),!,
	tokens(Source,Tokens).
tokens(Source,[TokenString|Tokens]):-

	oneToken(Source,Token,RestS),
	atom_codes(TokenString,Token),
	tokens(RestS,Tokens).

% Wyciaganie pojedynczego tokena z listy
oneToken([],[],[]).
oneToken([H|Tail],[],[H|Tail]):-
	token(H,_),!.
oneToken([H|Tail],[H|Token],Rest):-
	oneToken(Tail,Token,Rest).

%%%%%%%%%%GRAMATYKA%%%%%%%%%%%%%%%%%

program([program,NAZWA|Tail],Result):-
	nl,write('Nowy program: '),write(NAZWA),nl,
	NAZWA=_,
	deklaracje(Tail,[begin|R1],[[],Zmienne]),                        % Wczytywanie zmiennych
	Zmienne2 = [['@Stos',_]|Zmienne],                                % Tworzenie adresu stosu
	insZlozona(R1,[],end,Zmienne2,[[[_,0,0,0,0]],SextiumRev]),	 % Kompilowanie programu
	length([_,_|SextiumRev],N),					 % Sprawdzanie ilosci rozkazow
	norm([[_,0,0,0,0],[_,9,1,0,0]|SextiumRev],[[],Sextium],N),       % Dodanie rozkazu stop + normalizacja
	reverse(Zmienne2,Zmienne3),                                      % Odwrocenie listy zmiennych
	zaadresujZmienne(Zmienne3,N),					 % Adresowanie zmiennych
	flatt(Sextium,Result),						 % Zamiana list na rozkazy w HEX
	write('Koniec'),nl,nl.

% Odwroc liste rozkazow, zaadresuj wiersze i usun adresy z wyniku
norm([],[Result,Result],_).
norm([[ADR|OneLine]|List],[Acc,Result],N):-
	NN is N-1,
	formatuj(NN,ADR),
	NewAcc=[OneLine|Acc],
	norm(List,[NewAcc,Result],NN).

% Wyznacza adresy zmiennych
zaadresujZmienne([],_).
zaadresujZmienne([[_,Hex]|Zmienne],N):-
	formatuj(N,Hex),
	NN is N + 1,
	zaadresujZmienne(Zmienne,NN).

% Formatowanie DECIMAL to HEXADECIMAL
% Uzywane przy ONP i Adresach zmiennych - dlatego nie ma ujemnych
formatuj(N,Hex):-
	N>= 16^3,!,
	format(atom(Hex),'~16r',[N]).
formatuj(N,Hex):-
	N>= 16^2,!,
	format(atom(Hex),'0~16r',[N]).
formatuj(N,Hex):-
	N>= 16,!,
	format(atom(Hex),'00~16r',[N]).
formatuj(N,Hex):-
	format(atom(Hex),'000~16r',[N]).

flatt([],[]).
flatt([H|Tail],[X|Result]):-
	atomic_list_concat(H,'',X),
	flatt(Tail,Result).

deklaracje([begin|Tail],[begin|Tail],[Zmienne,Zmienne]):- !.
deklaracje(Source,Result,[Acc,Zmienne]):-
	deklarator(Source,R1,[Acc,Zmienne2]),
	deklaracje(R1,Result,[Zmienne2,Zmienne]).

deklarator([local|Tail],Result,[Acc,Zmienne]):-
	write('Deklaruje zmienne: '),
	zmienne(Tail,Result,[Acc,Zmienne]).

zmienne([IDENTYFIKATOR,KeyWord|Tail],[KeyWord|Tail],[Acc,[[IDENTYFIKATOR,_]|Acc]]):-
	(KeyWord=begin;
	KeyWord=local),!,
	write(IDENTYFIKATOR),nl.
zmienne([IDENTYFIKATOR,','|Tail],Result,[Acc,Zmienne]):-
	write(IDENTYFIKATOR),write(' '),
	zmienne(Tail,Result,[[[IDENTYFIKATOR,_]|Acc],Zmienne]).

% MAIN MAIN MAIN MAIN MAIN

insZlozona([ZnakKonca|Tail],Tail,ZnakKonca,_,[Sextium,Sextium]):-!.
insZlozona([';'|Tail],Result,ZnakKonca,Zmienne,[Acc,Sextium]):-!,
	insZlozona(Tail,Result,ZnakKonca,Zmienne,[Acc,Sextium]).

insZlozona([if|Tail],Result,ZnakKonca,Zmienne,[Acc,Sextium]):-!,
	insIF(Tail,R1,Zmienne,Instrukcje),
	append(Instrukcje,Acc,NewAcc),
	insZlozona(R1,Result,ZnakKonca,Zmienne,[NewAcc,Sextium]).

insZlozona([while|Tail],Result,ZnakKonca,Zmienne,[Acc,Sextium]):-!,
	insWh(Tail,R1,Zmienne,Instrukcje),
	append(Instrukcje,Acc,NewAcc),
	insZlozona(R1,Result,ZnakKonca,Zmienne,[NewAcc,Sextium]).

insZlozona([read|Tail],Result,ZnakKonca,Zmienne,[Acc,Sextium]):-!,
	insRead(Tail,R1,Zmienne,Instrukcje),
	putAll(Instrukcje,Acc,NewAcc),
	insZlozona(R1,Result,ZnakKonca,Zmienne,[NewAcc,Sextium]).

insZlozona([write|Tail],Result,ZnakKonca,Zmienne,[Acc,Sextium]):-!,
	toONP(Tail,R1,ZnakKonca,[],[],ONP),
	onpToSex(ONP,Zmienne,[[],Instrukcje]),
	append(Instrukcje,Acc,NewAcc),
	find('@Stos',Zmienne,MEM),
	NewAcc2 =[[_,0,0,0,2],[_,2,5,9,1],[_,2,b,3,4],[_,MEM],[_,0,0,0,1],[_,9,5,9,4]|NewAcc],
	write('write '),write(ONP),nl,
	insZlozona(R1,Result,ZnakKonca,Zmienne,[NewAcc2,Sextium]).

insZlozona([ID,':='|Tail],Result,ZnakKonca,Zmienne,[Acc,Sextium]):-
	find(ID,Zmienne,MEM1),
	find('@Stos',Zmienne,MEM2),
	toONP(Tail,R1,ZnakKonca,[],[],ONP),
	onpToSex(ONP,Zmienne,[[],Instrukcje]),
	append(Instrukcje,Acc,NewAcc),
	NewAcc2=[[_,3,0,0,0],[_,MEM1],[_,5,9,4,5],[_,2,b,4,2],[_,MEM2],[_,0,0,0,1],[_,9,5,9,4]|NewAcc],
	write(ID),write(' := '),write(ONP),nl,
	insZlozona(R1,Result,ZnakKonca,Zmienne,[NewAcc2,Sextium]).

insIF(Source,Result,Zmienne,Instrukcje):-
	write('IF '),
	scal(Source,R1,then,[],0,[],WyrLog),
	ltoONP(WyrLog,[],[],LONP),
	warunek(LONP,Zmienne,[],Warunek),
	write(LONP),write(' then'),nl,
	find('@Stos',Zmienne,MEM),
	Acc = [[_,5,6,0,0],[_,FALSE],[_,2,5,9,4],[_,2,b,3,4],[_,MEM],[_,0,0,0,1],[_,9,5,9,4]|Warunek],
	insZlozona(R1,R2,else,Zmienne,[[],InZl1]),
	append([[FALSE,0,0,0,0],[_,TRUE],[_,9,8,0,0]|InZl1],Acc,Acc2),
	write('else'),nl,
	insZlozona(R2,Result,fi,Zmienne,[[],InZl2]),
	append([[TRUE,0,0,0,0]|InZl2],Acc2,Instrukcje),
	write('fi'),nl.

insWh(Source,Result,Zmienne,Instrukcje):-
	write('While '),
	scal(Source,R1,do,[],0,[],WyrLog),
	ltoONP(WyrLog,[],[],LONP),
	Loop = [[LABEL2,0,0,0,0]],
	warunek(LONP,Zmienne,[],InLog),
	append(InLog,Loop,Warunek),
	write(LONP),write(' then'),nl,
	find('@Stos',Zmienne,MEM),
	Acc = [[_,5,6,0,0],[_,LABEL],[_,2,5,9,4],[_,2,b,3,4],[_,MEM],[_,0,0,0,1],[_,9,5,9,4]|Warunek],
	insZlozona(R1,Result,done,Zmienne,[[],InZl]),
	append([[LABEL,0,0,0,0],[_,LABEL2],[_,9,8,0,0]|InZl],Acc,Instrukcje),
	write('done'),nl.

insRead([ID|Tail],Tail,Zmienne,Instrukcje):-
	write('read '),write(ID),nl,
	find(ID,Zmienne,MEM),
	Instrukcje=[[_,9,4,9,1],[_,MEM],[_,0,0,0,1],[_,3,0,0,0]].

find(ID,[[ID,MEM]|_],MEM):-!.
find(ID,[_|Zmienne],MEM):-
	find(ID,Zmienne,MEM).

putAll([],Result,Result).
putAll([H|Tail],Sufix,Result):-
	putAll(Tail,[H|Sufix],Result).

%%%%%%%%%%%%% WYRAZENIA LOGICZNE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

warunek([],_,InLog,InLog):-!.
warunek([or|LONP],Zmienne,Acc,InLog):-!,
	find('@Stos',Zmienne,Stos),
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,0],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,FALSE],[_,0,0,0,1],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,6,9,5,9],[_,TRUE],[_,4,9,4,a]|Sciagnij],
	append(Instrukcje,Acc,NewAcc),
	warunek(LONP,Zmienne,NewAcc,InLog).


warunek([and|LONP],Zmienne,Acc,InLog):-!,
	find('@Stos',Zmienne,Stos),
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,1],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,FALSE],[_,0,0,0,0],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,6,9,5,9],[_,TRUE],[_,4,9,4,b]|Sciagnij],
	append(Instrukcje,Acc,NewAcc),
	warunek(LONP,Zmienne,NewAcc,InLog).

warunek([not|LONP],Zmienne,Acc,InLog):-!,
	find('@Stos',Zmienne,Stos),
	NewAcc = [[TRUE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,1],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[FALSE,9,5,9,4],[_,TRUE],[_,0,0,0,0],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,6,9,5,9],[_,FALSE],[_,5,9,4,5],[_,2,b,4,2],[_,Stos],[_,0,0,0,1],[_,9,5,9,4]|Acc],
	warunek(LONP,Zmienne,NewAcc,InLog).
warunek([Left,Right,OP|LONP],Zmienne,Acc,InLog):-
	oprLog(OP,_),
	onpToSex(Left,Zmienne,[[],ObliczL]),
	onpToSex(Right,Zmienne,[[],ObliczR]),
	find('@Stos',Zmienne,MEM),
	porownaj(OP,MEM,Porownaj),
	append(ObliczL,Acc,Acc2),
	append(ObliczR,Acc2,Acc3),
	append(Porownaj,Acc3,NewAcc),
	warunek(LONP,Zmienne,NewAcc,InLog).


porownaj('<',Stos,Instrukcje):-!,
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,1],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,FALSE],[_,0,0,0,0],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,7,9,5,9],[_,TRUE],[_,4,9,4,b]|Sciagnij].

porownaj('>',Stos,Instrukcje):-!,
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,1],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,8,0,0,0],[_,FALSE],[_,0,0,0,0],[_,4,9,3,9],[_,Stos],[_,9,4,2,b],[_,0,0,0,1],[_,b,7,9,5],[_,TRUE],[5,4,9,4]|Sciagnij].

porownaj('<=',Stos,Instrukcje):-!,
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,0],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,8,0,0,0],[_,FALSE],[_,0,0,0,1],[_,4,9,3,9],[_,Stos],[_,9,4,2,b],[_,0,0,0,1],[_,b,7,9,5],[_,TRUE],[5,4,9,4]|Sciagnij].

porownaj('>=',Stos,Instrukcje):-!,
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,0],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,FALSE],[_,0,0,0,1],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,7,9,5,9],[_,TRUE],[_,4,9,4,b]|Sciagnij].

porownaj('=',Stos,Instrukcje):-!,
	 dwieZeStosu(Stos,Sciagnij),
	 Instrukcje = [[FALSE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,1],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[TRUE,9,5,9,4],[_,FALSE],[_,0,0,0,0],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,6,9,5,9],[_,TRUE],[_,4,9,4,b]|Sciagnij].

porownaj('<>',Stos,Instrukcje):-
	dwieZeStosu(Stos,Sciagnij),
	Instrukcje = [[TRUE,0,0,0,0],[_,3,0,0,0],[_,0,0,0,0],[_,2,b,4,9],[_,Stos],[_,0,0,0,1],[FAL,9,5,9,4],[_,TRUE],[_,0,0,0,1],[_,9,3,9,8],[_,4,2,b,4],[_,Stos],[_,0,0,0,1],[_,6,9,5,9],[_,FAL],[_,4,9,4,b]|Sciagnij].

dwieZeStosu(Stos,Instrukcje):-
	Instrukcje = [[_,5,2,0,0],[_,2,4,b,4],[_,2,b,3,4],[_,Stos],[_,0,0,0,1],[_,9,5,9,4]].


% Funkcja przetwarzajaca wyrazenie ONP na odpowiednie instrukcje procesora

onpToSex([],_,[Instrukcje,Instrukcje]):-!.

onpToSex([mod|ONP],Zmienne,[Acc,Instrukcje]):-!,
	find('@Stos',Zmienne,MEM),
	NewAcc=[[_,3,0,0,0],[_,MEM],[_,5,9,4,a],[_,0,0,0,1],[_,2,b,3,9],[_,2,d,c,5],[_,4,b,4,5],[_,2,b,4,2],[_,MEM],[_,0,0,0,1],[_,9,5,9,4]|Acc],
	onpToSex(ONP,Zmienne,[NewAcc,Instrukcje]).

onpToSex([H|ONP],Zmienne,[Acc,Instrukcje]):-
	operator(H,_),!,
	find('@Stos',Zmienne,MEM),
	(H='+',OPERACJA=a;
	 H='-',OPERACJA=b;
	 H='!',OPERACJA=b;
	 H='*',OPERACJA=c;
	 H=div,OPERACJA=d),!,
	NewAcc=[[_,3,0,0,0],[_,MEM],[_,5,9,4,a],[_,0,0,0,1],[_,2,OPERACJA,3,9],[_,4,b,4,5],[_,2,b,4,2],[_,MEM],[_,0,0,0,1],[_,9,5,9,4]|Acc],
	onpToSex(ONP,Zmienne,[NewAcc,Instrukcje]).

onpToSex([H|ONP],Zmienne,[Acc,Instrukcje]):-
	find(H,Zmienne,MEM1),!,
	find('@Stos',Zmienne,MEM2),
	NewAcc = [[_,a,3,0,0],[_,MEM2],[_,5,9,4,2],[_,0,0,0,1],[_,5,4,3,9],[_,MEM1],[_,9,4,2,4],[_,MEM2],[_,9,4,2,5]|Acc],
	onpToSex(ONP,Zmienne,[NewAcc,Instrukcje]).

onpToSex([H|ONP],Zmienne,[Acc,Instrukcje]):-
	atom_number(H,N),
	find('@Stos',Zmienne,MEM),
	formatuj(N,HEX),
	NewAcc = [[_,3,0,0,0],[_,MEM],[_,9,4,2,a],[_,0,0,0,1],[_,HEX],[_,9,3,9,5],[_,MEM],[_,9,4,2,4]|Acc],
	onpToSex(ONP,Zmienne,[NewAcc,Instrukcje]).


% toONP - oczekuje zmiennej / negacji / poczatku nawiasu
%%	Zawsze rozpoczyna wyrazenie arytmetyczne
% toONP2 - oczekuje operatora / konca nawiasu
%%	Zawsze konczy wyrazenie arytmetyczne


% toONP(WE,Rest,ZnakKonca,Stos,ACC,ONP).

% Odwracanie listy i zwracanie listy ONP
toONP2([';'|Tail],Tail,_,[],Result,ONP):-!,
	reverse(Result,ONP).

toONP2([ZnakKonca|Tail],[ZnakKonca|Tail],ZnakKonca,[],Result,ONP):-!,
	reverse(Result,ONP).

% Wypisywanie reszty stosu
toONP2([';'|Tail],Rest,_,[H|Stos],Result,ONP):-!,
	toONP2([';'|Tail],Rest,_,Stos,[H|Result],ONP).

toONP2([ZnakKonca|Tail],Rest,ZnakKonca,[H|Stos],Result,ONP):-!,
	toONP2([ZnakKonca|Tail],Rest,ZnakKonca,Stos,[H|Result],ONP).

% IF WE=')' & H:(STOS) = '('
toONP2([')'|Tail],Rest,ZnakKonca,['('|Stos],Result,ONP):-!,
	toONP2(Tail,Rest,ZnakKonca,Stos,Result,ONP).

% IF WE=')' & H:(STOS) = OPERATOR then wypisz operator
toONP2([')'|Tail],Rest,ZnakKonca,[H|Stos],Result,ONP):-!,
	operator(H,_),
	toONP2([')'|Tail],Rest,ZnakKonca,Stos,[H|Result],ONP).

% IF WE=OPERATOR & STOS=[] then STOS = [OPERATOR]
toONP2([OPERATOR|Tail],Rest,ZnakKonca,[],Result,ONP):-
	operator(OPERATOR,_),!,
	toONP(Tail,Rest,ZnakKonca,[OPERATOR],Result,ONP).

% IF H:(STOS) = '(' then Umiesc operator na stosie
toONP2([OPERATOR|Tail],Rest,ZnakKonca,['('|Stos],Result,ONP):-
	operator(OPERATOR,_),!,
	toONP(Tail,Rest,ZnakKonca,[OPERATOR,'('|Stos],Result,ONP).

% Jezeli na stosie sa operatory o wiekszym/rownym priorytecie to je wypisz.
toONP2([OPERATOR|Tail],Rest,ZnakKonca,[H|Stos],Result,ONP):-
	operator(OPERATOR,PR1),
	operator(H,PR2),
	PR1=<PR2,!,
	toONP2([OPERATOR|Tail],Rest,ZnakKonca,Stos,[H|Result],ONP).

% Jezeli na stosie sa operatory o mniejszym priorytecie, umiesc operator na stosie
toONP2([OPERATOR|Tail],Rest,ZnakKonca,[H|Stos],Result,ONP):-
	operator(OPERATOR,_),
	operator(H,_),
	toONP(Tail,Rest,ZnakKonca,[OPERATOR,H|Stos],Result,ONP).

% Zmienna zanegowana
toONP(['-'|Tail],Rest,ZnakKonca,Stos,Result,ONP):-!,
	toONP(Tail,Rest,ZnakKonca,['!'|Stos],['0'|Result],ONP).

% Poczatek nawiasu
toONP(['('|Tail],Rest,ZnakKonca,Stos,Result,ONP):-!,
	toONP(Tail,Rest,ZnakKonca,['('|Stos],Result,ONP).

% Zmienna
toONP([ZMIENNA|Tail],Rest,ZnakKonca,Stos,Result,ONP):-
	ZMIENNA = _,
	toONP2(Tail,Rest,ZnakKonca,Stos,[ZMIENNA|Result],ONP).

% scal(WE,Rest,ZnakKonca,Stos,Number,ACC,Result).

% Zwracanie scalonego wyrazenia logicznego
scal([ZnakKonca|Tail],Tail,ZnakKonca,[],0,Acc,Result):-!,
	reverse(Acc,Result).

% Gdy dojdzie do konca, ale stos nie jest pusty
% Tylko dla Number<=0, bo dla Number>0 wyr jest niepoprawne
scal([ZnakKonca|Tail],Rest,ZnakKonca,Stos,Number,Acc,Result):-!,
	Number=<0,
	cut(Stos,Number,Nawiasy,Stos2),
	reverse([';'|Stos2],WyrAr),
	toONP(WyrAr,_,';',[],[],ONP),
	append(Nawiasy,[ONP|Acc],NewAcc),
	scal([ZnakKonca|Tail],Rest,ZnakKonca,[],0,NewAcc,Result).

% WE = not
scal([not|Tail],Rest,ZnakKonca,Stos,_,Acc,Result):-!,
	append(Stos,Acc,NewAcc),
	scal(Tail,Rest,ZnakKonca,[],0,[not|NewAcc],Result).

% WE = OprLog, zloz WyrAr i zapisz do akumulatora
scal([H|Tail],Rest,ZnakKonca,Stos,Number,Acc,Result):-
	oprLog(H,_),
	Number>=0,!,
	reverse([';'|Stos],Stos2),
	cut(Stos2,Number,Nawiasy,WyrAr),
	toONP(WyrAr,_,';',[],[],ONP),
	append([ONP|Nawiasy],Acc,NewAcc),
	scal(Tail,Rest,ZnakKonca,[],0,[H|NewAcc],Result).

% WE = OprLog, zloz WyrAr i zapisz do akumulatora
scal([H|Tail],Rest,ZnakKonca,Stos,Number,Acc,Result):-
	oprLog(H,_),!,
	Number<0,
	cut(Stos,Number,Nawiasy,Stos2),
	reverse([';'|Stos2],WyrAr),
	toONP(WyrAr,_,';',[],[],ONP),
	append(Nawiasy,[ONP|Acc],NewAcc),
	scal(Tail,Rest,ZnakKonca,[],0,[H|NewAcc],Result).

% WE!=OprLog, wloz H na stos
scal([H|Tail],Rest,ZnakKonca,Stos,Number,Acc,Result):-
	(H='(',!,NNumber is Number + 1;
	 H=')',!,NNumber is Number - 1;
	 H=_,NNumber is Number),
	scal(Tail,Rest,ZnakKonca,[H|Stos],NNumber,Acc,Result).

% Wyciaga N pierwszych elementow listy
cut(Suffix,0,[],Suffix):-!.
cut([H|Tail],N,[H|Prefix],Suffix):-
	(N>0,!,
	 NN is N-1;
	 N<0,
	 NN is N+1),
	cut(Tail,NN,Prefix,Suffix).

% ltoONP - oczekuje <WyrAry><OpRel><WyrAry> / negacji / poczatku nawiasu
%%	Zawsze rozpoczyna wyrazenie logiczne
% ltoONP2 - oczekuje operatora logicznego / konca nawiasu
%%	Zawsze konczy wyrazenie logiczne

% ltoONP(WE,Stos,ACC,ONP).

ltoONP2([],[],Acc,ONP):-!,
	reverse(Acc,ONP).

ltoONP2([')'|Tail],['('|Stos],Acc,ONP):-!,
	ltoONP2(Tail,Stos,Acc,ONP).

ltoONP2([')'|Tail],[H|Stos],Acc,ONP):-!,
	oprLog(H,_),
	ltoONP2([')'|Tail],Stos,[H|Acc],ONP).

ltoONP2([OPR|Tail],Stos,Acc,ONP):-
	oprLog(OPR,_),
	(Stos=[];
	 Stos=['('|_]),!,
	ltoONP(Tail,[OPR|Stos],Acc,ONP).

ltoONP2([OPR1|Tail],[OPR2|Stos],Acc,ONP):-
	oprLog(OPR1,PR1),
	oprLog(OPR2,PR2),
	PR1=<PR2,!,
	ltoONP2([OPR1|Tail],Stos,[OPR2|Acc],ONP).

ltoONP2([OPR1|Tail],[OPR2|Stos],Acc,ONP):-
	oprLog(OPR1,_),
	oprLog(OPR2,_),
	ltoONP(Tail,[OPR1|Stos],Acc,ONP).

ltoONP2([],[H|Stos],Acc,ONP):-
	ltoONP2([],Stos,[H|Acc],ONP).

ltoONP([not|Tail],Stos,Acc,ONP):-!,
	ltoONP(Tail,[not|Stos],Acc,ONP).

ltoONP(['('|Tail],Stos,Acc,ONP):-!,
	ltoONP(Tail,['('|Stos],Acc,ONP).

ltoONP([WL,OPR,WR|Tail],Stos,Acc,ONP):-
	oprLog(OPR,4),!,
	WL=[_|_],
	WR=[_|_],
	ltoONP2(Tail,Stos,[OPR,WR,WL|Acc],ONP).

