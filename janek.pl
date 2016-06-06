ensure_loaded(library(lists)).
l :- [zad].

% Założenia:
    %% - N - liczba procesów działających w systemie >= 1
    %% - wszystkie zmienne są inicjowane na 0 (w tym tablice)
    %% - wszystkie zmienne przyjmują wartości 0..N
    %% - tablice są jednowymiarowe, rozmiaru N
% Można: member, append
% Niemożna: findall, bagof, setof

%% evalExp(+Exp, +State, -Result).


/* kolejka ********************************************/

% init(Kolejka) - inicjacja kolejki (na pustą)
init(P-P).

% get(Elem, Kolejka, NowaKolejka) - pobranie
get(Elem, [Elem|P]-K, P-K).

% put(Elem, Kolejka, NowaKolejka) - wstawienie
%put(Elem, P-K, P-K1) :- K=[Elem|K1].
put(Elem, P-[Elem|K1], P-K1).

% empty(Kolejka) - czy kolejka pusta
empty(P-K) :- var(P).

% init(Q), put(5,Q,Q1), put(6,Q1,Q2), get(X,Q2,Q3), write(X), put(7,Q3,Q4), put(8,Q4,Q5), get(Y,Q5,Q6), write(Y), get(Z,Q6,Q7), write(Z). 


%% Arguments: OldValue, NewValue, List, NewList
replaceByValue(_, _, [], []).
replaceByValue(O, R, [O|T], [R|T2]) :- replaceByValue(O, R, T, T2).
replaceByValue(O, R, [H|T], [H|T2]) :- 
    H \= O, 
    replaceByValue(O, R, T, T2).

%% Arguments: Index, NewValue, List, NewList
%% Elements indexed from 0
replaceByIndex(0, X, [_|T], [X|T]).
replaceByIndex(I, X, [H|T], [H|R]):- 
    I > -1, NI is I-1, 
    replaceByIndex(NI, X, T, R), !.



count(_, [], 0).
count(X, [X | T], N) :- !, count(X, T, N1), N is N1 + 1.
count(X, [_ | T], N) :- count(X, T, N).


program(
    [assign(x, 1),
    assign(x, 2),
    goto(1)]).


%% verify(3, program(prog)) :-
%%  step(prog, [[],[]], ).

%% initState(Program, InitState) :-

check(X) :- 
    program(Program),
    (dfs(Program, [[x-0],[0, 0, 0]], []) -> 
        write("Critical Section!"), nl
        ;
        write("No Critical")).
    %% insection(Program, [1,2,0,2, 3, 3], X).

insection(Program, Count, Result) :-
    nth0(SectionNumber, Program, sekcja),
    count(SectionNumber, Count, Result), !.

step(Program, [Vars, Count], PrId, StateOut) :-
    nth0(PrId, Count, Step),
    nth0(Step, Program, Instruction),
    evalInstr(Instruction, [Vars, Count], PrId, StateOut).

dfs(Program, [Vars, Count], States) :-
    insection(Program, Count, InSect),
    %% write("In Section: "),
    %% write(InSect), nl,
    %% write(Count), nl,
    InSect > 1,
    write("Critical Section!"), nl,
    write(States), nl, !.

dfs(Program, StateIn, States) :-
    step(Program, StateIn, PrId, StateOut),
    %% write("DFS PrId "),
    %% write(PrId),
    %% write(" States "),
    %% write(States), nl,
    %% PrId < 3,
    not(member(StateOut, States)), !,
    dfs(Program, StateOut, [StateOut | States]).

%% evalInstr(assign(arr(Ident, ExpArythm), Exp), StateIn, PrId, StateOut) :-
    %% evalArythm(Exp, StateIn, New).
    
nextStep(PrId, Count, News) :-
    nth0(PrId, Count, Old),
    New is Old + 1,
    replaceByIndex(PrId, New, Count, News), !.
    %% dunno why for PrId == 0 it return two answers so I add ! to cut this stuff

evalInstr(sekcja, [Vars, Count], PrId, [Vars, News]) :-
    %% write("PrId: "), write(PrId), nl,
    %% write("sekcja"), nl,
    nextStep(PrId, Count, News), !.

evalInstr(goto(Number), [Vars, Count], PrId, [Vars, News]) :-
    %% write("PrId: "), write(PrId), nl,
    %% write("goto("), write(Number), write(")"), nl,
    New is Number - 1,
    replaceByIndex(PrId, New, Count, News), !.

evalInstr(assign(Ident, Exp), [Vars, Count], PrId, [NewVar, NewCount]) :-
    %% write("PrId: "), write(PrId), nl,
    %% write("assign("), write(Ident), write(","), write(Exp), write(")"), nl,
    evalArythm(Exp, Vars, New),
    nth0(Index, Vars, Ident-Dummy),
    replaceByValue(Ident-Dummy, Ident-New, Vars, NewVar),
    nextStep(PrId, Count, NewCount), !.


    
evalInstr(condGoto(Exp, Number), [Vars, Count], PrId, [Vars, News]) :-
    evalLogic(Exp, Vars, Result),
    (Result > 0 ->
        replaceByIndex(PrId, Number, Count, News);
        nextStep(PrId, Count, News)).


evalLogic(S1 < S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is R2 - R1, !.

evalLogic(S1 > S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is >(R1, R2), !.

evalLogic(S1 = S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is ==(R1, R2), !.

%%  CHANGE TO <> 
evalLogic(S1 - S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is \=(R1, R2), !.


evalArythm(S1 + S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is R1 + R2, !.
    
evalArythm(S1 - S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is R1 - R2, !.

evalArythm(S1 * S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is R1 * R2, !.

evalArythm(S1 / S2, State, Result) :-
    evalSimple(S1, State, R1),
    evalSimple(S2, State, R2),
    Result is R1 div R2, !.

evalArythm(Number, _, Number).

%% evalArythm(S, State, Result) :- evalSimple(S, State, Result).

evalSimple(arr(Ident, ExpArythm), State, Result) :- 
    evalArythm(ExpArythm, State, Index),
    member(Ident-Array, State),
    nth0(Index, Array, Result), !.

evalSimple(Ident, State, Result) :-
    member(Ident-Result, State), !.

evalSimple(Number, _, Number).

    



