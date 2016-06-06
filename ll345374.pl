% Logvinov
:- ensure_loaded(library(lists)).
%% :- set_prolog_flag(single_var_warnings, off).
:- op(700, xfx, <->).
:- op(700, xfx, <>).

% state(Vars, Arrays, Positions)
% program(Vars, Arrays, Code)

initVars([], []).
initVars([Var | Tail], [Var <-> 0 | StanZmiennych]) :- initVars(Tail, StanZmiennych).

zeroArray(0, []).
zeroArray(N, Array) :-
    N > 0,
    Len is N - 1,
    zeroArray(Len, Res),
    Array = [0 | Res].

initArrays([], _, []).
initArrays([Array | Tail], N, [Array <-> ZeroArray | StanTablic]) :-
    initArrays(Tail, N, StanTablic),
    zeroArray(N, ZeroArray).

initState(Program, N, StanPoczatkowy) :-
    program(vars(Vars), arrays(Arrays), _) = Program,
    initVars(Vars, InitVars),
    initArrays(Arrays, N, InitArrays),
    zeroArray(N, Positions),
    StanPoczatkowy = state(InitVars, InitArrays, Positions).

aOp(+, +).
aOp(-, -).
aOp(*, *).
aOp(/, div).

evalExpr(Expr, _, _, Expr) :- integer(Expr).
evalExpr(pid, PID, _, PID).
evalExpr(Ident, _, State, Val) :-
    atom(Ident),
    State =.. [_, Vars, _, _],
    member(Ident <-> Val, Vars).
evalExpr(arr(Ident, Expr), PID, State, Val) :-
    atom(Ident),
    State =.. [_, _, Arrays, _],
    member(Ident <-> Values, Arrays),
    evalExpr(Expr, PID, State, Index),
    nth0(Index, Values, Val).
evalExpr(Expr, PID, State, Val) :-
    Expr =.. [Op, Lhs, Rhs],
    evalExpr(Lhs, PID, State, LVal),
    evalExpr(Rhs, PID, State, RVal),
    aOp(Op, AOp),
    AExpr =.. [AOp, LVal, RVal],
    Val is AExpr.

bOp(=,==).
bOp(<,<).
bOp(<>,\==).

evalBExpr(Expr, PID, State, Val) :-
    Expr =.. [Op, Lhs, Rhs],
    evalExpr(Lhs, PID, State, LVal),
    evalExpr(Rhs, PID, State, RVal),
    bOp(Op, BOp),
    BExpr =.. [BOp, LVal, RVal],
    (BExpr -> Val = true; Val = false).

replaceVal(_, _, [], []).
replaceVal(Old, New, [Old | Tail], [New | Tail]).
replaceVal(Old, New, [Head | Tail], [Head | NewTail]) :-
    Old \= Head, 
    replaceVal(Old, New, Tail, NewTail).

replaceIndex(0, New, [_ | Tail], [New | Tail]).
replaceIndex(Index, New, [Head | Tail], [Head | NewTail]) :-
    NewIndex is Index - 1,
    replaceIndex(NewIndex, New, Tail, NewTail).

stepForward(Positions, PID, NewPositions) :-
    nth0(PID, Positions, Pos),
    NewPos is Pos + 1,
    replaceIndex(PID, NewPos, Positions, NewPositions).

stepInto(Positions, PID, Num, NewPositions) :-
    replaceIndex(PID, Num, Positions, NewPositions).

stmt(assign(Ident, Expr), State, PID, NewState) :-
    atom(Ident),
    evalExpr(Expr, PID, State, Val),
    state(Vars, Arrays, Positions) = State,
    replaceVal(Ident <-> _, Ident <-> Val, Vars, NewVars),
    stepForward(Positions, PID, NewPositions),
    state(NewVars, Arrays, NewPositions) = NewState.

stmt(assign(arr(Ident, IndexExpr), Expr), State, PID, NewState) :-
    atom(Ident),
    evalExpr(Expr, PID, State, Val),
    evalExpr(IndexExpr, PID, State, Index),
    state(Vars, Arrays, Positions) = State,
    member(Ident <-> Array, Arrays),
    replaceIndex(Index, Val, Array, NewArray),
    replaceVal(Ident <-> Array, Ident <-> NewArray, Arrays, NewArrays),
    stepForward(Positions, PID, NewPositions),
    state(Vars, NewArrays, NewPositions) = NewState.

stmt(goto(Num), State, PID, NewState) :-
    state(Vars, Arrays, Positions) = State,
    Num0 is Num - 1,
    stepInto(Positions, PID, Num0, NewPositions),
    state(Vars, Arrays, NewPositions) = NewState.

stmt(condGoto(BExpr, Num), State, PID, NewState) :-
    state(Vars, Arrays, Positions) = State,
    evalBExpr(BExpr, PID, State, Val),
    Num0 is Num - 1,
    (Val -> stepInto(Positions, PID, Num0, NewPositions); stepForward(Positions, PID, NewPositions)),
    state(Vars, Arrays, NewPositions) = NewState.

stmt(sekcja, State, PID, NewState) :-
    state(Vars, Arrays, Positions) = State,
    stepForward(Positions, PID, NewPositions),
    state(Vars, Arrays, NewPositions) = NewState.

unsafePositions(_, [], _, []).
unsafePositions(Code, [Pos | Tail], Index, Unsafe) :-
    nth0(Pos, Code, Stmt),
    NewIndex is Index + 1,
    unsafePositions(Code, Tail, NewIndex, NewUnsafe),
    (Stmt == sekcja -> Unsafe = [Index | NewUnsafe]; Unsafe = NewUnsafe).

safe(Program, State, Val, Unsafe) :-
    state(_, _, Positions) = State,
    program(_, _, program(Code)) = Program,
    unsafePositions(Code, Positions, 0, Unsafe),
    length(Unsafe, Num),
    (Num < 2 -> Val = true; Val = false).

step(Program, StanWe, PID, StanWy) :-
    state(_, _, Positions) = StanWe,
    nth0(PID, Positions, Index),
    program(_, _, program(Code)) = Program,
    nth0(Index, Code, Stmt),
    stmt(Stmt, StanWe, PID, StanWy).

% Neighbours = [edge(ProcessNo, StmtNo, NewState)]
neighbours(_, 0, _, []).
neighbours(State, N, Program, Neighbours) :-
    N > 0,
    NewN is N - 1,
    neighbours(State, NewN, Program, NewNeighbours),
    step(Program, State, NewN, NewState),
    state(_, _, Positions) = State,
    nth0(NewN, Positions, StmtNo0),
    StmtNo is StmtNo0 + 1,
    Neighbours = [edge(NewN, StmtNo, NewState) | NewNeighbours].

dfsNeighbours([], _, _, Visited, StateNo, Visited, StateNo, safe).
dfsNeighbours([edge(_, _, Son) | Tail], N, Program, Visited, StateNo, NewVisited, NewStateNo, Result) :-
    member(Son, Visited),
    dfsNeighbours(Tail, N, Program, Visited, StateNo, NewVisited, NewStateNo, Result).
dfsNeighbours([edge(ProcessNo, StmtNo, Son) | Tail], N, Program, Visited, StateNo, NewVisited, NewStateNo, Result) :-
    \+ member(Son, Visited),
    dfs(Son, N, Program, Visited, StateNo, InnerVisited, InnerStateNo, InnerResult),
    (InnerResult = unsafe(UnsafeState, Path, UnsafeProcesses) ->
        (
            Result = unsafe(UnsafeState, [ProcessNo <-> StmtNo | Path], UnsafeProcesses)
        );
        (
            dfsNeighbours(Tail, N, Program, InnerVisited, InnerStateNo, NewVisited, NewStateNo, Result)
        )
    ).

% Result = safe | unsafe(StateNo, Path, UnsafeProcesses)
% You should guarantee, that \+ member(State, Visited)
dfs(State, N, Program, Visited, StateNo, NewVisited, NewStateNo, Result) :-
    safe(Program, State, Safe, Unsafe),
    (Safe -> 
        (
            InnerStateNo is StateNo + 1,
            append(Visited, [State], VisitedMe),
            neighbours(State, N, Program, Neighbours),
            dfsNeighbours(Neighbours, N, Program, VisitedMe, InnerStateNo, NewVisited, NewStateNo, InnerResult),
            (InnerResult = safe -> Result = InnerResult;
                (
                    unsafe(UnsafeState, Path, UnsafeProcesses) = InnerResult,
                    Result = unsafe(UnsafeState, Path, UnsafeProcesses)
                )
            )
        );
        Result = unsafe(StateNo, [], Unsafe)
    ).

writeList([]).
writeList([Head | []]) :- write(Head).
writeList([Head , Next | Tail]) :- write(Head), write(', '), writeList([Next | Tail]).

writePath([]).
writePath([ProcessNo <-> StateNo | Tail]) :-
    format('   Proces ~d: ~d~n', [ProcessNo, StateNo]),
    writePath(Tail).

safe_verify(N, Str) :-
    read(Str, Vars),
    read(Str, Arrays),
    read(Str, Code),
    close(Str),
    Program = program(Vars, Arrays, Code),
    initState(Program, N, InitialState),
    dfs(InitialState, N, Program, [], 1, _, _, Result),
    (Result = safe -> 
        write('Program jest poprawny (bezpieczny).'), nl;
        (
            unsafe(StateNo, Path, UnsafeProcesses) = Result,
            format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.~n', StateNo),
            write('Niepoprawny przeplot:'), nl,
            writePath(Path),
            write('Procesy w sekcji: '),writeList(UnsafeProcesses), nl
        )
    ).

verify(N, Program) :-
    (integer(N), N > 0) -> 
    (
        catch((open(Program, read, Str), safe_verify(N, Str)), _, 
            (write('Error: brak pliku o nazwie - '), write(Program), nl, true))
    );
    write('Error: parametr 0 powinien byc liczba > 0\n').
