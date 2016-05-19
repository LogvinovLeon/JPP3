:- ensure_loaded(library(lists)).

safe_verify(N, Str) :-
    read(Str, Vars),
    read(Str, Arrays),
    read(Str, Code),
    close(Str), 
    write([Vars, Arrays, Code, N]), nl.

verify(N, Program) :-
    (integer(N), N > 0) -> 
    (
        catch(open(Program, read, Str), _, 
            (write('Error: brak pliku o nazwie - '), write(Program), fail)),
        safe_verify(N, Str)
    );
    (write('Error: parametr 0 powinien byc liczba > 0\n'), fail).
main :- verify(1, 'unsafe.txt').