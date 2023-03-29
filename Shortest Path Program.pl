:- dynamic ostacolo/1.
:- dynamic trofeo/1.
:- dynamic edge/2.




inizia:-
    % pulisci tutti gli ostacoli e trofei creati in precedenza.
    retractall(edge((X,Y),(X1,Y1))),
    retractall(ostacolo((X,Y))),
    retractall(trofeo((X,Y))),
    format('\t\t\t\t\t\t\t\t\033[1mBenvenuto, nel programma di calcolo del percorso minimo.\033[0m'),
    nl,
    nl,
    write('\t\t\t\t\t\t\tQui puoi inserire le coordinate X ed Y degli ostacoli e le coordinate X ed Y del trofeo.
\t\t\t\t\t\t\tIl programma calcolerà il percorso minimo per raggiungere il trofeo dalla posizione (0,0).'), nl,
    nl,
    quanti_ostacoli.

quanti_ostacoli:-
    write("Quanti ostacoli vuoi inserire? "), nl,
    read(NOstacoli),
    ( integer(NOstacoli), NOstacoli >= 0 ->
        inserisci_ostacoli(NOstacoli)
    ;
        write('Inserire un numero intero positivo, riprova.'), nl,
        quanti_ostacoli
    ).


inserisci_ostacoli(0) :- inserisci_trofeo.
inserisci_ostacoli(N) :-
    N > 0,
    nl,
    write('Inserire le coordinate x ed y dell''ostacolo'),nl,
    format('\033[1mX:\033[0m'), % per scrivere x in grassetto
    read(X),
    format('\033[1mY:\033[0m'), % per scrivere x in grassetto
    read(Y),
    ( integer(X), integer(Y), X >= 0, Y >= 0 ->
        ( ostacolo((X,Y)) -> write("Attenzione! L'ostacolo esiste già! "),
        inserisci_ostacoli(N) ;
        assertz(ostacolo((X,Y))),
        nl,
        write('Ostacolo inserito in posizione ('), write(X), write(','), write(Y), write(').'), nl,
        N1 is N - 1,
        inserisci_ostacoli(N1)
        )
    ;
        write("Errore! Le coordinate devono essere un intero, maggiori o uguali a 0. "), nl,
        inserisci_ostacoli(N)
    ).



inserisci_trofeo :-

     nl,
    write('Inserire le coordinate x ed y del trofeo:'), nl,
    format('\033[1mX:\033[0m'),
    read(X),
    format('\033[1mY:\033[0m'),
    read(Y),
    (  ( (not(integer(X));not(integer(Y));X<0;Y<0) ->
        (write("Errore! Le coordinate devono essere un intero, maggiori o uguali a 0. "), nl, inserisci_trofeo)) ;
        (   ostacolo((X,Y)) ->  write('Non puoi inserire il trofeo nella stessa posizione di un ostacolo'),
            nl,
            inserisci_trofeo) ;
        assertz(trofeo((X,Y))),
        nl,
        write('Trofeo inserito in posizione ('), write(X), write(','), write(Y), write(').'), nl
    ),
    max_coordinate1(MaxX, MaxY,L).






% dobbiamo usare reverse altrimenti gli elementi della lista ci appaiono
% in ordine inverso

show_path(X,X,T,P) :-
    reverse([X|T],P),!.
show_path(X,Z,T,P) :-
    edge(X,Y), \+ostacolo(Y), not(member(X,T)), show_path(Y,Z,[X|T],P).

show_path(X,Y,P) :-
    findall(P, show_path(X, Y, [], P), Paths),
    ( Paths = []
    -> write('Non è stato possibile trovare un percorso tra (0,0) e ('),
       write(Y), write('), riprova.'),
      nl,
      inizia,
      nl;
    trova_min_path(Paths, MinPath, Length)
    ).


% una lista vuota ha lunghezza 0

path_length([], 0).
path_length([ _ | T], Length) :-
    path_length(T, Resto), Length is Resto + 1.


add_length([], []).
add_length([Path | Paths], [Length - Path | Res]) :-
    path_length(Path, Length),
    add_length(Paths, Res).


% Frase di output
frase_output(MinPath, Length, Paths) :-
   nl,
   num_elementi(Paths, Lunghezza),
   write('Sono stati trovati '), write(Lunghezza), write(' possibili percorsi: '), nl,
   scrivi(Paths), nl,
   write('Il percorso minimo è: '), write(MinPath),
   write(', di lunghezza: '), write(Length).


% caso base: una lista vuota ha 0 elementi
num_elementi([], 0).
num_elementi([_|T], N) :-
    num_elementi(T, N1), N is N1 + 1.


scrivi([]).
scrivi([H|T]) :-
    nl, write(H),  nl,  scrivi(T).






trova_min_path(Paths, MinPath, Length) :-
    add_length(Paths, PathsWithLength),
    min_lista(PathsWithLength, Length-MinPath),
    frase_output(MinPath, Length, Paths).




min_lista([Length1-Path1 | Tail], Min) :-
    min_lista(Tail, Length1-Path1, Min).

min_lista([], Min, Min).

min_lista([Length-Path | Tail], Length1-Path1, Min) :-
    ( Length < Length1 -> min_lista(Tail, Length-Path, Min);
    min_lista(Tail, Length1-Path1, Min)
     ).





max([X],X):-!.
max([X|T],X):- max(T,N),X>=N,!.
max([X|T],N):- max(T,N).



% perche deve essere lanciata questa solo all avvio ma ci servono
% massimox e y e altrimenti ongi volta che riparte parte inserisci archi

max_coordinate1(MaxX, MaxY,L) :-
    findall(X, ostacolo((X, _)), XOstacoli),
    findall(Y, ostacolo((_, Y)), YOstacoli),
    findall(X, trofeo((X, _)), XTesoro),
    findall(Y, trofeo((_, Y)), YTesoro),
    append(XTesoro, XOstacoli, ListaX),
    append(YTesoro, YOstacoli, ListaY),
    max(ListaX,MaxX),
    max(ListaY,MaxY),
    inserisci_archi(MaxX, MaxY, L).


max_coordinate(MaxX, MaxY) :-
    findall(X, ostacolo((X, _)), XOstacoli),
    findall(Y, ostacolo((_, Y)), YOstacoli),
    findall(X, trofeo((X, _)), XTesoro),
    findall(Y, trofeo((_, Y)), YTesoro),
    append(XTesoro, XOstacoli, ListaX),
    append(YTesoro, YOstacoli, ListaY),
    max(ListaX,MaxX),
    max(ListaY,MaxY).




limite(X, Y) :-
  max_coordinate(MaxX, MaxY),
  X =< MaxX,
  Y =< MaxY.



positivo(X,Y) :-
    X >= 0, Y >= 0.




inserisci_archi(MaxX, MaxY, L) :-
    forall(between(0, MaxX, X),
           forall(between(0, MaxY, Y),
                  (   catch(crea_edge(X, Y), _, true),
                     (X,Y) = (MaxX,MaxY),
                     go(L) ; true))).



go(L):-
    trofeo((X, Y)),
    Z = (X, Y),
    show_path((0,0), Z, L).

crea_edge(X, Y) :-

 X = Y, limite(X,Y) -> ( X1 is X+1, Y1 is Y,limite(X1,Y1), positivo(X1,Y1),assert(edge((X, Y), (X1, Y1)));
                         X2 is X, Y2 is Y+1, limite(X2,Y2), positivo(X2,Y2), assert(edge((X, Y), (X2, Y2)));
                         X3 is X-1, Y3 is Y, limite(X3,Y3), positivo(X3,Y3),  assert(edge((X, Y), (X3, Y3)));
                         X4 is X, Y4 is Y-1, limite(X4,Y4), positivo(X4,Y4),  assert(edge((X, Y), (X4, Y4))));

 X \= Y, limite(X,Y) -> (X1 is X+1, Y1 is Y, limite(X1,Y1), positivo(X1,Y1),  assert(edge((X, Y), (X1, Y1)));
                         X2 is X, Y2 is Y-1, limite(X2,Y2), positivo(X2,Y2), assert(edge((X, Y), (X2, Y2)));
                         X3 is X-1, Y3 is Y, limite(X3,Y3), positivo(X3,Y3),  assert(edge((X, Y), (X3, Y3)));
                         X4 is X, Y4 is Y+1, limite(X4,Y4), positivo(X4,Y4),  assert(edge((X, Y), (X4, Y4)))).



