% set_prolog_flag(answer_write_options,  [quoted(true), portray(false), max_depth(20), spacing(next_argument)]).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).
digit(10).

divisable(1).
divisable(2).
divisable(4).
divisable(8).
divisable(11).
divisable(13).
divisable(16).
divisable(17).
divisable(19).

last([H], H).
last([_|T], L) :-
    last(T, L).

unique(X, []) :-
    digit(X).

unique(X, [H|T]) :-
    digit(X),
    digit(H),
    X =\= H,
    unique(X, T).

div(X, [H|_]) :-
    digit(X),
    digit(H),
    X =\= H,
    divisable(Z),
    X + H =:= Z.

div(X, []) :-
    digit(X).

pred([], 0).

pred([H|T], X) :-
    N is X-1,
    N > -1,
    pred(T, N),
    digit(H),
    div(H, T),
    unique(H,T).


star([H|T]) :-
    pred([H|T], 10),
    last(T, L),
    div(H, [L]).
