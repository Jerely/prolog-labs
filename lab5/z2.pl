% init_state(st(L,N,N,M,0)) :- L < N, L < M.
final_state(st(L,_,L,_,_)).
final_state(st(L,_,_,_,L)).

% Отлить воду из второй кувшинки в первый.
move(st(_,N,Na,_,Ma), from2ndTo1st) :- Na =\= N, Ma =\= 0.
% Отлить воду из первой кувшинки во вторую.
move(st(_,_,Na,M,Ma), from1stTo2nd) :- Ma =\= M, Na =\= 0.
% Вылить воду из первой кувшинки.
move(st(_,_,Na,_,_), empty1st) :- Na =\= 0.
% Вылить воду из второй кувшинки.
move(st(_,_,_,_,Ma), empty2nd) :- Ma =\= 0.
% Налить воду в первую кувшинку из резервуара.
move(st(_,N,Na,_,_), fill1st) :- Na =\= N.
% Налить воду во вторую кувшинку из резервуара.
move(st(_,_,_,M,Ma), fill2nd) :- Ma =\= M.

update(st(L,N,Na,M,Ma), from2ndTo1st, st(L,N,Nb,M,Mb)) :-
    Nb is min(N, Ma + Na),
    Mb is Na + Ma - Nb.
update(st(L,N,Na,M,Ma), from1stTo2nd, st(L,N,Nb,M,Mb)) :-
    Mb is min(M, Ma + Na),
    Nb is Na + Ma - Mb.
update(st(L,N,_,M,Ma), empty1st, st(L,N,0,M,Ma)).
update(st(L,N,Na,M,_), empty2nd, st(L,N,Na,M,0)).
update(st(L,N,_,M,Ma), fill1st, st(L,N,N,M,Ma)).
update(st(L,N,Na,M,_), fill2nd, st(L,N,Na,M,M)).

solve_dfs(State, _, []) :-
    final_state(State),!.

solve_dfs(State, History, [Move|Moves]) :-
    move(State, Move),
    update(State, Move, State1),
    \+ member(State1, History),
    solve_dfs(State1, [State1|History], Moves).

test_dfs(L, N, M, Moves) :-
    L < N,
    L < M,
    State = st(L,N,N,M,0),
    solve_dfs(State, [State], Moves).
