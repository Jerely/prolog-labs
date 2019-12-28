solve_dfs(State, _, []) :-
    final_state(State).

solve_dfs(State, History, [Move|Moves]) :-
    move(State, Move),
    update(State, Move, State1),
    legal(State1),
    \+ member(State1, History),
    solve_dfs(State1, [State1|History], Moves).

test_dfs(Problem, Moves) :-
    initial_state(Problem, State),
    solve_dfs(State, [State], Moves).

initial_state('Исходное состояние: все находятся на левом берегу.', wgc('Лодка на левом берегу.', ['Волк','Коза','Капуста'],[])).
final_state(wgc('Лодка на правом берегу.', [], ['Волк', 'Коза', 'Капуста'])).

move(wgc('Лодка на левом берегу.',L,_),Cargo) :-
    member(Cargo,L).

move(wgc('Лодка на правом берегу.',_,R),Cargo) :-
    member(Cargo,R).

move(wgc(_,_,_), 'Без груза').

update(wgc(B,L,R),Cargo,wgc(B1,L1,R1)) :-
    update_boat(B,B1),
    update_banks(Cargo,B,L,R,L1,R1).

update_boat('Лодка на левом берегу.', 'Лодка на правом берегу.').
update_boat('Лодка на правом берегу.', 'Лодка на левом берегу.').

select(X,[X|T],T).
select(X,[H|T],[H|D]) :-
    select(X,T,D).

precedes('Волк',_).
precedes(_,'Капуста').

insert(X, [], [X]).

insert(X,[Y|Ys],[X,Y|Ys]) :-
    precedes(X,Y).

insert(X,[Y|Ys],[Y|Zs]) :-
    precedes(Y,X),
    insert(X,Ys,Zs).

update_banks('Без груза',_,L,R,L,R).

update_banks(Cargo, 'Лодка на левом берегу.',L,R,L1,R1) :-
    select(Cargo,L,L1),
    insert(Cargo,R,R1).

update_banks(Cargo, 'Лодка на правом берегу.',L,R,L1,R1) :-
    select(Cargo,R,R1),
    insert(Cargo,L,L1).

legal(wgc('Лодка на левом берегу.',_,R)) :-
    \+ illegal(R).

legal(wgc('Лодка на правом берегу.',L,_)) :-
    \+ illegal(L).

illegal(List) :-
    member('Волк',List),
    member('Коза',List).

illegal(List) :-
    member('Коза',List),
    member('Капуста',List).
