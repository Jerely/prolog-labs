% set_prolog_flag(answer_write_options,  [quoted(true), portray(false), max_depth(30), spacing(next_argument)]).
% test1(X) :-
%     move([[0,1,2],
%           [3,4,5],
%           [6,7,8]], X).
% test2(X) :-
%     move([[1,0,2],
%           [3,4,5],
%           [6,7,8]], X).
% test3(X) :-
%     move([[1,2,0],
%           [3,4,5],
%           [6,7,8]], X).
% test4(X) :-
%     move([[1,2,3],
%           [0,4,5],
%           [6,7,8]], X).
% test5(X) :-
%     move([[1,2,3],
%           [4,0,5],
%           [6,7,8]], X).
move([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]], moveUp) :-
    move([[X1,X2,X3],[Y1,Y2,Y3],[Z1,Z2,Z3]], moveLeft).
move([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]], moveDown) :-
    move([[X1,X2,X3],[Y1,Y2,Y3],[Z1,Z2,Z3]], moveRight).
move([X,Y,Z], moveLeft) :-
    moveRow(X, moveLeft);
    moveRow(Y, moveLeft);
    moveRow(Z, moveLeft).
move([X,Y,Z], moveRight) :-
    moveRow(X, moveRight);
    moveRow(Y, moveRight);
    moveRow(Z, moveRight).
moveRow([0,_,_], moveRight).
moveRow([_,0,_], moveRight).
moveRow([_,0,_], moveLeft).
moveRow([_,_,0], moveLeft).

moveCol(moveUp, moveLeft).
moveCol(moveDown, moveRight).

final_state([[1,2,3],
             [8,0,4],
             [7,6,5]]).

updateRow([0,Y,Z], moveRight, [Y,0,Z]).
updateRow([X,0,Z], moveRight, [X,Z,0]).
updateRow([X,0,Z], moveLeft, [0,X,Z]).
updateRow([X,Y,0], moveLeft, [X,0,Y]).

update([Xa,Ya,Za], Move, [Xb,Ya,Za]) :- updateRow(Xa, Move, Xb).
update([Xa,Ya,Za], Move, [Xa,Yb,Za]) :- updateRow(Ya, Move, Yb).
update([Xa,Ya,Za], Move, [Xa,Ya,Zb]) :- updateRow(Za, Move, Zb).

update([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]], Move, [[X4,Y1,Z1],[X5,Y2,Z2],[X6,Y3,Z3]]) :-
    moveCol(Move, Move1),
    updateRow([X1,X2,X3], Move1, [X4,X5,X6]).
update([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]], Move, [[X1,Y4,Z1],[X2,Y5,Z2],[X3,Y6,Z3]]) :-
    moveCol(Move, Move1),
    updateRow([Y1,Y2,Y3], Move1, [Y4,Y5,Y6]).
update([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]], Move, [[X1,Y1,Z4],[X2,Y2,Z5],[X3,Y3,Z6]]) :-
    moveCol(Move, Move1),
    updateRow([Z1,Z2,Z3], Move1, [Z4,Z5,Z6]).

mapetop(1,0,0).
mapetop(2,0,1).
mapetop(3,0,2).
mapetop(8,1,0).
mapetop(0,1,1).
mapetop(4,1,2).
mapetop(7,2,0).
mapetop(6,2,1).
mapetop(5,2,2).

h(F, E, Heu) :-
    findPos(F, E, Ya, Xa),
    mapetop(E, Yb, Xb),
    DXPos is abs(Xa - Xb),
    DYPos is abs(Ya - Yb),
    Heu is DXPos + DYPos.

findPos([[X,_,_],[_,_,_],[_,_,_]], X, 0, 0).
findPos([[_,X,_],[_,_,_],[_,_,_]], X, 0, 1).
findPos([[_,_,X],[_,_,_],[_,_,_]], X, 0, 2).
findPos([[_,_,_],[X,_,_],[_,_,_]], X, 1, 0).
findPos([[_,_,_],[_,X,_],[_,_,_]], X, 1, 1).
findPos([[_,_,_],[_,_,X],[_,_,_]], X, 1, 2).
findPos([[_,_,_],[_,_,_],[X,_,_]], X, 2, 0).
findPos([[_,_,_],[_,_,_],[_,X,_]], X, 2, 1).
findPos([[_,_,_],[_,_,_],[_,_,X]], X, 2, 2).

heu(F, Heu) :-
    h(F, 1, H1),
    h(F, 2, H2),
    h(F, 3, H3),
    h(F, 4, H4),
    h(F, 5, H5),
    h(F, 6, H6),
    h(F, 7, H7),
    h(F, 8, H8),
    Heu is H1+H2+H3+H4+H5+H6+H7+H8,!.

f_func(State, D, F) :-
    heu(State,H),
    F is D + H.

solve(Start, Soln) :- 
    solvable(Start),
    f_func(Start, 0, F),
    search([node(Start,0,F,[])],S),
    reverse(S,Soln).

search([node(State, _D, _F, Soln)|_T], Soln) :- final_state(State).
search([B|R],S) :-
    expand(B, Children),
    insert_all(Children, R, NewOpen),
    search(NewOpen,S),!.

expand(node(State,D,_F,Path), Children) :-
    bagof(node(Child,D1,F,[Move|Path]),
          ( D1 is D+1,
            move(State, Move),
            update(State, Move, Child),
            f_func(Child,D1,F) ),
          Children).

insert_all([H|T], Open1, Open3) :-
    insert(H, Open1, Open2),
    insert_all(T, Open2, Open3).
insert_all([], Open, Open).

repNode(node(State,_,_,_), [node(State,_,_,_)|_]).

insert(Node,Open,Open) :- repNode(Node,Open),!.
insert(N, [H|T], [N,H|T]) :- cheaper(N,H),!.
insert(N, [H|T], [H|S]) :- insert(N,T,S),!.
insert(N,[],[N]).

cheaper(node(_,_,H1,_), node(_,_,H2,_)) :- H1 < H2.

solvable([[X1,X2,X3],[Y1,Y2,Y3],[Z1,Z2,Z3]]) :-
    countInv([X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3], Inv),
    % countInv([Y2,X1,X2,X3,Y3,Z3,Z2,Z1,Y1], Inv),
    mod(Inv, 2) =:= 1,!.


countGT(_, [], 0).
countGT(X, [H|T], C) :-
    H =\= 0,
    H < X,
    countGT(X, T, C1),
    C is C1+1,!.
countGT(X, [_|T], C) :-
    countGT(X,T,C),!.

countInv([], 0).
countInv([H|T], C) :-
    countGT(H, T, C1),
    countInv(T, C2),
    C is C1+C2,!.

solver(S,[H|T],R) :-
    move(S,H),
    update(S, H, Smut),
    solver(Smut, T, R),!.
solver(S,[],S).

% [[2,8,3],
%  [1,6,4],
%  [7,0,5]]
% 
% % [[5, 6, 7],
% % [1, 0, 4],
% % [2, 8, 3]]
% 
% [[4, 0, 6],
%  [7, 2, 3],
%  [8, 5, 1]]
% 
% [[3, 6, 4],
%  [0, 5, 8],
%  [2, 7, 1]]
