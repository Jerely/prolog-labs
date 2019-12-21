initField([[a,a,a],[a,a,a],[a,a,a]]).

main() :-
    initField(Field),
    askUserForMove(Field, _).

gameLoop(Field) :-
    movePossible(Field),
    makeStep(Field, Row, Column),
    setByRow(Field, Row, Column, o, ModField),
    writeByLine(ModField),
    askUserForMove(ModField, ModField2),
    gameLoop(ModField2),!.

gameLoop(Field) :-
    \+ movePossible(Field),
    chooseWinner(Field),!.

atLeastOneEmpty([[a,_,_],[_,_,_],[_,_,_]]).
atLeastOneEmpty([[_,a,_],[_,_,_],[_,_,_]]).
atLeastOneEmpty([[_,_,a],[_,_,_],[_,_,_]]).
atLeastOneEmpty([[_,_,_],[a,_,_],[_,_,_]]).
atLeastOneEmpty([[_,_,_],[_,a,_],[_,_,_]]).
atLeastOneEmpty([[_,_,_],[_,_,a],[_,_,_]]).
atLeastOneEmpty([[_,_,_],[_,_,_],[a,_,_]]).
atLeastOneEmpty([[_,_,_],[_,_,_],[_,a,_]]).
atLeastOneEmpty([[_,_,_],[_,_,_],[_,_,a]]).

makeStep(Field, Row, Column) :-
    tryToWin(Field, Row, Column),!.
makeStep(Field, Row, Column) :-
    tryNotLoose(Field, Row, Column),!.
makeStep(Field, Row, Column) :-
    moveToEmpty(Field, Row, Column),!.

tryToWin(Field, Row, Column) :-
    findWinningRow(Field, Row, Column),!.
tryToWin(Field, Row, Column) :-
    findWinningColumn(Field, Row, Column),!.
tryToWin(Field, Row, Column) :-
    findWinningDiag(Field, Row, Column),!.

tryNotLoose(Field, Result) :-
    findLoosingRow(Field, Row, Column),!.
tryNotLoose(Field, Result) :-
    findLoosingColumn(Field, Row, Column),!.
tryNotLoose(Field, Result) :-
    findLoosingDiag(Field, Row, Column)),!.

moveToEmpty([X,_,_], 1, Column) :-
    hasEmpty(X, Column),!.
moveToEmpty([_,X,_], 2, Column) :-
    hasEmpty(X, Column),!.
moveToEmpty([_,_,X], 3, Column) :-
    hasEmpty(X, Column),!.

hasEmpty([a,_,_], 1).
hasEmpty([_,a,_], 2).
hasEmpty([_,_,a], 3).


findWinningRow([X,_,_], 1, Column) :-
    isWinRow(X, Column),!.
findWinningRow([_,X,_], 2, Column) :-
    isWinRow(X, Column),!.
findWinningRow([_,_,X], 3, Column) :-
    isWinRow(X, Column),!.

isWinRow([a,o,o],1).
isWinRow([o,a,o],2).
isWinRow([o,o,a],3).

findWinningColumn([[X1,Y1,Z1],[X2,Y2,Z2],[Z3,Y3,Z3]], Row, Column) :-
    findWinningRow([[X1,X2,X3],[Y1,Y2,Y3],[Z1,Z2,Z3]], Column, Row),!.

findWinningDiag([[X1,_,Z1],[_,Y2,_],[X3,_,Z3]], Row, Column) :-
    

movePossible(Field) :-
    atLeastOneEmpty(Field),
    checkRows(Field),!,
    checkColumns(Field),!,
    checkDiags(Field),!.

checkRows([X,Y,Z]) :-
    checkRow(X),!,
    checkRow(Y),!,
    checkRow(Z),!.

checkDiags([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]]) :-
    checkRow([X1,Y2,Z3]),!,
    checkRow([Z1,Y2,X3]),!.

checkColumns([[X1,Y1,Z1],[X2,Y2,Z2],[X3,Y3,Z3]]) :-
    checkRow([X1,X2,X3]),!,
    checkRow([Y1,Y2,Y3]),!,
    checkRow([Z1,Z2,Z3]),!.


checkRow(X) :-
    \+ finishedRow(X).

finishedRow([x,x,x]).
finishedRow([o,o,o]).

askUserForMove(Field, Field) :-
    \+ movePossible(Field),!.

askUserForMove(Field, ResultField) :-
    movePossible(Field),
    waitForUser(Field, ResultField),
    format('Your move:~n'),
    writeByLine(ResultField),!.

waitForUser(Field, ResultField) :-
    read([Row,Column]),
    requestCorrectInput(Field, Row, Column, ResultField),!.

requestCorrectInput(Field, Row, Column, ResultField) :-
    setByRow(Field, Row, Column, x, ResultField),!.

requestCorrectInput(Field, Row, Column, ResultField) :-
    \+ setByRow(Field, Row, Column, x, _),
    format('You can\'t move there! Please, choose another position.~n'),
    waitForUser(Field, ResultField),!.

writeByLine([Row1,Row2,Row3]) :-
    format('~w~n~w~n~w~n', [Row1,Row2,Row3]).

setByRow(Field, Row, Column, Elem, ModField) :-
    get(Field, Row, WantedRow),
    set(WantedRow, Column, Elem, ModRow), 
    set(Field, Row, ModRow, ModField),!.

get([H|_], 1, H) :- !.
get([_|T], N, Elem) :-
    NewN is N - 1,
    get(T, NewN, Elem),!.

set([a|T], 1, Elem, [Elem|T]) :- !.
set([H|T], N, Elem, [H|NewT]) :-
    NewN is N - 1,
    set(T, NewN, Elem, NewT),!.
