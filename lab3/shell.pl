initGap(N, Gap) :-
    ntgp(N, 1, Gap),!.

ntgp(N, Init, Gap) :-
    Init >= N,
    Gap is div(Init, 2),!.
ntgp(N, Init, Gap) :-
    Init < N,
    NewInit is Init * 2 + 1,
    ntgp(N, NewInit, Gap),!.

get([H|_], 0, H) :- !.
get([_|T], N, Elem) :-
    NewN is N - 1,
    get(T, NewN, Elem),!.

set([_|T], 0, Elem, [Elem|T]) :- !.
set([H|T], N, Elem, [H|NewT]) :-
    NewN is N - 1,
    set(T, NewN, Elem, NewT),!.

shell(InList, OutList) :-
    length(InList, Length),
    initGap(Length, Gap),
    while0(InList, Length, Gap, OutList),!.

while0(InList, _, Gap, InList) :-
    Gap < 1,!.
while0(InList, Length, Gap, OutList) :-
    Gap >= 1,
    % format('~d ', Gap),
    % print(InList),
    % format('~n'),
    I is Gap,
    while1(InList, Length, Gap, I, MidList),
    NewGap is div(Gap, 2),
    while0(MidList, Length, NewGap, OutList),!.

while1(InList, Length, _, I, InList) :-
    I >= Length,!.
while1(InList, Length, Gap, I, OutList) :-
    I < Length,
    get(InList, I, Temp),
    J is I,
    while2(InList, Gap, Temp, J, MidList, NewJ),
    set(MidList, NewJ, Temp, MidList2),
    NewI is I+1,
    while1(MidList2, Length, Gap, NewI, OutList),!.

while2(InList, Gap, _, J, InList, J) :-
    J < Gap,!.
while2(InList, Gap, Temp, J, InList, J) :-
    J >= Gap,
    JMinusGap is J - Gap,
    get(InList, JMinusGap, AJMinusGap),
    AJMinusGap =< Temp,!.
while2(InList, Gap, Temp, J, OutList, NewJ) :-
    J >= Gap,
    JMinusGap is J - Gap,
    get(InList, JMinusGap, AJMinusGap),
    AJMinusGap > Temp,
    set(InList, J, AJMinusGap, MidList),
    while2(MidList, Gap, Temp, JMinusGap, OutList, NewJ),!.
