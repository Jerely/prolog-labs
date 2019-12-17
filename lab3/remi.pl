% Написать программу удаления из списка элементов, находящихся на (i ∗ n)-х местах, где i задается в качестве аргумента, а n = 1, 2, 3, . . .

remi(Li, I, Lo) :-
    hel(Li, I, 1, Lo),!.

hel([], _, _, []).

hel([_|Ti], I, I, Lo) :-
    hel(Ti, I, 1, Lo).

hel([Hi|Ti], I, K, [Hi|To]) :-
    I =\= K,
    Kn is K+1,
    hel(Ti, I, Kn, To).
