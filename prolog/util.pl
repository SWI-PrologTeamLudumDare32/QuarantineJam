:- module(util, [take/3  % N, InList, OutList
                ]).
/** <Module> small utility predicates
 *
 */

take(0, _, []).
take(_, [], []).
take(N, [H|T], [H|TO]) :-
    succ(NN, N),
    take(NN, T, TO).
