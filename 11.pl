%+filled +Capacity
bar(_,_).

%state([bar(_,_),bar(_,_),bar(_,_)]).

start:-
    solve([bar(4,6),bar(0,3),bar(6,7)],_),!.

solve(Bars,_):- write(Bars),nl,fail.
solve([bar(A,Ax),bar(B,Bx),bar(C,Cx)],
      [[bar(A,Ax),bar(B,Bx),bar(C,Cx)]]):-
    A = B, C = 0,!;
    A = C, B = 0,!;
    B = C, A = 0,!.
solve(Bars,[Bars|Tail]):-
    assert(state(Bars)),
    move(Bars,AfterTrans,_),
    not(state(AfterTrans)),
    solve(AfterTrans,Tail).

move([bar(A,Ax),bar(B,Bx),bar(C,Cx)],
     [bar(NextA,Ax),bar(NextB,Bx),bar(NextC,Cx)],1):-
    swap_two(bar(A,Ax),bar(B,Bx),bar(NextA,Ax),bar(NextB,Bx)),
    NextC = C.
move([bar(A,Ax),bar(B,Bx),bar(C,Cx)],
     [bar(NextA,Ax),bar(NextB,Bx),bar(NextC,Cx)],2):-
    swap_two(bar(A,Ax),bar(C,Cx),bar(NextA,Ax),bar(NextC,Cx)),
    NextB = B.
move([bar(A,Ax),bar(B,Bx),bar(C,Cx)],
     [bar(NextA,Ax),bar(NextB,Bx),bar(NextC,Cx)],3):-
    swap_two(bar(B,Bx),bar(C,Cx),bar(NextB,Bx),bar(NextC,Cx)),
    NextA = A.

swap_two(A,B,NextA,NextB):-
    swap(A,B,NextA,NextB);
    swap(B,A,NextB,NextA).

swap(bar(A,Ax),bar(B,Bx),bar(NextA,Ax),bar(NextB,Bx)):-
    Tab is A+B,
    BpA is min(Tab,Bx),
    D is BpA-B,
    D \= 0,
    NextA is A-D, NextB is B+D.
