%+filled +Capacity
bar(_,_).

%state([bar(_,_),bar(_,_),bar(_,_)]).

start:-
    node(1,1,Node),
    solve([bar(4,6),bar(0,3),bar(6,7)],_,Node),
    clear_state,!.

clear_state:-
    state(X),
    retract(state(X)),
    clear_state,!;true,!.

solve(Bars,_,_):- write(Bars),nl,fail.
solve([bar(A,Ax),bar(B,Bx),bar(C,Cx)],
      [[bar(A,Ax),bar(B,Bx),bar(C,Cx)]],_):-
    A = B, C = 0,!;
    A = C, B = 0,!;
    B = C, A = 0,!.
solve(Bars,[Bars|Tail],Node):-
    find(Node,FNode),
    node(Xn,_,FNode),
    assert(state(Bars)),
    member(Y,[1,2,3]),
    move(Bars,AfterTrans,Y),
    not(state(AfterTrans)),
    show_node(FNode),
    compare(FNode,[Xn,Y]),
    solve(AfterTrans,Tail,[Xn,Y]).

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

compare(N1,N2):-
    N1 = N2;
    node(X,Y,N1),
    node(X1,Y1,N2),
    maplist(write,["bad node:(",X,";",Y,"), choosing :(",
                   X1,";",Y1,")"]),nl,
    show_node(N2).

show_node(Node):-
    node(X,Y,Node),
    dotab(X),write(Y),nl.

node(A,B,[A,B]).

get_children(X,Y,Nodes,N):-
    X1 is X+1,
    create_nodes(N,X1,Y,[],Nodes).

create_nodes(0,_,_,L,L):-!.
create_nodes(N,X,Y,Nodes,RNodes):-
    N > 0, N1 is N-1,
    Y1 is Y+1,
    node(X,Y,Node),
    append(Nodes,[Node],FNodes),
    create_nodes(N1,X,Y1,FNodes,RNodes);
    create_nodes(0,_,_,_,RNodes).

h_ni(G,I,H):-
    nth1(1,G,Xg),
    nth1(2,G,Yg),
    nth1(1,I,Xi),
    nth1(2,I,Yi),
    H is abs(Xg-Xi)+abs(Yg-Yi).

find(PNode,FNode):-
    node(X,Y,PNode),
    get_children(X,Y,Nodes,3),
    dpth_node(Nodes,PNode,[],H),
    min_list(H,Hn),
    nth1(Hn,Nodes,FNode),!.

dpth_node([],_,H,H):-!.
dpth_node([Node|Tail],PNode,Hn,HN):-
    h_ni(Node,PNode,H),
    append([H],Hn,NHn),
    dpth_node(Tail,PNode,NHn,HN).

dotab(0):-!.
dotab(N):-
    N>0,
    N1 is N-1,
    write(">"),
    dotab(N1).
