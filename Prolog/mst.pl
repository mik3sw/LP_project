%%%% -*- Mode: Prolog -*-



%%% Progetto a cura di:
% Davide Mazzitelli [851657]
% Michele Angelo Marcucci [851905]

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.


new_graph(G) :-
    graph(G),
    !.
new_graph(G) :-
    assert(graph(G)),
    !.

delete_graph(G) :-
    graph(G),
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retractall(graph(G)), !.

new_vertex(G, V) :-
    nonvar(G),
    nonvar(V),
    graph(G),
    vertex(G, V),
    !.
new_vertex(G, V) :-
    assert(vertex(G, V)),
    !.

graph_vertices(G, Vs) :-
    nonvar(G),
    graph(G),
    findall(V, vertex(G, V), Vs).

list_vertices(G) :-
    nonvar(G),
    graph(G),
    listing(vertex(G, _)).

new_arc(G, U, V) :-
    U \= V,
    new_arc(G, U, V, 1),
    !.
new_arc(G, U, U) :-
    new_arc(G, U, U, 1),
    !.
new_arc(G, U, V, Weight) :-
    U \= V,
    graph(G),
    arc(G, U, V, _),
    retractall(arc(G, U, V, _)),
    retractall(arc(G, V, U, _)),
    new_arc(G, U, V, Weight),
    !.
new_arc(G, U, V, Weight) :-
    U\= V,
    assert(arc(G, U, V, Weight)),
    assert(arc(G, V, U, Weight)),
    !.
new_arc(G, U, U, Weight) :-
    graph(G),
    arc(G, U, U, _),
    retractall(arc(G, U, U, _)),
    new_arc(G, U, U, Weight),
    !.
new_arc(G, U, U, Weight) :-
    assert(arc(G, U, U, Weight)),
    !.

graph_arcs(G, Es) :-
    nonvar(G),
    graph(G),
    findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es), !.

vertex_neighbors(G, V, Ns) :-
    nonvar(G),
    nonvar(V),
    graph(G),
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns),
    !.

adjs(G, V, Vs) :-
    nonvar(G),
    nonvar(V),
    graph(G),
    vertex(G, V),
    findall(vertex(G, N), arc(G, V, N, _), Vs), !.

list_arcs(G) :-
    nonvar(G),
    graph(G),
    listing(arc(G, _, _, _)),
    !.

list_graph(G) :-
    nonvar(G),
    graph(G),
    list_vertices(G),
    list_arcs(G),
    !.

set_graph(G, V1, V2, Weight) :-
    nonvar(G),
    nonvar(V1),
    nonvar(V2),
    nonvar(weight),
    graph(G),
    new_vertex(G, V1),
    new_vertex(G, V2),
    new_arc(G, V1, V2, Weight).

read_graph(G, FileName) :-
    nonvar(G),
    csv_read_file(FileName, Vs, [separator(0';)]),
    new_graph(G),
    foreach(member(row(V1, V2, Weight), Vs), set_graph(G, V1, V2, Weight)).

sostituzione([], []).
sostituzione([arc(_, V1, V2, W) | Xs], [row(V1, V2, W) | Ys]) :-
    sostituzione(Xs, Ys).

write_graph(G, FileName) :-
    nonvar(G),
    write_graph(G, FileName, graph).
write_graph(G, FileName, Type) :-
    nonvar(G),
    graph(G),
    Type = graph,
    findall(row(U, V, Weight), arc(G, U, V, Weight), Es),
    csv_write_file(FileName, Es, [separator(0';)]), !.
write_graph(G, FileName, Type) :-
    nonvar(G),
    is_list(G),
    Type = edges,
    sostituzione(G, Es),
    csv_write_file(FileName, Es, [separator(0';)]), !.




new_heap(H) :-
    heap(H, _S),
    !.
new_heap(H) :-
    assert(heap(H, 0)),
    !.

delete_heap(H) :-
    heap(H, _),
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)), !.

heap_has_size(H, S) :- heap(H, S), !.

heap_empty(H) :-
    heap(H, _),
    heap_has_size(H, 0), !.

heap_not_empty(H) :-
    heap(H, S),
    heap_has_size(H, S),
    S > 0, !.

heap_head(H, K, V) :-
    heap_not_empty(H),
    heap_entry(H, 1, K, V), !.

heap_insert(H, K, V) :- % heap vuoto
    heap(H, _),
    heap_empty(H),
    retract(heap(H, _)),
    assert(heap(H, 1)),
    assert(heap_entry(H, 1, K, V)), !.
heap_insert(H, K, V) :-
    heap(H, _),
    heap_not_empty(H),
    heap_entry(H, _, K, V), !.
heap_insert(H, K, V) :-
    heap(H, _),
    heap_not_empty(H),
    heap_entry(H, P, K1, V),
    K1 \= K,
    retract(heap_entry(H, P, K1, V)),
    assert(heap_entry(H, P, K, V)),
    heap_property(H, P), !.
heap_insert(H, K, V) :- % V non nell'heap
    heap(H, _),
    heap_not_empty(H),
    \+ heap_entry(H, _, _, V),
    heap_has_size(H, S),
    Stmp is S + 1,
    retract(heap(H, S)),
    assert(heap(H, Stmp)),
    assert(heap_entry(H, Stmp, K, V)),
    heap_property(H, Stmp), !.

heap_property(H, P) :-    % nodo padre>P : scambio
    heap(H, _),
    Father is floor(P/2),
    heap_entry(H, P, K, V),
    heap_entry(H, Father, Kf, Vf),
    Kf > K,
    retract(heap_entry(H, Father, Kf, Vf)),
    assert(heap_entry(H, Father,K, V)),
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, P, Kf, Vf)),
    heap_property(H, Father), !.
heap_property(H, P) :- % nodo padre <= P : heapify
     heap(H, _),
    Father is floor(P/2),
    heap_entry(H, P, K, _),
    heap_entry(H, Father, Kf, _),
    Kf =< K,
    heapify(H, P), !.

heapify(H, P) :- % P non ha figli: nulla
    heap(H, S),
    S \= 0,
    P =< S,
    Left is 2*P,
    Left > S,
    Right is ((2*P)+1),
    Right > S, !.
heapify(H, P) :- % P con un figlio, P e' il minimo
    heap(H, S),
    S \= 0,
    P =< S,
    Left is 2*P,
    Left =< S,
    Right is ((2*P)+1),
    Right > S,
    heap_entry(H, Left, Kleft, _),
    heap_entry(H, P, K, _),
	K =< Kleft, !.
heapify(H, P) :- % P con un figlio, figlio e' il minimo
    heap(H, S),
    S \= 0,
    P =< S,
    Left is 2*P,
    Left =< S,
    Right is ((2*P)+1),
    Right > S,
    heap_entry(H, Left, Kleft, Vleft),
    heap_entry(H, P, K, V),
    Kleft < K,
    retract(heap_entry(H, Left, Kleft, Vleft)),
    assert(heap_entry(H, Left, K, V)),
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, P, Kleft, Vleft)),
    heap_property(H, P), !.
heapify(H, P) :- % P con 2 figli, P e' il minimo
    heap(H, S),
    S \= 0,
    P =< S,
    Left is 2*P,
    Left =< S,
    Right is ((2*P)+1),
    Right =< S,
    heap_entry(H, Left, _, _),
    heap_entry(H, Right, _, _),
    heap_entry(H, P, _, _),
    minimo(H, P, Left, Right, Min),
    P = Min, !.
heapify(H, P) :- % P con 2 figli, P non e' il minimo
    heap(H, S),
    S \= 0,
    P =< S,
    Left is 2*P,
    Left =< S,
    Right is ((2*P)+1),
    Right =< S,
    heap_entry(H, Left, _, _),
    heap_entry(H, Right, _, _),
    heap_entry(H, P, _, _),
    minimo(H, P, Left, Right, Min),
    P \= Min,
    heap_entry(H, Min, Kmin, Vmin),
    heap_entry(H, P, K, V),
    retract(heap_entry(H, Min, Kmin, Vmin)),
    assert(heap_entry(H, Min, K, V)),
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, P, Kmin, Vmin)),
    heapify(H, Min), !.

minimo(H, P1, P2, P3, Min) :- % K(Min1) < K(Min2)
    heap(H, S),
    P1 =< S,
    P2 =< S,
    P3 =< S,
    minimo(H, P1, P2, Min1),
    minimo(H, P2, P3, Min2),
    heap_entry(H, Min1, K1, _),
    heap_entry(H, Min2, K2, _),
    K1 < K2,
    Min is Min1, !.
minimo(H, P1, P2, P3, Min) :- % K(Min1) >= K(Min2)
    heap(H, S),
    P1 =< S,
    P2 =< S,
    P3 =< S,
    minimo(H, P1, P2, Min1),
    minimo(H, P2, P3, Min2),
    heap_entry(H, Min1, K1, _),
    heap_entry(H, Min2, K2, _),
    K1 >= K2,
    Min is Min2, !.
minimo(H, P1, P2, Min) :-
    heap(H, S),
    S >= P1,
    S >= P2,
    heap_entry(H, P1, K1, _),
    heap_entry(H, P2, K2, _),
    K1 < K2,
    Min is P1, !.
minimo(H, P1, P2, Min) :-
    heap(H, S),
    S >= P1,
    S >= P2,
    heap_entry(H, P1, K1, _),
    heap_entry(H, P2, K2, _),
    K1 >= K2,
    Min is P2, !.

heap_extract(H, _, _) :-  % heap vuoto
    heap_empty(H), !.
heap_extract(H, K, V) :-  % heap_size=1
    heap(H, S),
    heap_not_empty(H),
    S = 1,
    heap_entry(H, S, K, V),
    Stmp is S-1,
    retract(heap_entry(H, S, K, V)),
    retract(heap(H, S)),
    assert(heap(H, Stmp)), !.
heap_extract(H, K, V) :-  % heap_size > 1
    heap(H, S),
    heap_not_empty(H),
    Stmp is S-1,
    heap_head(H, K, V),
    heap_entry(H, S, Klast, Vlast),
    scambio(H, 1, K, V, S, Klast, Vlast),
    retract(heap_entry(H, S, K, V)),
    retract(heap(H, S)),
    assert(heap(H, Stmp)),
    heapify(H, 1), !.

scambio(H, P1, K1, V1, P2, K2, V2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    assert(heap_entry(H, P1, K2, V2)), !.

list_heap(H) :-
    listing(heap_entry(H, _, _, _)).




mst_prim(G, Source) :-
    graph(G),
    vertex(G, Source),
    clear(G),
    graph_vertices(G, Vs),
    imposta_key_previous(G, Vs),
    retract(vertex_key(G, Source, _)),
    assert(vertex_key(G, Source, 0)),
    new_heap(codaMinPrio),
    crea_heap(G, codaMinPrio, Vs),
    prim(G, codaMinPrio).

imposta_key_previous(_, []) :- !.
imposta_key_previous(G, [X | Xs]) :-
    graph(G),
    vertex(G, X),
    assert(vertex_key(G, X, inf)),
    assert(vertex_previous(G, X, nil)),
    imposta_key_previous(G, Xs).

crea_heap(_, _, []) :- !.
crea_heap(G, Q, [X | Xs]) :-
    graph(G),
    heap(Q, _),
    vertex(G, X),
    vertex_key(G, X, K),
    heap_insert(Q, K, X),
    crea_heap(G, Q, Xs).

prim(G, Q) :-
    graph(G),
    heap_empty(Q),
    !.
prim(G, Q) :-
    graph(G),
    heap_not_empty(Q),
    heap_extract(Q, _, U),
    adjs(G, U, Vs),
    ciclo_prim(G, Q, U, Vs),
    prim(G, Q);
    !.

ciclo_prim(_, _, _, []) :- !.  
ciclo_prim(G, Q, U, [X | Xs]) :-
    X = vertex(G, V),
    heap_entry(Q, _, _, V),
    vertex_key(G, V, K),
    arc(G, U, V, W),
    W < K,
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)),
    retract(vertex_key(G, V, K)),
    assert(vertex_key(G, V, W)),
    heap_insert(codaMinPrio, W, V),
    ciclo_prim(G, Q, U, Xs),
    !.
ciclo_prim(G, Q, U, [X | Xs]) :-
    X = vertex(G, V),
    heap_entry(Q, _, _, V),
    vertex_key(G, V, K),
    arc(G, V, U, W),
    W < K,
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)),
    retract(vertex_key(G, V, K)),
    assert(vertex_key(G, V, W)),
    heap_insert(codaMinPrio, W, V),
    ciclo_prim(G, Q, U, Xs),
    !.
ciclo_prim(G, Q, U, [X | Xs]) :-
    X = vertex(G, V),
    heap_entry(Q, _, _, V),
    vertex_key(G, V, K),
    arc(G, U, V, W),
    W >= K,
    ciclo_prim(G, Q, U, Xs),
    !.
ciclo_prim(G, Q, U, [X | Xs]) :-
    X = vertex(G, V),
    not(heap_entry(Q, _, _, V)),
    ciclo_prim(G, Q, U, Xs),
    !.


mst_get(G, Source, PreorderTree) :-
    vertex_neighbors(G, Source, Vs),
    ordina(Vs, Ws),
    preorder(G, Ws, PreorderTree).


ordina(Xs, Ys) :-
    sort(3, @=<, Xs, Ks),
    sort(4, @=<, Ks, Ys).


preorder(_, [], []).

preorder(G, [X | Xs], [X | Ps]) :-
    X = arc(G, V, U, _),
    vertex_previous(G, U, V),
    not(vertex_previous(G, V, U)),
    vertex_neighbors(G, U, Ys),
    ordina(Ys, Zs),
    preorder(G, Zs, Ks),
    preorder(G, Xs, Ms),
    append(Ks, Ms, Ps).


preorder(G, [X | Xs], Ps) :-
    X = arc(G, V, U, _),
    vertex_previous(G, V, U),
    preorder(G, Xs, Ps).

preorder(G, [X | Xs], Ps) :-
    X = arc(G, V, U, _),
    not(vertex_previous(G, U, V)),
    preorder(G, Xs, Ps).

clear(G) :-
    graph(G),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    new_heap(codaMinPrio),
    delete_heap(codaMinPrio),
    !.

%%%% end of file -*- mst.pl
