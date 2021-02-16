%%% -*- Mode: Prolog -*-
% README.txt

Progetto realizzato da: 
- Michele Angelo Marcucci   [851905]
- Davide Mazzitelli         [851657]


%------------------------------------------------------------------------------%
% CONSEGNA %

Nome progetto: mst.pl
Linguaggio utilizzato: SWI-Prolog 8.2.1

Obiettivo: implementare l'algoritmo di prim per la creazione dell'MST
riferito a un grafo non orientato.

Librerie necessarie: per eseguire prim è necesario implementare le
funzioni che ci permettono di manipolare grafi e heaps

%------------------------------------------------------------------------------%
% INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI GRAFI %

:- new_graph(G).
Questo predicato inserisce un nuovo grafo nella base-dati Prolog. 


:- delete_graph(G).
Rimuove tutto il grafo, con vertici e archi riferiti al suddetto grafo
dalla base-dati Prolog.


:- new_vertex(G, V).
Aggiunge il vertice V riferito al grafo G nella base-dati Prolog. 


:- graph_vertices(G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti i vertici di G.
 

:- list_vertices(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei 
vertici del grafo G.


:- new_arc(G, U, V, Weight).
Aggiunge un arco del grafo G alla base dati Prolog, se il peso non è specificato
Viene creato un arco da U a V di peso (Weight) 1.
Come logica noi abbiamo fatto in modo che creando un arco A-B venga creato anche il
suo opposto B-A, in modo da facilitarci alcuni controlli usati nell'algoritmo
di Prim. 


:- graph_arcs(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.


:- vertex_neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente
gli archi, arc(G, V, N, W) e arc(G, N, V, W), che portano ai vertici N
immediatamente raggiungibili da V .


:- adjs(G, V, Vs).
Questo predicato è vero quando V è un vertice di G e Vs è una lista contenente
tutti i vertici, vertex(G, U) a esso adiacenti.


:- list_arcs(G).
Questo predicato stampa alla console dell’interprete Prolog una lista degli 
archi del grafo G.


:- list_graph(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei 
vertici e degli archi del grafo G.


:- write_graph(G, FileName).
Questo predicato è vero quando G viene scritto sul file FileName secondo il 
valore dell’argomento Type.
Type può essere graph o edges. Se Type è graph, allora G è un termine che 
identifica un grafo nella base di dati Prolog; In FileName saranno scritti 
gli archi del grafo secondo il formato descritto per read_graph/2. 
Se Type è edges, allora G è una lista di archi, ognuno dei quali viene 
stampato su FileName, sempre secondo il formato descritto per read_graph/2.
La versione write_graph/2 è implementata come: 
write_graph(G, FileName) :- write_graph(G, FileName, graph).
Il tipo del file deve essere ".csv".


:- read_graph(G, Filename).
Questo predicato legge un “grafo” G, da un file FileName (di tipo CSV) e lo 
inserisce nel data base di Prolog. 
Si presuppone che il separatore usato di default dal file csv 
(per identificare le colonne) sia il carattere ';'. 
In caso contrario non verrà creato il grafo correttamente.


%------------------------------------------------------------------------------%
% INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEL MINHEP %


:- new_heap(H).
Questo predicato inserisce un nuovo heap nella base-dati Prolog.


:- delete_heap(H).
Rimuove tutto lo heap dalla base-dati Prolog.


:- heap_has_size(H, S).
Questo predicato è vero quanto S è la dimensione dello heap. 


:- heap_empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.


:- heap_not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un elemento.


:- heap_head(H, K, V).
Questo predicato è vero quando l’elemento dello heap H con 
chiave minima K è V.


:- heap_insert(H, K, V).
Il predicato insert/3 è vero quando l’elemento V è inserito nello heap H 
con chiave K.
Lo heap H dovrà essere modificato in modo da fargli mantenere 
la "heap property" per ogni elemento.


:- heap_extract(H, K, V).
Il predicato extract/3 è vero quando la coppia K, V con K minima, è rimossa
dallo heap H.
Lo heap H dovrà essere modificato in modo da fargli mantenere 
la “heap property” per ogni elemento.


:- modify_key(H, NewKey, OldKey, W).
Non abbiamo utilizzato questo predicato.


:- list_heap(H).
Il predicato richiama listing/1 per stampare sulla console Prolog lo stato
interno dello heap.


%-----% Predicati creati da noi %-----%

:- heap_property(H, P).
Questo predicato viene utilizzato da heap_insert/3 per aggiornare 
lo stato dello heap in modo da rispettare la heap property dopo che una entry
è stata aggiunta o modificata.

:- heapify(H, P).
Questo predicato riordina le entry dello heap facendo in modo di rispettare 
la "heap property".

:- scambio(H, P1, K1, V1, P2, K2, V2).
Questo predicato scambia la posizione nello heap di due entry.

:- minimo(H, P1, P2, P3, Min).
Questo predicato viene utilizzato da heapify/2 per determinare quale tra 
nodo padre (P1) e i nodi figli (P2, P3) ha chiave minima.


%------------------------------------------------------------------------------%
% MINIMUM SPANNING TREE %

:- vertex_key(G, V, K)
Predicato dynamic;
Questo predicato è vero quando V è un vertice di G e, durante e dopo 
l’esecuzione dell’algoritmo di Prim, contiene il peso minimo di un arco che 
connette V nell’albero minimo; se questo arco non esiste 
(ed all’inizio dell’esecuzione) allora K è inf.


:- vertex_previous(G, V, U).
Predicato dynamic;
Questo predicato è vero quando V ed U sono vertici di G e, durante e dopo 
l’esecuzione dell’algoritmo di Prim, il vertice U è il vertice “genitore” 
(“precedente”, o “parent”) di V nel minimum spanning tree.


:- mst_prim(G, Source).
Questo predicato ha successo con un effetto collaterale. Dopo la sua prova, 
la base-dati Prolog ha al suo interno i predicati vertex_key(G, V, k) per ogni 
V appartenente a G; la base-dati Prolog contiene anche i predicati 
vertex_previous(G, V, U) per ogni V, ottenuti durante le iterazioni 
dell’algoritmo di Prim.


:- mst_get(G, Source, PreorderTree).
Questo predicato è vero quando PreorderTree è una lista degli archi del MST 
ordinata secondo un attraversamento preorder dello stesso, fatta rispetto al 
peso dell’arco. Attenzione che l’albero non è binario e dovete ordinare archi 
con pari peso secondo l’ordinamento “lessicografico” del vertice “target”


%-----% Predicati creati da noi %-----%

:- imposta_key_previous(G, L).
Questo predicato è utilizzato in mst_prim/2 per inizializzare le vertex_key/3
e i vertex_previous/3 dei vertici contenuti nella lista L del grafo G
rispettivamente a infinito (inf) e nil.

:- crea_heap(G, Q, L).
Questo predicato è utilizzato in mst_prim/2 per creare la coda di priorità 
contenente i vertici del grafo G e le rispettive Key (che all'inizio saranno inf)

:- prim(G, Q).
Questo predicato è utilizzato in mst_prim/2, estrae dalla coda di priorità Q 
Il vertice e la rispettiva chiave del grafo G e richiama il predicato ciclo_prim/4.

:- ciclo_prim(G, Q, U, L).
Questo predicato è utilizzato in prim/2 ed esegue i controlli e le modifiche sui
vertici adiacenti al vertice U (contenuti nella lista L) e se necessario imposta 
i nuovi valori per vertex_key/3 e vertex_previous/3.

:- clear(G).
Questo predicato serve per preparare e pulire la base dati prolog necessaria ad
eseguire l'algoritmo di Prim in caso di test successivi per eliminare i possibili
conflitti ed effetti collaterali.

:- ordina(Xs, Ys).
Questo predicato è vero se la lista Ys è la lista Xs ordinata rispetto al peso 
dell'arco e della precedenza alfabetica dei nomi dei suoi vertici in caso 
di peso uguale.

:- preorder(G, Xs, Ys).
Questo predicato è il cuore di mst_get/3 e serve per creare la lista ordinata 
secondo un attraversamento preorder.

%------------------------------------------------------------------------------%

end of file -*- README.txt