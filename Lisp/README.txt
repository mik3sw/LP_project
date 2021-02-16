%%% -*- Mode: Lisp -*-
% README.txt

Progetto realizzato da: 
- Michele Angelo Marcucci   [851905]
- Davide Mazzitelli         [851657]


%------------------------------------------------------------------------------%
% CONSEGNA %

Nome progetto: mst.lisp
Linguaggio utilizzato: COMMON LISP

Obiettivo: implementare l'algoritmo di prim per la creazione dell'MST
riferito a un grafo non orientato.

Librerie necessarie: per eseguire prim è necesario implementare le
funzioni che ci permettono di manipolare grafi e heaps


%------------------------------------------------------------------------------%
% INTERFACCIA LISP PER LA MANIPOLAZIONE DEI GRAFI %

(is-graph graph-id)
Questa funzione ritorna il graph-id stesso se questo grafo è già stato creato, 
oppure NIL se no.


(new-graph graph-id)
Questa funzione genera un nuovo grafo e lo inserisce nel data base 
(ovvero nella hash-table) dei grafi.


(delete-graph graph-id)
Rimuove l’intero grafo dal sistema (vertici archi etc); ovvero rimuove tutte 
le istanze presenti nei data base (ovvero nelle hash-tables) del sistema.


(new-vertex graph-id vertex-id)
Aggiunge un nuovo vertice vertex-id al grafo graph-id.


(graph-vertices graph-id)
Questa funzione torna una lista di vertici del grafo.


(new-arc graph-id vertex1-id vertex2-id)
Questa funzione aggiunge un arco del grafo graph-id nella hash-table *arcs*.
Se i vertici specificati non esistono, questi vengono creati per evitare
conflitti con l'algoritmo di Prim.
Come logica noi abbiamo fatto in modo che creando un arco A-B venga creato anche il
suo opposto B-A, in modo da facilitarci alcuni controlli usati nell'algoritmo
di Prim. 


(graph-arcs graph-id)
Questo funzione ritorna una lista una lista di tutti gli archi presenti in graph-id.


(graph-vertex-neighbors graph-id vertex-id)
Questa funzione ritorna una lista arc-rep-list contenente gli archi 
(arc graph-id vertex-id N W) che portano ai vertici N immediatamente 
raggiungibili da vertex-id.


(graph-vertex-adjacent graph-id vertex-id)
Questa funzione ritorna una lista vertex-rep-list contenente i vertici 
(arc graph-id vertex-id V) adiacenti a vertex-id. Per questa funzione
abbiamo implementato una funzione esterna


(graph-print graph-id)
Questa funzione stampa alla console dell’interprete Common Lisp una 
lista dei vertici e degli archi del grafo graph-id.


%------------------------------------------------------------------------------%
% INTERFACCIA LISP PER LA MANIPOLAZIONE DEL MIN-HEAP %


(new-heap heap-id &optional (capacity 42))
Questa funzione inserisce un nuovo heap nella hash-table *heaps*.


(heap-delete heap-id)
Rimuove tutto lo heap indicizzato da heap-id.


(heap-empty heap-id)
Questo predicato è vero quando lo heap heap-id non contiene elementi


(heap-not-empty heap-id)
Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.


(heap-head heap-id)
La funzione heap-head ritorna una lista di due elementi dove K è la chiave 
minima e V il valore associato.


(heap-insert heap-id k v)
La funzione heap-insert inserisce l’elemento V nello heap heap-id con chiave K. 
Naturalmente, lo heap heap-id dovrà essere ristrutturato in modo da mantenere 
la “heap property” ad ogni nodo dello heap.

 
(heap-extract heap-id)
La funzione heap-extract ritorna la lista con K, V e con K minima; 
la coppia è rimossa dallo heap heap-id. Naturalmente, lo heap heap-id dovrà 
essere ristrutturato in modo da mantenere la “heap property” 
ad ogni nodo dello heap.


(heap-modify-key heap-id newkey oldkey value)
La funzone heap-modify-key sostituisce la chiave OldKey (associata al valore V) 
con NewKey. Naturalmente, lo heap heap-id dovrà essere ristrutturato in modo da 
mantenere la “heap property” ad ogni nodo dello heap.


(heap-print heap-id)
Questa funzione stampa sulla console lo stato interno dello heap heap-id.


%-----% Funzioni create da noi %-----%

(order-element heap-id)
(swap-element heap-id pos1 pos2)
(swap-other heap-id size)
(order-element-ex heap-id)
(swap-to-nill heap-id pos1 pos2)
(swap-other-ex heap-id initial size)
(find-position list size key value)
(heap-shift list size point)
(heap-shift-left list size point)
(heap-shift-right list size point)

Queste funzioni servono per mantenere la heap property durante le operazioni di
Inserimento, Estrazione e Modifica degli elementi dello Heap.


%------------------------------------------------------------------------------%
% INTERFACCIA LISP PER MINIMUM SPANNING TREE %


(mst-vertex-key graph-id vertex-id)
Questa funzione, dato un vertex-id di un grafo graph-id ritorna, durante e dopo
l’esecuzione dell’algoritmo di Prim, il peso minimo di un arco che connette 
vertex-id nell’albero minimo; se questo arco non esiste 
(ed all’inizio dell’esecuzione) allora k è MOST-POSITIVE-DOUBLE-FLOAT


(mst-previous graph-id vertex-id)
Questa funzione, durante e dopo l’esecuzione dell’algoritmo di Prim, ritorna 
il vertice U che il vertice “genitore” (“precedente”, o “parent”) di V nel 
minimum spanning tree


(mst-prim graph-id source)


% OPERAZIONI PRELIMINARI %

[1] - pulisco la base dati da tutto ciò che riguarda graph-id 
(quindi elimino vertex-key e previous relative a graph-id, 
una cosa simile è già stata implementata per delete-graph, 
quando elimini tutti i vertici e tutti gli archi associati a un grafo);

[2] - rendo per ogni vertice la key INFINITA:
crea una funzione che per ogni elemento presente in *vertices* 
se la (second key) è graph-id crea un vertex-key con la (third key).
Ricordo che i vertici sono fatti così: (VERTEX GRAFO A)
second key è il graph-id e la third key è il nome del vertice;

[3] - stessa cosa, per ogni vertice imposto il previous a NIL;

[4] - elimino la vertex-key riferita a source 
e la ricreo impostandola a 0 al posto che INFINITO

[5] - creo l'heap e lo riempio usando le vertex-key riferite a graph-id

[6] - chiamo il ciclo prim passandogli graph-id e heap-id
      (prim graph-id heap-id)

% FINE OPERAZIONI PRELIMINARI %


% INIZIO CICLO PRIM %

Logica:
WHILE heap non è vuoto
- estraggo elemento U
- metto U nei visited
- FOR ogni vertice V adiacente a U
  - IF V è nell'heap (non è nei visited) AND (peso U-V) < V.key
     - V.previous = U
     - V.key = (peso U-V)

Il codice è diviso in 2 parti:
- CICLO WHILE
- CICLO FOR


% CICLO WHILE %
[1] - Innanzitutto controllo ad ogni ciclo che l'heap non sia vuoto
(if (heap-not-empty heap-id) (part-one graph-id))
--> Se non è vuoto chiamo un'altra funzione passandogli solo il grafo (PART-ONE)

[2] - in PART-ONE viene svolto il seguente ciclo:
1. Imposto una variabile U (con defparameter) con (second (heap-extract heap-id))
   ovvero il nome del vertice in cima all'heap;
2. Imposto una variabile LISTA con (graph-vertex-neighbors graph-id u), dove u è
   il vertice appena estratto nel punto 1;
3. Imposto il vertice U come visited, abbiamo creato una hash table *visited* 
   per tenere conto di tutti i vertici già tenuti in considerazione, esempio:
   (VISITED GRAFO A)
4. Entriamo nel ciclo-for-prim --> (ciclo-for-prim graph-id heap-id lista u)
5. Chiamo ricorsivasmente la funzione (prim graph-id heap-id)

% FINE CICLO WHILE %


% CICLO FOR %

[1] - controlliamo che la lista dei neighbors passata a questa 
funzione non sia vuota.
Se non è vuota allora si può procedere con la funzione 
(ciclo-for-prim-one graph-id heap-id lista u)

[2] - all'interno di ciclo-for-prim-one viene fatto questo controllo:

1. SE il quarto elemento del primo elemento della lista NON è nei *visited*

   (fourth (car lista)) 

   Ricordiamo che la lista in questione è l'output di neighbors, 
   quindi per esempio ((ARC TEST A B 4) (ARC TEST A H 8)) e il quarto
   Elemento di ogni elemento della lista sarebbe il nome del vertice.
   Passato l'IF di questo punto ci sarà un altro IF

2. SE il quinto elemento del primo elemento della lista 
   (quindi il peso dell'arco) è MINORE della key del quarto elemento
   Della lista (quindi il vertice dal quale siamo partiti)
   ALLORA chiamo ciclo-for-prim-two

Is-visited ritorna l'elemento visited con I parametri passati, 
se non esiste continuerà con il ciclo.
mst-vertex-key ritorna la vertex key dell'elemento passatogli in input

3. Fuori dall'if richiamo il ciclo for principale, quindi:
   (ciclo-for-prim graph-id heap-id (cdr lista) u)


[3] - all'interno di ciclo-for-prim-two avviene quello che nello 
pseudocodice avviene nell'if

- V.previous = U
- V.key = (peso U-V)

% FINE CICLO FOR %


(mst-get graph-id source)
Questa funzione ritorna preorder-mst che è una lista degli archi del MST 
ordinata secondo un attraversamento preorder dello stesso, fatta rispetto al 
peso dell’arco. Attenzione che l’albero non è binario e dovete ordinare archi 
con pari peso secondo l’ordinamento “lessicografico” del vertice “target”

%-----% Funzioni create da noi %-----%
(mst-get2 graph-id source mst-final)
(ordina lista)
(preorder-check graph-id vs mst-final)
(preorder graph-id vs mst-final)
(preorder1 graph-id vs mst-final)
(appendi-lista x)
(check-previous graph-id previous vertex lista-prev)
(aggiungi-arco graph-id previous vertex lista-arc)

Noi abbiamo scomposto il codice in diverse funzioni con specifici compiti, 
ma tutte servono solo per ordinare la lista secondo un attraversamento
preorder.


%%% end of file -*- mst.pl