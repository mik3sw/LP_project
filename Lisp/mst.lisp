; -*- Mode: Common Lisp -*-

;Progetto realizzato da: 
;- Michele Angelo Marcucci   [851905]
;- Davide Mazzitelli         [851657]


(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
  (if(gethash graph-id *graphs*)
      graph-id))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))


(defun delete-graph (graph-id)
  (elimina-vertici-archi graph-id)
  (if (remhash graph-id *graphs*) nil))
(defun elimina-vertici-archi (graph-id)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *vertices*))) 
                        *vertices*)) nil)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *arcs* ))) 
                        *arcs*)) nil)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *mst-vertex-key* ))) 
                        *mst-vertex-key*)) nil)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *mst-previous* ))) 
                        *mst-previous*)) nil))


(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id)
                 *vertices*)
  (list 'vertex graph-id vertex-id)))


(defun graph-vertices (graph-id)
  (crea-lista-vertici graph-id NIL))
(defun crea-lista-vertici (graph-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (push key lista))) *vertices*)) lista))


(defun new-arc (graph-id vertex1-id vertex2-id &optional (weight 1))
  (if (not (esiste-vertice graph-id vertex1-id))
      (new-vertex graph-id vertex1-id))
  (if (not (esiste-vertice graph-id vertex2-id))
      (new-vertex graph-id vertex2-id))
  (setf (gethash (list 'arc graph-id vertex2-id vertex1-id weight)
                 *arcs*)
        (list 'arc graph-id vertex2-id vertex1-id weight))
  (setf (gethash (list 'arc graph-id vertex1-id vertex2-id weight)
                 *arcs*)
        (list 'arc graph-id vertex1-id vertex2-id weight)))


(defun esiste-vertice (graph-id vertex-id)
  (crea-lista-esiste-vertice graph-id vertex-id NIL))
(defun crea-lista-esiste-vertice (graph-id vertex-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                     (eql vertex-id (third key)))
                                (push key lista))) *vertices*)) lista))


(defun graph-arcs (graph-id)
  (crea-lista-archi graph-id NIL))
(defun crea-lista-archi (graph-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (push key lista))) *arcs*)) lista))


(defun graph-vertex-neighbors (graph-id vertex-id)
  (crea-lista-neighbors graph-id vertex-id NIL))
(defun crea-lista-neighbors (graph-id vertex-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                     (eql vertex-id (third key)))
                                (push key lista))) *arcs*)) lista))


(defun remove-weight(lista)
    (reverse (cdr (reverse lista))))
(defun graph-vertex-adjacent (graph-id vertex-id)
  (crea-lista-adjacent graph-id vertex-id NIL))
(defun crea-lista-adjacent (graph-id vertex-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                     (eql vertex-id (third key)))
                                (push (remove-weight key) lista))) 
                        *arcs*)) lista))


(defun graph-print (graph-id)
  (maphash
   (lambda (keys values)
     (cond
      ((equal (second keys) graph-id)
       (format t "Vertice -->  ~S~%" keys)))) 
   *vertices*)
  (maphash
   (lambda (keys values)
     (cond
      ((equal (second keys) graph-id)
       (format t "Arco -->  ~S~%" keys)))) 
   *arcs*))

(defun graph-size (graph-id)
  (defparameter graphsize 0)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (defparameter graphsize (+ 1 graphsize)))) 
                        *vertices*)) graphsize))

  


(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))
    (second temp)))

(defun heap-size (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))
    (third temp)))

(defun heap-actual-heap (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))
    (fourth temp)))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  (let ((temp
         (gethash heap-id *heaps*)))
    (cond 
     ((= (third temp) 0) 
      t))))

(defun heap-not-empty (heap-id)
  (let ((temp
         (gethash heap-id *heaps*)))
    (cond ((/= (third temp) 0) 
           t))))

(defun heap-head (heap-id)
  (aref (heap-actual-heap heap-id) 0))

(defun heap-print (heap-id)
  (print (gethash heap-id *heaps*)) t)

(defun heap-capacity (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))
    (length(fourth temp))))

; Inserimento elemento
(defun heap-insert (heap-id key value)
  (let ((cap (heap-capacity heap-id))
	(size (heap-size heap-id))
        (list (gethash heap-id *heaps*)))
    (cond
     ((< size cap)
      (progn 
        (setf 
         (aref (fourth list) size) 
         (list key value))
        (setf 
         (third list) 
         (+ (third list) 1))
        (order-element heap-id)))
     (T (error "Full")))))


(defun order-element (heap-id)
  (let ((size 
         (heap-size heap-id)))
    (cond
     ((= size 1) t)
     ((= size 2) (swap-element heap-id 1 0))
     ((> size 2) (swap-other heap-id size)))))


(defun swap-element (heap-id pos1 pos2)
  (let ((list 
         (heap-actual-heap heap-id)))
    (let ((first-el (first (aref list pos2)))
	  (second-el (first (aref list pos1))))
      (cond
       ((< second-el first-el)
	(let ((temp (aref list pos2)))
	  (setf 
           (aref list pos2) 
           (aref list pos1))
	  (setf 
           (aref list pos1) 
           temp)))))))


(defun swap-other (heap-id size)
  (cond
   ((= size 1) t)
   (t
    (swap-element heap-id (- size 1) (- size 2))
    (swap-other heap-id (- size 1)))))



(defun heap-extract (heap-id)
  (let ((list 
         (heap-actual-heap heap-id))
        (l-for-decrease 
         (gethash heap-id *heaps*)))
    (let ((kval 
           (aref list 0)))
      (progn
        (setf 
         (aref list 0) 
         nil)
        (setf 
         (third l-for-decrease) 
         (- (third l-for-decrease) 1))
        (order-element-ex heap-id)
        kval))))


(defun order-element-ex (heap-id)
  (let ((size 
         (heap-size heap-id)))
    (cond
     ((= size 0) t)
     ((= size 1) (swap-to-nill heap-id 1 0))
     ((> size 1) (swap-other-ex heap-id 0 size)))))


(defun swap-to-nill (heap-id pos1 pos2)
  (let ((list 
         (heap-actual-heap heap-id)))
    (setf 
     (aref list pos2) 
     (aref list pos1))
    (setf 
     (aref list pos1) 
     nil)))


(defun swap-other-ex (heap-id initial size)
  (cond
   ((= initial size) t)
   (t
    (swap-to-nill heap-id (+ initial 1) initial)
    (swap-other-ex heap-id (+ initial 1) size))))




(defun heap-modify-key (heap-id new-key old-key value)
  (let ((list (heap-actual-heap heap-id))
	(position (find-position
                  (heap-actual-heap heap-id)
                  (- (heap-size heap-id) 1)
                  old-key
                  value)))
    (cond
     ((equal position nil) nil)
     (t
      (setf 
       (aref list position) 
       (list new-key value))
      (heap-shift list (- (heap-size heap-id) 1) position)))))


(defun find-position (list size key value)
  (cond
   ((< size 0) nil)
   (t
    (let ((l-key (first (aref list size)))
          (l-value (second (aref list size))))
      (cond
       ((and (= l-key key)(eql l-value value)) size)
       (t 
        (find-position list (- size 1) key value)))))))


(defun heap-shift (list size point)
  (cond
   ((= point 0) (heap-shift-right list size point))
   ((= point size) (heap-shift-left list size point))
   (t
    (heap-shift-right list size point)
    (heap-shift-left list size point))))


(defun heap-shift-right (list size point)
  (cond
   ((= size point) t)
   ((> (first(aref list point)) (first(aref list (+ point 1))))
    (let ((temp 
           (aref list point)))
      (setf 
       (aref list point) 
       (aref list (+ point 1)))
      (setf 
       (aref list (+ point 1)) 
       temp)
      (heap-shift-right list size (+ point 1))))))


(defun heap-shift-left (list size point)
  (cond
   ((= point 0) t)
   ((< (first(aref list point)) (first(aref list (- point 1))))
    (let ((temp 
           (aref list point)))
      (setf 
       (aref list point) 
       (aref list (- point 1)))
      (setf 
       (aref list (- point 1)) 
       temp)
      (heap-shift-left list size (- point 1))))))





(defparameter *mst-vertex-key* (make-hash-table :test #'equal))
(defparameter *mst-previous* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))


(defun mst-prim (graph-id source) 
  (clear-data graph-id)  
  (imposta-key graph-id) 
  (imposta-previous graph-id) 
  (imposta-source-key graph-id source) 
  (crea-heap-prim 'codaminprio graph-id) 
  (prim graph-id 'codaminprio))



(defun clear-data (graph-id) 
  (clear-key graph-id)
  (clear-previous graph-id)
  (clear-visited graph-id))
(defun clear-key (graph-id)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *mst-vertex-key*))) 
                        *mst-vertex-key*)) nil))

(defun clear-previous (graph-id) 
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *mst-previous*))) 
                        *mst-previous*)) nil))

(defun clear-visited (graph-id) 
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (remhash key *visited*))) 
                        *visited*)) nil))


(defun imposta-key (graph-id) 
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (setf (gethash 
                                       (list 'vertex-key graph-id 
                                             (third key) 
                                             MOST-POSITIVE-DOUBLE-FLOAT) 
                                       *mst-vertex-key*) 
                                     (list 'vertex-key graph-id 
                                           (third key) 
                                           MOST-POSITIVE-DOUBLE-FLOAT))))
                              *vertices*)) nil))


(defun imposta-previous (graph-id)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                               (setf (gethash 
                                      (list 
                                       'previous graph-id 
                                       (third key) nil) 
                                      *mst-previous*) 
                                     (list 'previous 
                                           graph-id (third key) nil))))
                              *vertices*)) nil))

;; [4] Imposto la source-key a 0
(defun imposta-source-key (graph-id source)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key)) 
                                     (eql source (third key)))
                               (remhash key *mst-vertex-key*)))
                              *mst-vertex-key*)) nil)
  (if (setf (gethash (list 'vertex-key graph-id source 0) 
                     *mst-vertex-key*) 
            (list 'vertex-key graph-id source 0)) nil))


(defun crea-heap-prim (heap-id graph-id)
  (new-heap heap-id)
  (heap-delete heap-id)
  (graph-size graph-id)
  (new-heap heap-id graphsize)
  (riempi-heap heap-id graph-id))
(defun riempi-heap (heap-id graph-id)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (eql graph-id (second key))
                                (heap-insert 
                                 'codaminprio 
                                 (fourth key) 
                                 (third key)))) 
                        *mst-vertex-key*)) nil))



(defun prim (graph-id heap-id)
  (if (heap-not-empty 'codaminprio) 
      (part-one graph-id)))

(defun part-one (graph-id)
  (defparameter u (second (heap-extract 'codaminprio))) 
  (defparameter lista (graph-vertex-neighbors graph-id u))
  (imposta-visited graph-id u)
  (ciclo-for-prim graph-id 'codaminprio lista u)
  (prim graph-id 'codaminprio))

(defun imposta-visited (graph-id vertex-id)
  (if (setf (gethash (list 'visited graph-id vertex-id) *visited*) 
                                     (list 'visited graph-id vertex-id)) nil))

(defun ciclo-for-prim (graph-id heap-id lista u)
  (if (not (null lista))
      (ciclo-for-prim-one graph-id heap-id lista u)))

(defun ciclo-for-prim-one (graph-id heap-id lista u)
  (if (not (is-visited graph-id (fourth (car lista)))) 
          (if (< (fifth (car lista)) 
                 (mst-vertex-key graph-id (fourth (car lista)))) 
              (ciclo-for-prim-two graph-id lista u)))
  (ciclo-for-prim graph-id heap-id (cdr lista) u))

(defun ciclo-for-prim-two (graph-id lista u)
  (imposta-new-peso graph-id (fifth (car lista)) 
                    (fourth (car lista)) 'codaminprio)
  (imposta-new-previous graph-id (fourth (car lista)) u))



(defun is-visited (graph-id vertex-id)
  (crea-lista-visited graph-id vertex-id NIL))
(defun crea-lista-visited (graph-id vertex-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                    (eql vertex-id (third key)))
                                (push key lista))) *visited*)) lista))

(defun mst-vertex-key (graph-id vertex-id)
  (first (crea-lista-get-key graph-id vertex-id NIL)))
(defun crea-lista-get-key (graph-id vertex-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                    (eql vertex-id (third key)))
                                (push (fourth key) lista))) 
                        *mst-vertex-key*)) lista))

(defun mst-previous (graph-id vertex-id)
  (first (crea-lista-mst-previous graph-id vertex-id NIL)))
(defun crea-lista-mst-previous (graph-id vertex-id lista)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                    (eql vertex-id (third key)))
                                (push (fourth key) lista))) 
                        *mst-previous*)) lista))


(defun imposta-new-previous (graph-id vertex-id u)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                    (eql vertex-id (third key)))
                                (remhash key *mst-previous*))) 
                        *mst-previous*)) nil)
  (if (setf (gethash (list 'previous graph-id vertex-id u) 
                     *mst-previous*) 
            (list 'previous graph-id vertex-id u)) nil))

(defun imposta-new-peso (graph-id peso vertex-id heap-id)
  (heap-modify-key heap-id peso (mst-vertex-key graph-id vertex-id) vertex-id)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                    (eql vertex-id (third key)))
                                (remhash key *mst-vertex-key*))) 
                        *mst-vertex-key*)) nil)
  (if (setf (gethash (list 'vertex-key graph-id vertex-id peso) 
                     *mst-vertex-key*)
            (list 'vertex-key graph-id vertex-id peso)) nil))



(defun mst-get (graph-id source)
  (defparameter lista-suprema NIL)
  (mst-get2 graph-id source NIL)
  (reverse lista-suprema))

(defun mst-get2 (graph-id source mst-final)
  (preorder-check graph-id (ordina 
                            (graph-vertex-neighbors graph-id source)) 
                  mst-final))

(defun ordina (lista)
  (sort lista #'string<= :key #'fourth)
  (sort lista  #'< :key #'fifth))
  
(defun preorder-check (graph-id vs mst-final)
  (if (not (null vs))
      (preorder graph-id vs mst-final)))

(defun preorder (graph-id vs mst-final)
  (if 
      (and (check-previous graph-id (third (car vs)) (fourth (car vs)) NIL) 
           (not (check-previous graph-id (fourth (car vs)) (third (car vs)) 
                                NIL)))
      (preorder1 graph-id vs mst-final))
  (if (check-previous graph-id (fourth (car vs)) (third (car vs)) NIL)
      (preorder-check graph-id (cdr vs) mst-final))
  (if (not (check-previous graph-id (third (car vs)) (fourth (car vs)) NIL))
      (preorder-check graph-id (cdr vs) mst-final)))


(defun preorder1 (graph-id vs mst-final)
  (appendi-lista (aggiungi-arco graph-id (third (car vs)) (fourth (car vs)) 
                                NIL))
  (preorder-check graph-id (ordina 
                            (graph-vertex-neighbors graph-id 
                                                    (fourth (car vs)))) NIL)
  (preorder-check graph-id (cdr vs) NIL))

(defun appendi-lista (x)
  (if (not (find (first x) lista-suprema))
      (push (first x) lista-suprema)))

(defun check-previous (graph-id previous vertex lista-prev)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                     (and 
                                      (eql previous (fourth key))
                                      (eql vertex (third key))))
                                (push key lista-prev))) 
                        *mst-previous*)) lista-prev))

(defun aggiungi-arco (graph-id previous vertex lista-arc)
  (if (eql NIL (maphash #'(lambda (key val)
                            (if (and (eql graph-id (second key))
                                     (and 
                                      (eql previous (third key))
                                      (eql vertex (fourth key))))
                                (push key lista-arc))) *arcs*)) lista-arc))

; -*- end of file mst.lisp -*-
