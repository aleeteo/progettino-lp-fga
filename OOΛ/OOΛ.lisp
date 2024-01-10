;;; -*- Mode: Lisp -*-
;;; OOΛ.lisp

(defparameter central-memory (make-hash-table))

;;; def-class definisce la struttura di una classe e la memorizza
;;; in una locazione centralizzata (una variabile globale).
(defun def-class (class-name parents &rest parts)
  (cond ((and (symbolp class-name)
              (listp parents))
         (let (table (make-table parts))
           (setf (gethash class-name central-memory)
                 (cons parents table)))
         class-name)
        (t (error "Errore: classe invalida"))))

;;; make: crea una nuova istanza di una classe.
(defun make (class-name &rest parts)
  (cond (and (symbolp class-name)
             ) ;TODO controllo sul secondo argomento
                                        ;TODO creazione istanza
        (t (error "Error: invalid instance"))))

;;; is-class: restituisce T se l’atomo passatogli `e il nome di una classe.
(defun is-class (class-name)
  (cond ((symbolp class-name)
         ) ;TODO controllo classe
        (t (error "invalid class name"))))

;;; is-instance: restituisce T se l’oggetto passatogli `e l’istanza
;;; di una classe.
(defun is-instance (&optional class-name value))

;;; field: estrae il valore di un campo da una classe.
(defun field (instance field-name))

;;; field*: estrae il valore da una classe percorrendo una catena di attributi.
(defun field* (instance field-name))
;;; end od file -- OOΛ.lisp

;; (defun make-table (list)
;;   (cond ((null (list)) nil)
;;         ((not (symbolp (first list)))
;;          (error "Errore: la chiave non è un simbolo"))
;;         ((null (second list))
;;          (error "Errore: nessun valore da abbinare alla chiave"))
;;         (t
;;          (if (is-method (second (list)))
;;              (append (list
;;                       (cons (first list)
;;                             (proc-method (first list)
;;                                          (second list))))
;;                      (make-table (cddr (list))))
;;              (append (list
;;                       (cons (first list)
;;                             (second list)))
;;                      (make-table (cddr (list))))))))

;; (defun make-table (parts)
;;   (cond ((null parts) nil)
;;         ((not (listp (car parts))) nil)
;;         ((eql (caar parts) 'fields)
;;          (cons (make-table-fields (cdr (car parts)))
;;                (make-table (cdr parts))))
;;         ((eql (caar parts) 'methods)
;;          (cons (make-table (cdr parts))
;;                (make-table-methods (cdr (car parts)))))
;;         (t )))

;;; is-method: controlla se una lista è un metodo (in part methods)
(defun is-method (method*)
  (and (listp method*)
       (symbolp (first method*))
       (listp (second method*))
       (listp (third method*))))

<<<<<<< HEAD
=======
;;; is-class: verifica dato il nome di una classe se essa è un simbolo e se ha una hash table associata ad essa
(defun is-class (class-name)
    (and
        (symbolp class-name)
        (hash-table-p (symbol-value class-name))))

>>>>>>> origin/main
                                        ;DONE create-assoc function
                                        ;DONE is-method function
                                        ;DONE proc-method function
                                        ;TODO part-exist function
                                        ;TODO get-part function
