;;; -*- Mode: Lisp -*-
;;; OOΛ.lisp

(defparameter central-memory (make-hash-table))
;;; def-class definisce la struttura di una classe e la memorizza
;;; in una locazione centralizzata (una variabile globale).
(defun def-class (class-name parents &rest parts)
  (cond ((and (symbolp class-name)
              (listp parents))
         t) ;TODO creazione classe
        (t (error "Error: invalid class"))))

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

                                        ;TODO create-assoc funcion
                                        ;TODO is-method func
                                        ;TODO process-method function
                                        ;TODO part-exist function
                                        ;TODO get-part function
