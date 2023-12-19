;;; -*- Mode: Lisp -*-
;;; OOΛ.lisp

;;; def-class definisce la struttura di una classe e la memorizza
;;; in una locazione centralizzata (una variabile globale).
(defun def-class (class-name parents &rest parts))

;;; make: crea una nuova istanza di una classe.
(defun make (class-name &rest parts))

;;; is-class: restituisce T se l’atomo passatogli `e il nome di una classe.
(defun is-class (class-name))

;;; is-instance: restituisce T se l’oggetto passatogli `e l’istanza
;;; di una classe.
(defun is-instance (&optional class-name value))

;;; field: estrae il valore di un campo da una classe.
(defun field (instance field-name))

;;; field*: estrae il valore da una classe percorrendo una catena di attributi.
(defun field* (instance field-name))
;;; end od file -- OOΛ.lisp
