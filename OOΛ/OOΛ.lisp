
;;;; -*- Mode: Lisp -*-
;;;; Melon Cristiano 899647
;;;; Teodori Alessandro 899894
;;;; ool.lisp

(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

;;; def-class consente di creare
;;; e inserire in memoria una classe
(defun def-class (class-name parents &rest part)
  (cond
    ((and (symbolp class-name) (listp parents))
     (let ((fields (get-fields (assoc 'fields part)))
           (methods (get-methods (assoc 'methods part))))
       (add-class-spec class-name (list
                                   (cons 'name class-name)
                                   (cons 'parents parents)
                                   (cons 'fields fields)
                                   (cons 'methods methods))))
     class-name)
    (t (error "ERROR: class-name or parents not valid!"))))

;;; make consente di creare un'istanza di una classe
(defun make (class-name &rest parts)
  (cond
    ((is-class class-name)
     (if (verify-instance-fields parts class-name)
         (append (list 'oolinst)
                 (list :class class-name :fields parts))))
    (t (error "ERROR: given class not valid"))))

;;; is-class verifica che il nome della classe
;;; passata sia di una classe presente in memoria
(defun is-class (class-name)
  (and (symbolp class-name)
       (gethash class-name *classes-specs*)))

;;; is-insnace verifica che l'oggetto passato sia una
;;; istanza di una classe
(defun is-instance (value &optional (class-name T))
  (if (listp value)
      (cond ((and (equal (car value) 'OOLINST)
                  (equal class-name 'T)) T)
            ((and (equal (car value) 'OOLINST)
                  (equal (third value) class-name)) T)
            ((deep-member class-name (second (class-spec (third value)))) T)
            (t (error "ERROR: given value not an instance of the class")))
      (error "ERROR: given value can't be an instance, value is not a list")))

;;; field prende come argomenti una istanza e il nome
;;; di un campo dell'istanza e ne restituisce il valore
(defun field (instance field-name)
  (cond
    ((not (is-instance instance)) nil)
    ((not (symbolp field-name)) nil)
    (t (if (deep-member field-name (fifth instance))
           (find-field field-name (fifth instance))
           (car (list (field-class field-name (third instance))))))))

;;; field* estrae il valore da una classe percorrendo
;;; una catena di attributi
(defun field* (instance &rest field-name)
  (cond ((null field-name) (error "ERROR: list is empty!"))
        ;; ((is-instance
        ;;         (field instance (if (listp (car field-name))
        ;;                             (caar field-name) (car field-name)))))
        ((eq (length field-name) 1)
         (field instance (if (listp (car field-name))
                             (caar field-name) (car field-name))))
        (T (field* (field instance (if (listp (car field-name))
                                       (caar field-name) (car field-name)))
                   (cdr field-name)))))

;;; FUNZIONI AGGIUNTIVE

;;; get-fields restituisce una lista formattata
;;; con chiavi di fields
(defun get-fields (field-part)
  (cond
    ((null field-part) NIL)
    (t (mapcar (lambda (field)
                 (let ((field-name (first field))
                       (field-value (second field))
                       (field-type (if (not (null (third field)))
                                       (third field)
                                       NIL)))
                   (if (or (null field-type) (type-check field-value field-type))
                       (list :name field-name
                             :value field-value
                             :type field-type)
                       (error "Type check failed for field ~a" field-name))))
               (cdr field-part)))))

;;; typecheck verifica il tipo di una field, controllando che il value sia
;;; uguale o sottotipo del tipo specificato nella field
(defun type-check (value expected-type)
  (cond ((null expected-type) t)
        (t (let ((actual-type (type-of value)))
             (first (list (subtypep actual-type expected-type)))))))


;;; get-methods restituisce una lista formattata
;;; con chiavi di metodi
(defun get-methods (method-part)
  (cond
    ((null method-part) NIL)
    (t (mapcar (lambda (method)
                 (cond
                   ((not (valid-method-structure (cdr method)))
                    (error "ERROR: invalid method structure ~a" method))
                   ((not (symbolp (first method)))
                    (error "ERROR: invalid method name ~a" method))
                   (t (let ((method-name (first method))
                            (method-body (cdr method)))
                        (list :name method-name
                              :body (eval method-body))))))
               (cdr method-part)))))

;;; valid-method-structure verifica che la struttura
;;; di un metodo sia corretta
(defun valid-method-structure (method)
  (and (listp method)
       (= (length method) 2)
       (listp (second method))))

;;; verify-instance-fields verifica che i fields di
;;; una istanza siano gli stessi della classe istanziata
(defun verify-instance-fields (fields class-name)
  (cond ((null fields) t)
        (t (if (not (null (field-class (first fields) class-name)))
               (if (type-check (second fields)
                               (first
                                (list (class-field-type
                                       (first fields) class-name))))
                   (verify-instance-fields (nthcdr 2 fields) class-name)
                   (error "ERROR: type of ~a field not valid" (first fields)))
               (error "ERROR: class doesn't have ~a field" (first fields))))))

;;; deep-member verifica che un elemento passato
;;; sia contenuto all'interno di una lista passata
(defun deep-member (atomo lista)
  (cond ((null lista) nil) ; caso base: lista vuota
        ((eq atomo (car lista)) t) ; atomo trovato
        ((listp (car lista)) (or (deep-member atomo (car lista)) ; ricerca ricorsiva nella sottolista
                                 (deep-member atomo (cdr lista)))) ; continua nella lista principale
        (t (deep-member atomo (cdr lista))))) ; continua nella lista principale

;;; field-class ha la stessa funzione di field ma sulle classi
(defun field-class (field-name class-name)
  (let ((class-fields (rest (third (class-spec class-name)))))
    (if (deep-member field-name class-fields)
        (find-field field-name class-fields)
        (values-list
         (remove nil
                 (mapcar
                  (lambda (p) (field-class field-name p))
                  (second (class-spec class-name))))))))

;;; find-field prende come argomenti un field-name
;;; e una lista di fields in cui cercare quel campo
(defun find-field (field-name fields)
  (if (null fields)
      nil
      (if (listp (car fields))
          (if (equal field-name (second (car fields)))
              (fourth (car fields))
              (find-field field-name (cdr fields)))
          (if (equal field-name (first fields))
              (second fields)
              (find-field field-name (cddr fields))))))



;;; class-field-type funziona come field-class, ma richiama una variante di
;;; finde-field che restituisce solamente il type del dato field
(defun class-field-type (field-name class-name)
  (let ((class-fields (rest (third (class-spec class-name)))))
    (if (deep-member field-name class-fields)
        (find-field-type field-name class-fields)
        (values-list
         (remove nil
                 (mapcar
                  (lambda (p) (class-field-type field-name p))
                  (second (class-spec class-name))))))))

;;; find-field-type prende come argomenti un field-name e una lista
;;; di fields in cui cercare il dato field e restituirne il tipo
(defun find-field-type (field-name fields)
  (if (null fields)
      nil
      (if (listp (car fields))
          (if (equal field-name (second (car fields)))
              (sixth (car fields))
              (find-field-type field-name (cdr fields))))))

;;; TESTS
;;(def-class 'person nil '(fields (name "Eve") (age 21 integer)))
;;(defparameter adam (make 'person 'name "Adam" 'age 21))
;;(print adam)
;;(field adam 'age)


;; (def-class 'address nil '(fields (city "Unknown City") (street "Unknown Street")))
;; (def-class 'person nil '(fields (name "Unknown") (address (make 'address))))

;; (defparameter person-instance (make 'person 'name "Alice" 'address (make 'address 'city "Wonderland" 'street "Rabbit Hole Lane")))

;; (field* person-instance 'address 'city)   ; Restituisce "Wonderland"
;; (field* person-instance 'address 'street) ; Restituisce "Rabbit Hole Lane"

(def-class 'person nil '(fields (name "Eve" string)))
(def-class 'c1 '(person) '(fields (age 22)))
(def-class 'c2 '(c1) '(fields (city "Legnano")))
(def-class 'c3 nil '(fields (state "Italy") (name "John") (band "AC/DC")))
(def-class 'c4 '(c2 c3) '(fields (sex "female")))

(defparameter inst (make 'c4))

;;;; end of file -- ool.lisp
