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
     (let ((class-specs (class-spec class-name)))
       (if (verify-instance-fields class-specs parts)
           (append (list 'oolinst)
                   (list :class class-name :fields parts)))))
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
            (t (error "ERROR: given value is not an instance of the specified class")))
      (error "ERROR: given value can't be an instance, as value is not a list")))

;; field retituisce il valore del campo field-name nell'istanza instance
(defun field (instance field-name)
  (cond
    ((not (is-instance instance)) nil)
    ((not (symbolp field-name)) nil)
    (t (if (deep-member field-name (fifth instance))
           (find-field field-name (fifth instance))
           (find-field field-name
                       (cdr (third (class-spec (third instance)))))))))

(defun field* (instance &rest field-names)
  (cond ((not (is-instance instance)) (error "ERROR: Not an instance"))
        ((null field-names) (error "No field names provided"))
        (t (reduce (lambda (obj field-name)
                     (if (listp obj)
                         (field obj field-name)
                         (error "Intermediate value is not an instance")))
                   field-names
                   :initial-value instance))))

;;; FUNZIONI AGGIUNTIVE

;;; get-fields restituisce una lista formattata
;;; con chiavi di fields
(defun get-fields (field-part)
  (cond
    ((null field-part) NIL)
    (t (mapcar (lambda (field)
                 (let (
                       (field-name (first field))
                       (field-value (second field))
                       (field-type (if (not (null (third field)))
                                       (third field)
                                       NIL)))
                   (list :name field-name
                         :value field-value
                         :type field-type)))
               (cdr field-part)))))

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
(defun verify-instance-fields (class-specs fields)
  (cond
    ((null fields) t)
    ((not (null (cdr (third class-specs))))
     (let ((class-fields (cdr (third class-specs))))
       (if (deep-member (first fields) class-fields)
           (verify-instance-fields class-specs (nthcdr 2 fields)))))
    (t (error "error: no field in class"))))

;;; deep-member verifica che un elemento passato
;;; sia contenuto all'interno di una lista passata
(defun deep-member (atomo lista)
  (cond ((null lista) nil) ; caso base: lista vuota
        ((eq atomo (car lista)) t) ; atomo trovato
        ((listp (car lista)) (or (deep-member atomo (car lista)) ; ricerca ricorsiva nella sottolista
                                 (deep-member atomo (cdr lista)))) ; continua nella lista principale
        (t (deep-member atomo (cdr lista))))) ; continua nella lista principale

;; find-field è una funzione di supporto per field
;; e si occupa di trovare e restituire il valore del campo field
;; se il campo non è presente restituisce nil
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

;;; TESTS
;; (def-class 'person nil '(fields (name "Eve") (age 21 integer)))
(def-class 'address nil '(fields (city "Unknown City") (street "Unknown Street")))
(def-class 'person nil '(fields (name "Unknown") (address (make 'address))))

(defparameter person-instance (make 'person 'name "Alice" 'address (make 'address 'city "Wonderland" 'street "Rabbit Hole Lane")))

(field* person-instance 'address 'city)   ; Restituisce "Wonderland"
(field* person-instance 'address 'street) ; Restituisce "Rabbit Hole Lane"

;;;; end of file -- ool.lisp
