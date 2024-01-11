(defparameter classes-specs (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name classes-specs) class-spec))

(defun class-spec (name)
  (gethash name classes-specs))


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

(defun get-methods (method-part)
  (cond
    ((null method-part) NIL)
    (t (mapcar (lambda (method)
                 (cond
                   ((not (valid-method-structure method))
                    (error "ERROR: invalid method structure ~a" method))
                   ((not (symbolp (first method)))
                    (error "ERROR: invalid method name ~a" method))
                   (t (let ((method-name (first method))
                            (method-body (rest method)))
                        (list :name method-name
                              :body (eval method-body))))))
               (cdr method-part)))))

(defun valid-method-structure (method)
  (and (listp method)
       (= (length method) 2)
       (listp (second method))))

;;; make consente di creare un'istanza di una classe
(defun make (class-name &rest parts)
  (cond
    ((is-class class-name)
     (let ((class-specs (class-spec class-name)))
       (print parts)
       (if (verify-instance-fields class-specs parts)
           (append (list 'oolinst)
                   (list :class class-name :fields parts)))))
    (t (error "error: given class not valid"))))

;;; is-class verifica che il nome della classe
;;; passata sia di una classe presente in memoria
(defun is-class (class-name)
  (and (symbolp class-name)
       (gethash class-name classes-specs)))

(defun verify-instance-fields (class-specs fields)
  (cond
    ((null fields) t) ;;da rimettere nil
    ((not (null (cdr (third class-specs))))
     (let ((class-fields (cdr (third class-specs))))
       (if (deep-member (first fields) class-fields)
           (verify-instance-fields class-specs (nthcdr 2 fields)))))
    (t (error "error: no field in class"))))

(defun deep-member (atomo lista)
  (cond ((null lista) nil) ; caso base: lista vuota
        ((eq atomo (car lista)) t) ; atomo trovato
        ((listp (car lista)) (or (deep-member atomo (car lista)) ; ricerca ricorsiva nella sottolista
                                 (deep-member atomo (cdr lista)))) ; continua nella lista principale
        (t (deep-member atomo (cdr lista))))) ; continua nella lista principale

;;tests
(def-class 'person nil '(fields (name "Eve") (age 21 integer)))
(defparameter adam (make 'person 'name "Adam"))
(print adam)
