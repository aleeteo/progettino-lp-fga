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
