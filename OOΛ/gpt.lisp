(defvar *class-table* (make-hash-table :test 'equal))

(defun def-class (class-name parents &rest parts)
  (let ((class-fields (process-fields (find-part 'fields parts)))
        (class-methods (process-methods (find-part 'methods parts))))
    (setf (gethash class-name *class-table*)
          (list (cons 'name class-name)
                (cons 'parents parents)
                (cons 'fields class-fields)
                (cons 'methods class-methods)))))

(defun find-part (part-type parts)
  (assoc part-type parts))

(defun process-fields (field-part)
  (cond ((null field-part) nil)
        (t (mapcar (lambda (field)
                     (let ((field-name (first field))
                           (default-value (second field))
                           (type (third field)))
                       (list :name field-name :default default-value :type type)))
                   (cdr field-part)))))

(defun process-methods (method-part)
  (cond ((null method-part) nil)
        (t (mapcar (lambda (method)
                     (cond ((not (valid-method-structure method))
                            (error "Invalid method structure: ~a" method))
                           ((not (valid-method-name (first method)))
                            (error "Invalid method name: ~a" (first method)))
                           (t (let ((method-name (first method))
                                    (method-body (second method)))
                                (list :name method-name :body (eval method-body))))))
                   (cdr method-part)))))

(defun valid-method-structure (method)
  (and (listp method)             ; Verifica che sia una lista
       (= (length method) 2)       ; Verifica che abbia esattamente due elementi
       (listp (second method))))   ; Verifica che il secondo elemento sia una lista (presumibilmente una lambda expression)

(defun valid-method-name (name)
  (symbolp name))  ; Verifica che il nome sia un simbolo
