\> (def-class 'person nil '(fields (name "Eve") (age 21 integer)))\
***PERSON***

\> (def-class 'student '(person) '(fields (name "Eva Lu Ator") (university "Berkeley" string)) '(methods (talk (&optional (out *standard-output*)) (format out "My name is ~A~%My age is ~D~%" (field this 'name) (field this 'age)))))\
***STUDENT***

\> (defparameter eve (make 'person))\
***EVE***

\> (defparameter adam (make 'person 'name "Adam"))\
***ADAM***

\> (defparameter s1 (make 'student 'name "Eduardo De Filippo" 'age 108))\
***S1***

\> (defparameter s2 (make 'student))\
***S2***

\> (defparameter s3 (make 'student 'age "42"))\
***Error in field assignment!***

\> (field eve 'age)\
***21***

\> (field s1 'age)\
***108***

\> (field s2 'name)\
***'Eva Lu Athor'***

\> (field eve 'address)\
***Error: Unknown field***

\> (talk s1)\
***My name is Eduardo De Filippo\
My age is 108\
NIL***

\> (talk eve)\
***Error: Method not found for this instance!***

\> (def-class 'studente-bicocca '(student) '(methods (talk () (format t "Mi chiamo ~A,~%e studio alla Bicocca~%" (field this 'name)))) '(fields (university "UNIMIB")))\
***STUDENTE-BICOCCA***

\> (defparameter ernesto (make 'studente-bicocca 'name "Ernesto"))\
***ERNESTO***

\> (talk ernesto)\
***Mi chiamo Ernesto,\
e studio alla Bicocca\
NIL***

\> (def-class 'using-integers '() '(fields (x 42 integer)))\
***USING-INTEGERS***

\> (def-class 'using-reals '(using-integers) '(fields (x 42.0 real)))\
***Error: not a subtype***
