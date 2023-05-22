(defun findkey (obj key)
  (cdr (assoc key obj)))

(defmacro findkey-ref (obj key)
  `(cdr (assoc ,key ,obj)))

(defmacro make-slots (object)
  (let ((slots (symbol-append object '- 'slots)))
    `(mapcar (lambda (slot) (symbol-append (quote ,object) '- slot)) ,slots)))

(defmacro make-slots-self (object)
  (let ((slots (symbol-append object '- 'slots)))
    `(mapcar (lambda (slot) (symbol-append 'self '- slot)) ,slots)))

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defmacro symbol-append-immediate (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defun obj/type-of (obj)
  (cdr (assoc 'obj/type obj)))

(defun unwrap-and (&rest conds)
  (eval `(and ,@conds)))

(defmacro make-tests (item keys)
  `(apply #'unwrap-and (mapcar
			(lambda (key)
			  (funcall (if (> (length key) 2) (cadr key) #'=)
				   (cdr (assoc (car key) ,item))
				   (caddr key)))
			,keys)))

(defun map-slots (object-name slots)
  (double-mapcar (lambda (slot value) (cons slot (list value)))
		 (mapcar (lambda (slot) (symbol-append 'self '- slot)) slots)
		 (mapcar (lambda (slot) nil) slots)))

(defun map-slots1 (object-name slots)
  (double-mapcar (lambda (slot value) (cons slot (list value)))
		 (mapcar (lambda (slot) (symbol-append 'self '- slot)) slots)
		 (mapcar (lambda (slot) `(cdr (assoc (quote ,slot) self))) slots)))

(defun map-slots2 (object-name slots)
  (double-mapcar (lambda (slot value) `(setf ,slot ,value))
		 (mapcar (lambda (slot) (symbol-append 'self '- slot)) slots)
		 (mapcar (lambda (slot) `(cdr (assoc (quote ,slot) self))) slots)))

(defmacro where (list keys &key test)
  `(remove-if-not (lambda (item)
		    (make-tests item (quote ,keys)))
		  *db*))

(defun double-mapcar (func list1 list2 &key accum1)
  (if list1 (double-mapcar func (cdr list1) (cdr list2)
			   :accum1 (cons (funcall func (car list1) (car list2)) accum1))
      (reverse accum1)))

(defmacro make-object (name keys)
  (let* ((slots (map-slots name keys))
	 (object-slots (symbol-append name '- 'slots))
	 (self-slots `(cons (cons 'cons (cons ''obj/type (list '(quote ,name))))
			    (mapcar (lambda (slot) (cons 'cons (cons
							   `(quote ,slot)
							   (list (symbol-append 'self '- slot)))))
				    ,object-slots))))
    `(progn (defun ,(symbol-append 'make '- name) (&key ,@keys)
	      (cons (cons 'obj/type (quote ,name))
		    (double-mapcar
		     (lambda (key other-key) (cons key other-key))
		     (quote ,keys)
		     (list ,@keys))))
	    (defun ,name (key ,name)
	      (cdr (assoc key ,name)))
	    (defvar ,object-slots (quote ,keys))
	    (defun ,object-slots () (quote ,keys))

	    (defmacro ,(symbol-append name '- 'modify '- 'self) (&rest body)
	      `(let* ((self (progn ,@body)))
		 ,@(map-slots2 (quote ,name) ,object-slots)
		 self))

	    (defmacro ,(symbol-append name '- 'defmethod) (method-name args &rest body)
	      `(progn
		 (defun ,(symbol-append (quote ,name) '- method-name '- 'intern) ,(cons 'self args)
		   (macrolet ((self () '(list ,@,self-slots)))
		     (let* ,(map-slots1 (quote ,name) ,object-slots)
		       ,@body
		       (self))))

		 (defmacro ,(symbol-append (quote ,name) '- method-name) (obj &rest args)
		   (if (not (or (equal obj 'self) (equal obj '(self))))
		       `(setf ,obj (,(symbol-append (quote ,(quote ,name)) '- (quote ,method-name) '- 'intern) ,obj ,@args))
		       `(,(symbol-append (quote ,(quote ,name)) '- 'modify '- 'self)
			 (,(symbol-append (quote ,(quote ,name)) '- (quote ,method-name) '- 'intern)
			  (self) ,@args))))))
	    
	    (mapcar (lambda (var) (eval `(,(symbol-append (quote ,name) '- 'defmethod)
				     ,(symbol-append 'set '- var)
				     (,var) (setf ,(symbol-append 'self '- var) ,var))))
		    ,object-slots))))

;; -----------------------------------------------------------------------------------------------------------

;;(make-object person (name year language))

;; (defvar *db* nil)

;; (defun add-db (person)
;;   (setf *db* (cons person *db*)))

;; (add-db (make-person :name "John" :year 1990 :language "Scheme"))
;; (add-db (make-person :name "Mary" :year 1995 :language "Haskell"))
;; (add-db (make-person :name "Ann"  :year 1993 :language "Scheme"))
;; (add-db (make-person :name "Paul" :year 2000 :language "Python"))
;; (add-db (make-person :name "Amy"  :year 2004 :language "C"))

;; (person-defmethod print-name ()
;; 		  (format t "~a~%" self-name))


;; (mapcar	(lambda (person) (person-print-name person))
;; 	(where *db* ((year < 2000)
;; 		     (language string= "Scheme"))))


