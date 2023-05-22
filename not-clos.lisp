(defpackage :not-clos
  (:use :cl)
  (:nicknames :nclos)
  (:export
   #:where
   #:make-object
   #:self))

(in-package :not-clos)

;; Make the object slots (symbol-name (car 
;; '(<object>-slot <object>-slot0)
(defmacro make-slots (object)
  (let ((slots (symbol-append object '- 'slots)))
    `(mapcar (lambda (slot) (symbol-append (quote ,object) '- slot)) ,slots)))

;; Make self-referencing slots
;; '(self-slot self-slot0)
(defmacro make-slots-self (object)
  (let ((slots (symbol-append object '- 'slots)))
    `(mapcar (lambda (slot) (symbol-append 'self '- slot)) ,slots)))

;; Append multiple symbols, abused for macros
(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

;; Get type of object
(defun obj/type-of (obj)
  (cdr (assoc 'obj/type obj)))

;; Allow applying 'and macro on a list
;; a little ugly
(defun unwrap-and (&rest conds)
  (eval `(and ,@conds)))

;; Create test string for the where macro
(defmacro make-tests (item keys)
  `(apply #'unwrap-and (mapcar
			(lambda (key)
			  (funcall (if (> (length key) 2) (cadr key) #'=)
				   (cdr (assoc (car key) ,item))
				   (caddr key)))
			,keys)))

;; Create a list of '((self-slot nil) (self-slot0 nil)) from '(slot slot0)
(defun map-slots (object-name slots)
  (double-mapcar (lambda (slot value) (cons slot (list value)))
		 (mapcar (lambda (slot) (symbol-append 'self '- slot)) slots)
		 (mapcar (lambda (slot) nil) slots)))

;; Same as above but initializes values to match associated object; used for let bindings
;; '((self-slot) (cdr (assoc 'slot self)))
(defun map-slots1 (object-name slots)
  (double-mapcar (lambda (slot value) (cons slot (list value)))
		 (mapcar (lambda (slot) (symbol-append 'self '- slot)) slots)
		 (mapcar (lambda (slot) `(cdr (assoc (quote ,slot) self/internal))) slots)))

;; Generates setf list
;; '((setf self-slot  (cdr (assoc 'slot  self)))
;;   (setf self-slot0 (cdr (assoc 'slot0 self))))
(defun map-slots2 (object-name slots)
  (double-mapcar (lambda (slot value) `(setf ,slot ,value))
		 (mapcar (lambda (slot) (symbol-append 'self '- slot)) slots)
		 (mapcar (lambda (slot) `(cdr (assoc (quote ,slot) (car self/internal)))) slots)))

;; Find objects by key conditions in object list
;; (where list ((value1 > 2000)
;;              (value2 string= "test")))
(defmacro where (list keys &key test)
  `(remove-if-not (lambda (item)
		    (make-tests item (quote ,keys)))
		  ,list))

;; Map to 2 lists and return a merge
;; (double-mapcar (lambda (first second)) first second)
(defun double-mapcar (func list1 list2 &key accum1)
  (if list1 (double-mapcar func (cdr list1) (cdr list2)
			   :accum1 (cons (funcall func (car list1) (car list2)) accum1))
      (reverse accum1)))

;; Object creation macro
(defmacro make-object (name keys)
  (let* ((slots (map-slots name keys))
	 (object-slots (symbol-append name '- 'slots))
	 (self-slots `(cons (cons 'cons (cons ''obj/type (list '(quote ,name))))
			    (mapcar (lambda (slot) (cons 'cons (cons
							   `(quote ,slot)
							   (list (symbol-append 'self '- slot)))))
				    ,object-slots))))
    `(progn

       ;; obj/type defines the type as a symbol
       ;; make-<object>
       (defun ,(symbol-append 'make '- name) (&key ,@keys)
	 (cons (cons 'obj/type (quote ,name))
	       (double-mapcar
		(lambda (key other-key) (cons key other-key))
		(quote ,keys)
		(list ,@keys))))

       ;; Create <object>-slots as well as its function
       (defvar ,object-slots (quote ,keys))
       (defun ,object-slots () (quote ,keys))


       ;; modify-self macro; sets self- variables to returned value; for internal use
       ;; should only be used with matching methods
       ;;obj/<object>-modify-self
       (defmacro ,(symbol-append 'obj/ name '-modify-self) (&rest body)
	 `(let* ((self/internal (progn ,@body)))
	    ,@(map-slots2 (quote ,name) ,object-slots)
	    self/internal))

       ;; <object>-defmethod
       ;; Creates an internal function and an invoker macro
       ;; The invoker macro wraps around the function and applies its return values,
       ;;   either over the object which was invoked or 'self if inside method.

       ;;<object>-defmethod
       (defmacro ,(symbol-append name '- 'defmethod) (method-name args &rest body)
	 `(progn

	    ;; Internal method function
	    ;; Wraps around the method body; assigns 'self- variables with a let*,
	    ;;   assigns the (self) macro.
	    ;; (self) produces an object from the local 'self- variables
	    ;;obj/<object>-<method-name>-intern (self args)
	    (defun ,(symbol-append 'obj/ (quote ,name) '- method-name '-intern) ,(cons 'self/internal args)
	      (macrolet ((self () '(list ,@,self-slots))) ;; (self) macro
		            ;; bind self to this package so symbol comparisons work
		(let* ,(cons '(self nil) (map-slots1 (quote ,name) ,object-slots)) ;; 'self-* bindings
		  (let ((res/internal (progn ,@body)))
		    `(,(self) . ,(if res/internal res/internal '(nil))))))) ;; call (self) to return new object

	    ;; Invoker macro
	    ;;<object>-<method-name> (obj &rest args)
	    (defmacro ,(symbol-append (quote ,name) '- method-name) (obj &rest args)
	      (if (not (eq obj 'self)) ;; Check the 'self special case

		  ;; Wrap with standard setf
		  `(let ((obj/result
			   (,(symbol-append 'obj/ (quote ,(quote ,name)) '- (quote ,method-name) '- 'intern) ,obj ,@args)))
		     (setf ,obj (car obj/result)) ;;(car obj/result))
		     (cdr obj/result))

		  ;; Wrap with modify-self (when 'self is used)
		  `(cdr (,(symbol-append 'obj/ (quote ,(quote ,name)) '-modify-self)
		    (,(symbol-append 'obj/ (quote ,(quote ,name)) '- (quote ,method-name) '- 'intern)
		     (self) ,@args)))))))

       ;; Define internal setter functions by mapping over defined slots.
       ;;<object>-set-<var>-intern
       (mapcar (lambda (var) (eval `(,(symbol-append (quote ,name) '- 'defmethod) ;; ugly eval hack!
				,(symbol-append 'set '- var '- 'intern)
				(,var) (setf ,(symbol-append 'self '- var) ,var))))
	       ,object-slots)

       ;; Create a setter macro which invokes the appropriate internal function.
       ;;<object>-set
       (defmacro ,(symbol-append name '- 'set) (,name var val)
	 `(,(symbol-append (quote ,name) '- 'set '- var '- 'intern) ,,name ,val))

       ;; Define internal getter functions by mapping over defined slots.
       ;;<object>-get-<var>-intern
       (mapcar (lambda (var) (eval `(,(symbol-append (quote ,name) '- 'defmethod) ;; ugly eval hack again!
				,(symbol-append 'get '- var '- 'intern)
				() ,(symbol-append 'self '- var))))
	       ,object-slots)

       ;; Create a setter macro which invokes the appropriate internal function.
       ;;<object>-set
       (defmacro ,(symbol-append name '- 'get) (,name var)
	 `(,(symbol-append (quote ,name) '- 'get '- var '- 'intern) ,,name)))))
