(load "not-clos.lisp")
(defpackage :my-package
  (:use :cl))

(not-clos:make-object person ;; Create a new object person
		      ;; The slots of this object
                     ((name "unnamed") ;; We can set an initialization form for slot
		       year
                      (language (error "make-person: Missing required :name key")))) ;; initforms are lazily evaluated

;; Define a method; will be defined as <object name>-<method name>
(person-defmethod incf-year () ;; person-incf-year in this case
		  (incf self-year)) ;; accessible as self-<slot> inside methods for comfort
                                    ;; You can still use (person-set self year <x>)

(person-defmethod make-younger ()
		  (person-incf-year not-clos:self) ;; we can pass self to another method
		  (format t "Person ~a has just become younger!~%" self-name))

(person-defmethod return-self ()
		  (not-clos:self)) ;; the not-clos:self symbol evaluates to nil when not passed to self method,
                                   ;;  so when passing self to foreign function (not this object's method) we have to evaluate (self) first

(person-defmethod calculate-age ()
		  (- 2023 self-year)) ;; values are returned from methods as you'd expect

(defvar my-person (make-person :year 1998 :language "English")) ;; create a new person, name is going to be equal to initform "unnamed"

(person-set my-person name "Mary") ;; person-set macro to change existing value
(format t "Our person just got a name, her name is ~a!~%" (person-get my-person name)) ;; person-get macro to get value
(person-make-younger my-person)
(format t "Her age is ~a.~%" (person-calculate-age my-person))
(format t "She is a ~a.~%" (not-clos:object-type my-person))

(person-defmethod print-name ()
		  (format t "~a~%" self-name))

(defvar *db* nil)

(defun add-db (person)
  (setf *db* (cons person *db*)))

(add-db (make-person :name "John" :year 1990 :language "Scheme"))
(add-db (make-person :name "Mary" :year 1995 :language "Haskell"))
(add-db (make-person :name "Ann"  :year 1993 :language "Scheme"))
(add-db (make-person :name "Paul" :year 2000 :language "Python"))
(add-db (make-person :name "Amy"  :year 2004 :language "C"))

;; Find all people born before 2000 who like Scheme and print their names
(mapcar	(lambda (person) (person-print-name person))
	(not-clos:where *db* ((year < 2000) ;; using the where macro
			      (language string= "Scheme"))))
