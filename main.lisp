(load "not-clos.lisp")
(defpackage :my-package
  (:use :cl))

(not-clos:make-object person (name year language))

(defvar *db* nil)

(defun add-db (person)
  (setf *db* (cons person *db*)))

(person-defmethod print-name ()
		  (format t "~a~%" self-name))


(add-db (make-person :name "John" :year 1990 :language "Scheme"))
(add-db (make-person :name "Mary" :year 1995 :language "Haskell"))
(add-db (make-person :name "Ann"  :year 1993 :language "Scheme"))
(add-db (make-person :name "Paul" :year 2000 :language "Python"))
(add-db (make-person :name "Amy"  :year 2004 :language "C"))

(mapcar	(lambda (person) (person-print-name person))
	(not-clos:where *db* ((year < 2000)
			      (language string= "Scheme"))))


(person-set (car *db*) name "x")
(person-print-name (car *db*))
(write (person-get (car *db*) name))
