#|  Name:       Craig Opie
    Assignment: 2
    Sources:    https://lisp-lang.org/learn/getting-started/
                https://linux.die.net/man/1/sbcl
                http://www.sbcl.org/manual/#Global-and-Always_002dBound-variables
                https://gigamonkeys.com/book/syntax-and-semantics.html
                https://gigamonkeys.com/book/numbers-characters-and-strings.html
                https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node52.html
                https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node75.html
|#

;;; declare global constants
(defvar +ID+ "Craig Opie")

;;; Create a function that prints your name, ICS course number and assignment number. The function should return nil. 
;;; Name the function ID. The two parameters should be the course number and assignment number. Do error checking on
;;; the input. 
(defun ID (course assignment)
    (cond 
        ((null course) (format t "Error: Course not provided."))
        ((null assignment) (format t "Error: Assignment not provided."))
        ((listp course) (format t "Error: Course should not be a list."))
        ((listp assignment) (format t "Error: Assignment should not be a list."))
        ((not (numberp course)) (format t "Error: Course should be an integer."))
        ((not (numberp assignment)) (format t "Error: Assignment should be an integer."))
        ((< course 1) (format t "Error: Course should be greater than zero."))
        ((< assignment 1) (format t "Error: Assignment should be greater than zero."))
        (T (format t "Name: ~a~%Course: ICS~d~%Assignment # ~d" +ID+ course assignment))))


;;; Create a recursive function to get the last element in a list and return the element as a list.
(defun GETLAST(mylist)
    (cond 
        ((null mylist) (format t "Error: List must not be empty."))
        ((not (listp mylist)) (format t "Error: Argument must be a list."))
        ((equal (length (cdr mylist)) 0) (list (car mylist)))
        ((equal (length (cdr mylist)) 1) (cdr mylist))
        (T (GETLAST(cdr mylist)))))


;;; Create a recursive function FIRSTLAST that takes a list as a parameter. It will return a list made up of the first
;;; and last item in the original list. Do error checking on the input (e.g. checking that it is a list of at least two
;;; items).
(defun FIRSTLAST(mylist)
    (cond 
        ((null mylist) (format t "Error: List must not be empty."))
        ((not (listp mylist)) (format t "Error: Argument must be a list."))
        ((< (length mylist) 2) (format t "Error: The list must be two or more items."))
        (T (cons (car mylist) (GETLAST(rest mylist))))))

        
;;; Create a recursive function ALPHABET that takes a list of letters. It returns a list of the same length with the 
;;; corresponding integer substituted for the letter (e.g. 1 becomes A). Do error checking on the input. Return the 
;;; empty list if the list is already empty. You may use a global parameter to store letter-integer pairs. 
(defun ALPHABET(mylist)
    (cond 
        ((null mylist) nil)
        ((not (listp mylist)) (format t "Error: Argument must be a list."))
        ((equal (length (cdr mylist)) 0) (list (- (char-int (coerce (car mylist) 'character)) 64)))
        (T (cons (- (char-int (coerce (car mylist) 'character)) 64) (ALPHABET(cdr mylist))))))


;;; Perform type determination.
(defun FINDTYPE(mylist)
    (cond
        ((typep (car mylist) 'nil) (list 'nil))
        ((typep (car mylist) 'null) (list 'null))
        ((typep (car mylist) 'number) (list 'number 't))
        ((typep (car mylist) 'symbol) (list 'symbol 't))
        ((typep (car mylist) 'string) (list 'string 't))
        ((typep (car mylist) 'list) (list 'list 't))
        ((typep (car mylist) 't) (list 't))))


;;; Create a recursive function TYPECHECK that takes a list as its parameter and returns a list containing a list of
;;; the applicable types (in any order) for each item on the list. The types you should check for are: NIL, T, NUMBER,
;;; SYMBOL, LIST and STRING. Do error checking on the input.
(defun TYPECHECK(mylist)
    (cond 
        ((equal (length (cdr mylist)) 0) (list (FINDTYPE(list (car mylist)))))
        (T (cons (FINDTYPE(list (car mylist))) (TYPECHECK(cdr mylist))))))


;;; Create a recursive function FILTERTYPE that takes a type name (see #4) and a list. It should return the list with
;;; all items of that type removed. Do error checking on the input.
(defun FILTERTYPE(mytype mylist)
    (cond 
        ((not (or (equal mytype 'nil)
            (equal mytype 'null)
            (equal mytype 't)
            (equal mytype 'number)
            (equal mytype 'symbol)
            (equal mytype 'string)
            (equal mytype 'list)))
            (format t "Error: First argument must be a valid type name."))
        ((typep (car mylist) mytype) (FILTERTYPE mytype (cdr mylist)))
        ((equal (length (cdr mylist)) 0) (list (car mylist)))
        ((equal (length (cdr mylist)) 1) (if (not (typep (car (cdr mylist)) mytype)) (cons (car mylist) (cdr mylist)) (list (car mylist))))
        ((> (length (cdr mylist)) 1) (cons (car mylist) (FILTERTYPE mytype (cdr mylist))))))