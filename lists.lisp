;;;; ================================================================
;;;; CMPS 3500 – Common Lisp Tutorial with Interactive Test Menu
;;;; ================================================================
;;;; Purpose:
;;;;   Demonstrate recursion, tail recursion, list manipulation, and
;;;;   higher-order functions in Common Lisp. Includes an interactive
;;;;   menu for testing functions.
;;;; Usage:
;;;;   sbcl --load lists_menu_final.lisp --eval '(menu)' --quit
;;;; ================================================================

(in-package :cl-user)

(declaim (optimize (safety 3) (speed 1) (debug 0)))

;; -------------------------------------------------------------------
;; FUNCTION TYPE DECLARATIONS — improve compile-time checks
;; -------------------------------------------------------------------
(declaim
 (ftype (function (list list) list) list-append)
 (ftype (function (list) list) slow-list-reverse list-reverse reverse-list-elements)
 (ftype (function (integer) (integer 0 *)) factorial fast-factorial)
 (ftype (function (integer integer) (values integer integer)) order)
 (ftype (function (function fixnum t) t) repeat-transformation)
 (ftype (function (list) t) find-even remove-even))

;; ================================================================
;; SECTION 1: BASIC LIST RECURSION
;; ================================================================

;; Sample run:
;;   (list-append '(1 2 3) '(4 5))  => (1 2 3 4 5)
;; Built-ins used:
;;   NULL, FIRST, REST, CONS
(defun list-append (L1 L2)
  "Append L2 to the end of L1 using recursion.
   Example:
     (list-append '(1 2 3) '(4 5)) => (1 2 3 4 5)"
  (if (null L1)
      L2
      (cons (first L1) (list-append (rest L1) L2))))

;; Sample run:
;;   (slow-list-reverse '(a b c))  => (c b a)
;; Built-ins used:
;;   NULL, FIRST, REST, LIST, (calls list-append)
(defun slow-list-reverse (L)
  "Reverse a list in a non-tail-recursive manner (less efficient).
   Example:
     (slow-list-reverse '(1 2 3)) => (3 2 1)"
  (if (null L)
      nil
      (list-append (slow-list-reverse (rest L)) (list (first L)))))

;; Sample run:
;;   (list-reverse '(a b c d))  => (d c b a)
;; Built-ins / macros used:
;;   LOOP, SETF, CONS
(defun list-reverse (L)
  "Reverse a list using an explicit loop (constant stack usage).
   Example:
     (list-reverse '(1 2 3)) => (3 2 1)"
  (loop for x in L
        with acc = nil
        do (setf acc (cons x acc))
        finally (return acc)))

;; ================================================================
;; SECTION 2: FACTORIAL – NORMAL & TAIL-RECURSIVE
;; ================================================================

;; Sample runs:
;;   (factorial 0) => 1
;;   (factorial 5) => 120
;;   (ignore-errors (factorial -2)) => NIL and a condition (error message)
;; Built-ins used:
;;   MINUSP, ZEROP, =, *, 1-, COND, ERROR
(defun factorial (N)
  "Compute factorial of N recursively with 0! = 1; error on negatives.
   Example: (factorial 5) => 120"
  (cond
    ((minusp N) (error "factorial: N must be >= 0, got ~a" N))
    ((or (zerop N) (= N 1)) 1)
    (t (* N (factorial (1- N))))))

;; Sample runs:
;;   (fast-factorial 0) => 1
;;   (fast-factorial 6) => 720
;;   (ignore-errors (fast-factorial -1)) => NIL and a condition
;; Built-ins used:
;;   WHEN, MINUSP, LABELS, IF, <=, *, 1-, ERROR
(defun fast-factorial (N)
  "Compute factorial of N with a tail-recursive/iterative helper.
   Supports 0! = 1; errors on negatives.
   Example: (fast-factorial 5) => 120"
  (when (minusp N) (error "fast-factorial: N must be >= 0, got ~a" N))
  (labels ((fast-factorial-aux (K A)
             (if (<= K 1) A (fast-factorial-aux (1- K) (* K A)))))
    (fast-factorial-aux N 1)))

;; ================================================================
;; SECTION 3: FUNCTIONS AS FIRST-CLASS OBJECTS
;; ================================================================

;; Sample run:
;;   (doublen 7) => 14
;; Built-ins used:
;;   * (numeric multiplication)
(defun doublen (x)
  "Multiply X by 2.
   Example: (doublen 5) => 10"
  (* 2 x))

;; Sample runs:
;;   (repeat-transformation #'doublen 3 5) => 40
;;   ; 5 -> 10 -> 20 -> 40 (apply doublen three times)
;;   (repeat-transformation (lambda (y) (+ y 1)) 4 0) => 4
;; Built-ins used:
;;   ZEROP, 1-, FUNCALL, IF
(defun repeat-transformation (F N X)
  "Apply function F to X repeatedly, N times.
   Example:
     (repeat-transformation #'doublen 3 5) => 40"
  (if (zerop N)
      X
      (repeat-transformation F (1- N) (funcall F X))))

;; ================================================================
;; SECTION 4: LIST MANIPULATION
;; ================================================================

;; Sample run:
;;   (prepend-blah '(1 2 3)) => (BLAH 1 2 3)
;; Built-ins used:
;;   CONS, quoted symbol 'BLAH
(defun prepend-blah (L)
  "Add 'BLAH to the front of list L.
   Example: (prepend-blah '(1 2 3)) => (BLAH 1 2 3)"
  (cons 'blah L))

;; Sample runs:
;;   (list-nth 0 '(a b c)) => A
;;   (list-nth 2 '(a b c)) => C
;;   (list-nth 1 '(10 20 30 40)) => 20
;; Built-ins used:
;;   REPEAT-TRANSFORMATION (user-defined), FIRST
(defun list-nth (N L)
  "Return the Nth element (0-based) of list L.
   Example:
     (list-nth 2 '(a b c d)) => C"
  (first (repeat-transformation #'rest N L)))

;; Sample run:
;;   (double-list-elements '(1 2 3)) => (2 4 6)
;; Built-ins used:
;;   NULL, FIRST, REST, CONS, (calls doublen)
(defun double-list-elements (L)
  "Return a list with all numeric elements doubled.
   Example:
     (double-list-elements '(1 2 3)) => (2 4 6)"
  (if (null L)
      nil
      (cons (doublen (first L)) (double-list-elements (rest L)))))

;; Sample run:
;;   (reverse-list-elements '((1 2) (a b c) (x))) => ((2 1) (c b a) (x))
;; Built-ins used:
;;   NULL, FIRST, REST, CONS, REVERSE
(defun reverse-list-elements (L)
  "Given a list of lists, return a list containing the reversals.
   Example:
     (reverse-list-elements '((1 2) (3 4))) => ((2 1) (4 3))"
  (if (null L)
      nil
      (cons (reverse (first L)) (reverse-list-elements (rest L)))))

;; Sample run:
;;   (mapfirst #'1+ '(2 4 6)) => (3 5 7)
;;   (mapfirst #'doublen '(2 4 6)) => (4 8 12)
;; Built-ins used:
;;   MAPCAR (applies a function elementwise)
(defun mapfirst (F L)
  "Apply function F to every element of L and return new list.
   Example:
     (mapfirst #'doublen '(2 4 6)) => (4 8 12)"
  (mapcar F L))

;; ================================================================
;; SECTION 5: SEARCHING & FILTERING
;; ================================================================

;; Sample runs:
;;   (find-even '(1 3 5 6 8)) => 6
;;   (find-even '(x 3 7 9))   => NIL
;;   (find-even '(0 2 4))     => 0
;; Built-ins used:
;;   NULL, FIRST, REST, INTEGERP, EVENP, COND
(defun find-even (L)
  "Return the first even integer in list L, or NIL if none.
   Note: 0 counts as even in Lisp. Non-integers are skipped.
   Example:
     (find-even '(1 3 5 6 8)) => 6
     (find-even '(0 2 4)) => 0"
  (cond
    ((null L) nil)
    ((and (integerp (first L)) (evenp (first L))) (first L))
    (t (find-even (rest L)))))

;; Sample runs:
;;   (find-positive-even '(0 2 4 6)) => 2
;;   (find-positive-even '(1 3 5))   => NIL
;; Built-ins used:
;;   NULL, FIRST, REST, INTEGERP, EVENP, >, COND
(defun find-positive-even (L)
  "Return the first positive even integer in list L.
   Non-integers are skipped.
   Example:
     (find-positive-even '(0 2 4 6)) => 2"
  (cond
    ((null L) nil)
    ((and (integerp (first L)) (evenp (first L)) (> (first L) 0)) (first L))
    (t (find-positive-even (rest L)))))

;; Sample runs:
;;   (list-find-if #'evenp '(3 5 6 9)) => 6
;;   (list-find-if (lambda (x) (> x 10)) '(2 9 10 11)) => 11
;; Built-ins used:
;;   NULL, FIRST, REST, FUNCALL, COND
(defun list-find-if (P L)
  "Return the first element of L that satisfies predicate P.
   Example:
     (list-find-if #'evenp '(3 5 6 9)) => 6"
  (cond
    ((null L) nil)
    ((funcall P (first L)) (first L))
    (t (list-find-if P (rest L)))))

;; Sample runs:
;;   (remove-short-lists '((1 2) (3 4 5) (6) (a b c d))) => ((3 4 5) (A B C D))
;; Built-ins used:
;;   NULL, FIRST, REST, LENGTH, COND, CONS
(defun remove-short-lists (L)
  "Remove all sublists of L that have length < 3.
   Example:
     (remove-short-lists '((1 2) (3 4 5) (6))) => ((3 4 5))"
  (cond
    ((null L) nil)
    ((< (length (first L)) 3) (remove-short-lists (rest L)))
    (t (cons (first L) (remove-short-lists (rest L))))))

;; Sample runs:
;;   (remove-even '(1 2 3 4 5 6)) => (1 3 5)
;;   (remove-even '(a 2 b 4 c))   => (A B C)  ; non-integers kept
;; Built-ins used:
;;   NULL, FIRST, REST, INTEGERP, EVENP, COND, CONS
(defun remove-even (L)
  "Return list L with all even integers removed.
   Non-integers are kept as-is.
   Example:
     (remove-even '(1 2 3 4 5)) => (1 3 5)"
  (cond
    ((null L) nil)
    ((and (integerp (first L)) (evenp (first L))) (remove-even (rest L)))
    (t (cons (first L) (remove-even (rest L))))))

;; Sample runs:
;;   (list-intersection '(1 2 2 3 4) '(2 3 4 5)) => (2 2 3 4)
;;   (list-intersection '(a b c) '(x y z))       => NIL
;; Built-ins used:
;;   REMOVE-IF, MEMBER, LAMBDA
(defun list-intersection (L1 L2)
  "Return a list of elements common to both L1 and L2.
   Keeps duplicates from L1 and preserves L1 order.
   Example:
     (list-intersection '(1 2 2 3 4) '(2 3 4 5)) => (2 2 3 4)"
  (remove-if #'(lambda (X) (not (member X L2))) L1))

;; ================================================================
;; SECTION 6: MULTIPLE RETURN VALUES
;; ================================================================

;; Sample runs:
;;   (multiple-value-bind (mn mx) (order 10 4) (list mn mx)) => (4 10)
;;   (multiple-value-list (order 3 9)) => (3 9)
;; Built-ins used:
;;   VALUES, >=, IF
(defun order (a b)
  "Return two values: (min a b) and (max a b).
   Example:
     (multiple-value-bind (mn mx) (order 10 4)
       (list mn mx)) => (4 10)"
  (if (>= a b)
      (values b a)
      (values a b)))

;; ================================================================
;; SECTION 7: DEFAULT TEST LISTS
;; ================================================================

(defparameter *list1* (list 0 2 4 6 8 10))
(defparameter *list2* (list 1 10 1001 10000 100202))

;; ================================================================
;; SECTION 8: MENU UTILITIES
;; ================================================================

;; Sample runs:
;;   (prompt "Hello ~a!" "world") => prints "Hello world!" immediately.
;; Built-ins used:
;;   APPLY, FORMAT, FINISH-OUTPUT
(defun prompt (fmt &rest args)
  "Print formatted text and flush output immediately."
  (apply #'format t fmt args)
  (finish-output))

;; Sample runs:
;;   (with-input-from-string (s "  hello  ")
;;     (let ((*standard-input* s)) (read-line-trimmed))) => "hello"
;; Built-ins used:
;;   READ-LINE, STRING-TRIM
(defun read-line-trimmed ()
  "Read a line of text from user input and trim spaces."
  (string-trim '(#\Space #\Tab) (read-line)))

;; Sample runs:
;;   (parse-integer-safe "42")   => 42
;;   (parse-integer-safe "oops") => NIL and prints a warning
;; Built-ins used:
;;   HANDLER-CASE, PARSE-INTEGER, FORMAT, PROGN
(defun parse-integer-safe (s)
  "Safely parse string S as an integer; return NIL on error."
  (handler-case (parse-integer s)
    (error () (progn (format t "~&[!] Not an integer: ~a~%" s) nil))))

;; Sample runs:
;;   (parse-list-safe "(1 2 3)")     => (1 2 3)
;;   (parse-list-safe "not a list")  => NIL with a helpful message
;;   (parse-list-safe "#.(+ 1 2)")   => NIL (read-time eval disabled)
;; Built-ins used:
;;   HANDLER-CASE, MAKE-STRING-INPUT-STREAM, LET*, READ, LISTP, EQ,
;;   FORMAT, PROGN, SPECIAL VAR *READ-EVAL*
(defun parse-list-safe (s)
  "Read a Lisp list from string S safely; used for custom input.
   Disables read-time evaluation for safety."
  (handler-case
      (let* ((stream (make-string-input-stream s))
             (*read-eval* nil)
             (form (read stream nil :eof)))
        (if (and (listp form) (not (eq form :eof)))
            form
            (progn (format t "~&[!] Please enter a proper Lisp list form.~%") nil)))
    (error () (progn (format t "~&[!] Could not read a Lisp list from input.~%") nil))))

;; Sample interaction:
;;   (choose-list) =>
;;     prompts user to pick 1/2 or enter a custom list;
;;     returns the chosen/parsed list.
;; Built-ins used:
;;   LOOP, PROMPT (user-defined), FORMAT, READ-LINE, STRING=,
;;   PARSE-INTEGER-SAFE, CASE, RETURN, PROGN
(defun choose-list (&optional (default *list1*))
  "Prompt user to select a predefined list or enter their own."
  (loop
     (prompt "~&Choose a list:
  1) *list1* => ~a
  2) *list2* => ~a
  3) Enter a list yourself
Selection (1-3) [default 1]: " *list1* *list2*)
     (let* ((ans (read-line-trimmed))
            (pick (if (string= ans "") 1 (parse-integer-safe ans))))
       (when pick
         (return
           (case pick
             (1 *list1*)
             (2 *list2*)
             (3 (progn
                  (prompt "~&Enter a Lisp list (e.g., (1 2 3)): ")
                  (loop for L = (parse-list-safe (read-line-trimmed))
                        when L return L
                        do (prompt "~&Try again: "))))
             (t (progn (format t "~&[!] Pick 1, 2, or 3.~%") default))))))))

;; ================================================================
;; SECTION 9: MENU COMMAND FUNCTIONS
;; ================================================================

;; Sample interaction:
;;   User enters N=5
;;   Output shows factorial(5) and fast-factorial(5)
;; Built-ins used:
;;   PROMPT, READ-LINE-TRIMMED, PARSE-INTEGER-SAFE,
;;   HANDLER-CASE, FORMAT
(defun menu-factorial ()
  "Prompt for a number and show factorial using both methods."
  (prompt "~&Enter a non-negative integer N: ")
  (let ((n (parse-integer-safe (read-line-trimmed))))
    (when n
      (handler-case
          (progn
            (format t "~&factorial(~d)      = ~a~%" n (factorial n))
            (format t "fast-factorial(~d) = ~a~%" n (fast-factorial n)))
        (error (e) (format t "~&[!] ~a~%" e))))))

;; Sample interaction:
;;   After choosing a list, prints both slow and fast reversals
;; Built-ins used:
;;   CHOOSE-LIST (user-defined), FORMAT
(defun menu-list-reverse ()
  (let ((L (choose-list)))
    (format t "~&slow-list-reverse => ~a~%" (slow-list-reverse L))
    (format t "list-reverse      => ~a~%" (list-reverse L))))

;; Sample interaction:
;;   Choose a list; prints doubled elements
;; Built-ins used:
;;   CHOOSE-LIST, FORMAT
(defun menu-double-list ()
  (let ((L (choose-list)))
    (format t "~&double-list-elements => ~a~%" (double-list-elements L))))

;; Sample interaction:
;;   Choose a list; prints the first even integer or NIL
;; Built-ins used:
;;   CHOOSE-LIST, FORMAT
(defun menu-find-even ()
  (let ((L (choose-list)))
    (format t "~&find-even => ~a~%" (find-even L))))

;; Sample interaction:
;;   Choose a list; prints list with evens removed
;; Built-ins used:
;;   CHOOSE-LIST, FORMAT
(defun menu-remove-even ()
  (let ((L (choose-list)))
    (format t "~&remove-even => ~a~%" (remove-even L))))

;; Sample interaction:
;;   Choose two lists; prints their intersection (with duplicates from L1)
;; Built-ins used:
;;   CHOOSE-LIST, FORMAT
(defun menu-list-intersection ()
  (let ((L1 (choose-list *list1*))
        (L2 (choose-list *list2*)))
    (format t "~&list-intersection => ~a~%" (list-intersection L1 L2))))

;; Sample interaction:
;;   Choose a list; prints (BLAH . list)
;; Built-ins used:
;;   CHOOSE-LIST, FORMAT
(defun menu-prepend-blah ()
  (let ((L (choose-list)))
    (format t "~&prepend-blah => ~a~%" (prepend-blah L))))

;; Sample interaction:
;;   Choose a list; enter index N; prints element or out-of-range message
;; Built-ins used:
;;   CHOOSE-LIST, PROMPT, READ-LINE-TRIMMED,
;;   PARSE-INTEGER-SAFE, LENGTH, MINUSP, >=, FORMAT
(defun menu-list-nth ()
  (let ((L (choose-list)))
    (prompt "~&Enter N (0-based index): ")
    (let ((n (parse-integer-safe (read-line-trimmed))))
      (when n
        (if (or (minusp n) (>= n (length L)))
            (format t "~&[!] N out of range for list of length ~d.~%" (length L))
            (format t "~&list-nth ~d => ~a~%" n (list-nth n L)))))))

;; Sample interaction:
;;   Choose a list; applies doublen to each element
;; Built-ins used:
;;   CHOOSE-LIST, FORMAT
(defun menu-mapfirst-doublen ()
  (let ((L (choose-list)))
    (format t "~&mapfirst doublen => ~a~%" (mapfirst #'doublen L))))

;; Sample interaction:
;;   Enter X and N; shows repeated doubling result
;; Built-ins used:
;;   PROMPT, READ-LINE-TRIMMED, PARSE-INTEGER-SAFE, FORMAT
(defun menu-repeat-transformation ()
  (prompt "~&Enter a number X: ")
  (let ((x (parse-integer-safe (read-line-trimmed))))
    (when x
      (prompt "How many times to apply doublen? N: ")
      (let ((n (parse-integer-safe (read-line-trimmed))))
        (when n
          (format t "~&repeat-transformation doublen ~d times to ~d => ~a~%"
                  n x (repeat-transformation #'doublen n x)))))))

;; Sample interaction:
;;   Enter A and B; prints min and max using multiple values
;; Built-ins used:
;;   PROMPT, READ-LINE-TRIMMED, PARSE-INTEGER-SAFE,
;;   MULTIPLE-VALUE-BIND, FORMAT
(defun menu-order ()
  (prompt "~&Enter first number A: ")
  (let ((a (parse-integer-safe (read-line-trimmed))))
    (when a
      (prompt "Enter second number B: ")
      (let ((b (parse-integer-safe (read-line-trimmed))))
        (when b
          (multiple-value-bind (mn mx) (order a b)
            (format t "~&order(~a, ~a) => min=~a  max=~a~%" a b mn mx)))))))

;; ================================================================
;; SECTION 10: MENU LOOP
;; ================================================================

;; Sample run:
;;   (print-banner) =>
;;     Prints static header including current *list1* and *list2*.
;; Built-ins used:
;;   FORMAT
(defun print-banner ()
  (format t "~%---------------------------------------------~%")
  (format t "   CMPS 3500 — LISP Tutorial Test Menu~%")
  (format t "   *list1* = ~a~%" *list1*)
  (format t "   *list2* = ~a~%" *list2*)
  (format t "---------------------------------------------~%"))

;; Sample run:
;;   (print-menu) =>
;;     Prints numbered options for the interactive program.
;; Built-ins used:
;;   FORMAT
(defun print-menu ()
  (format t "~%Choose an option:
  1) Factorial & Fast Factorial
  2) Reverse a List (slow vs fast)
  3) Double All Elements
  4) Find Leftmost Even
  5) Remove Evens
  6) List Intersection
  7) Prepend 'BLAH
  8) Nth Element
  9) mapfirst doublen
 10) repeat-transformation (doublen)
 11) order (min & max)
 99) Reset default lists
  0) Quit~%"))

;; Sample run:
;;   (reset-default-lists) =>
;;     Restores *list1* and *list2* to their initial values and prints confirmation.
;; Built-ins used:
;;   SETF, LIST, FORMAT
(defun reset-default-lists ()
  (setf *list1* (list 0 2 4 6 8 10)
        *list2* (list 1 10 1001 10000 100202))
  (format t "~&Defaults restored.~%"))

;; Sample run:
;;   (menu-dispatch 1) => invokes menu-factorial
;;   (menu-dispatch 99) => resets lists
;; Built-ins used:
;;   CASE, FORMAT
(defun menu-dispatch (choice)
  (case choice
    (1  (menu-factorial))
    (2  (menu-list-reverse))
    (3  (menu-double-list))
    (4  (menu-find-even))
    (5  (menu-remove-even))
    (6  (menu-list-intersection))
    (7  (menu-prepend-blah))
    (8  (menu-list-nth))
    (9  (menu-mapfirst-doublen))
    (10 (menu-repeat-transformation))
    (11 (menu-order))
    (99 (reset-default-lists))
    (t  (format t "~&[!] Unknown option: ~a~%" choice))))

;; Sample interaction:
;;   (menu) =>
;;     Shows banner and menu, reads choices until user enters 0.
;; Built-ins used:
;;   PRINT-BANNER, PRINT-MENU, PROMPT, READ-LINE-TRIMMED,
;;   STRING=, PARSE-INTEGER-SAFE, COND, =, RETURN, LOOP, FORMAT
(defun menu ()
  "Main interactive menu loop."
  (print-banner)
  (loop
     (print-menu)
     (prompt "~&Your choice: ")
     (let* ((raw (read-line-trimmed))
            (choice (if (string= raw "") -1 (parse-integer-safe raw))))
       (cond
         ((null choice) (format t "~&Try again.~%"))
         ((= choice 0)  (return (format t "~&Goodbye!~%")))
         (t (menu-dispatch choice))))))

;;;; ================================================================
;;;; END OF FILE
;;;; ================================================================