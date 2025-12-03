;;;; ================================================================
;;;; CMPS 3500 – lab07.lisp
;;;; ================================================================

(in-package :cl-user)
(declaim (optimize (safety 3) (speed 1) (debug 0)))

;; ================================================================
;; SECTION 1: UTILITY FUNCTIONS
;; ================================================================

(defun prompt (fmt &rest args)
  (apply #'format t fmt args)
  (finish-output))

(defun read-line-trimmed ()
  (string-trim '(#\Space #\Tab) (read-line)))

(defun parse-integer-safe (s)
  (handler-case (parse-integer s)
    (error () (progn (format t "~&[!] Not an integer: ~a~%" s) nil))))

(defun parse-list-safe (s)
  (handler-case
      (let* ((stream (make-string-input-stream s))
             (*read-eval* nil)
             (form (read stream nil :eof)))
        (if (and (listp form) (not (eq form :eof)))
            form
            (progn (format t "~&[!] Please enter a proper Lisp list form.~%") nil)))
    (error () (progn (format t "~&[!] Could not read a Lisp list from input.~%") nil))))

;; ================================================================
;; SECTION 2: LAB FUNCTIONS
;; ================================================================

;; --------------------
;; 1) Unique Words
;; --------------------
(defun unique-words (infile outfile)
  "Read words from INFILE, find unique words (case-insensitive), write to OUTFILE."
  (with-open-file (in infile)
    (let ((words (loop for line = (read-line in nil)
                       while line
                       append (remove-if #'string= '() (mapcar #'string-downcase (split-sequence #\Space line))))))
      ;; Remove duplicates
      (let ((unique-list nil))
        (dolist (w words)
          (unless (member w unique-list :test #'string=)
            (push w unique-list)))
        ;; Write to outfile
        (with-open-file (out outfile :direction :output :if-exists :supersede)
          (dolist (w (nreverse unique-list))
            (format out "~a~%" w)))))))

;; --------------------
;; 2) Recursice Reverse
;; --------------------
(defun reverse-list (lst)
  "Recursively reverse a list."
  (if (null lst)
      nil
      (append (reverse-list (rest lst)) (list (first lst)))))

;; -------------------------
;; 3) Recursive Power of two
;; -------------------------
(defun power-two (n)
  "Compute 2^n recursively (logarithmic time)."
  (cond
    ((= n 0) 1)
    ((evenp n) (let ((half (power-two (/ n 2)))) (* half half)))
    (t (* 2 (power-two (1- n))))))

;; --------------------------------
;; 4) Test Recursive Power of a Set
;; --------------------------------
(defun powerset (lst)
  "Recursively generate the powerset of a list."
  (if (null lst)
      (list nil)
      (let* ((rest-ps (powerset (rest lst))))
        (append rest-ps
                (mapcar (lambda (x) (cons (first lst) x)) rest-ps)))))

;; --------------------
;; 5) Intercalate
;; --------------------
(defun intercalate (l1 l2)
  "Intercalate elements of two lists."
  (if (or (null l1) (null l2))
      (append l1 l2)
      (cons (first l1)
            (cons (first l2)
                  (intercalate (rest l1) (rest l2))))))

;; ================================================================
;; SECTION 3: MENU INTEGRATION
;; ================================================================

(defparameter *list1* '(0 2 4 6 8 10))
(defparameter *list2* '(1 10 1001 10000 100202))

(defun choose-list (&optional (default *list1*))
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

(defun menu-dispatch (choice)
  (case choice
    (1 (prompt "~&unique-words demo not interactive; call manually.~%"))
    (2 (let ((L (choose-list))) (format t "~&reverse-list => ~a~%" (reverse-list L))))
    (3 (prompt "~&power-two demo: enter n: ")
       (let ((n (parse-integer-safe (read-line-trimmed))))
         (when n (format t "~&2^~a = ~a~%" n (power-two n)))))
    (4 (let ((L (choose-list))) (format t "~&powerset => ~a~%" (powerset L))))
    (5 (let ((L1 (choose-list)) (L2 (choose-list)))
         (format t "~&intercalate => ~a~%" (intercalate L1 L2))))
    (0 (format t "~&Goodbye!~%"))
    (t (format t "~&[!] Unknown option: ~a~%" choice))))

(defun print-menu ()
  (format t "~%Choose an option:
  1) Test unique-words
  2) Test Recursive reverse
  3) Tests Recursive Power of 2
  4) Test power of a set
  5) Test intercalate
  0) Quit"))

(defun menu ()
  (loop
     (print-menu)
     (prompt "~&Your choice: ")
     (let* ((raw (read-line-trimmed))
            (choice (if (string= raw "") -1 (parse-integer-safe raw))))
       (cond
         ((null choice) (format t "~&Try again.~%"))
         ((= choice 0) (return (format t "~&Goodbye!~%")))
         (t (menu-dispatch choice))))))
;;;; ================================================================
;;;; END OF FILE
;;;; ================================================================

