;;; CL-PARSEC core

(in-package :parsec)

(locally-enable-literal-syntax :sharp-backq)


;;; conditions

(define-condition parsec-error (simple-error)
  ((stack :initarg :stack :reader error-stack))
  (:report (lambda (condition stream)
             (format stream "Parsing error.  Error stack: ~a"
                     (error-stack condition)))))

(define-condition parsec-signal (condition)
  ((features :initarg :features :initform nil :accessor signal-features)))

(define-condition parsec-success (parsec-signal)
  ((result :initarg :result :reader parsing-result)))

(define-condition try-success (parsec-signal)
  ((backlog :initarg :backlog :reader parsing-backlog)))

(defun ?! (&optional reason prev-error)
  "Signal PARSEC-ERROR with REASON on top of PREV-ERROR's stack.
\(Name stands for `Wtf?!')"
  (error 'parsec-error :stack (when reason
                                (cons (mklist reason)
                                      (when prev-error
                                        (error-stack prev-error))))))


;;; specials

(defvar *stream* nil
  "Items' input stream.")

(defvar *backlog* nil
  "Items' backlog.")

(defvar *source* nil
  "Current source for items.")

(defvar *current-item* nil
  "Current item (acquired from *SOURCE* in NEXT-ITEM).")

(defvar *echo-p* nil
  "Is debugging to *ECHO-STREAM* on?")

(defvar *echo-stream* t
  "Stream for debugging information.")


;;; convenience macros

(defmacro next-item ()
  "Sets *CURRENT-ITEM* to newly READ-ITEM from *STREAM* or *BACKLOG* and ~
returns it."
  (with-gensyms (cur)
    `(prog1 (setf *source* (if *backlog* :backlog :stream)
                  *current-item* (read-item (or *backlog* *stream*)))
       (when *echo-p*
         (format *echo-stream* "~:@c" *current-item*)))))


;;; reading/unreading

(defgeneric read-item (source)
  (:documentation "`Read' item from SOURCE.")
  (:method ((source stream))
    (read-char *stream*))
  (:method ((source list))
    (pop *backlog*)))

(defgeneric unread-item (item source)
  (:documentation "`Unread' item to SOURCE.")
  (:method ((char character) (source stream))
    (unread-char char *stream*))
  (:method (item (source list))
    (push item *backlog*)))

(defmacro unread-last-item ()
  "Unread *CURRENT-ITEM* to current *SOURCE*."
  `(unread-item *current-item*
                (if (eq *source* :stream) *stream*
                    *backlog*)))

;;; parsing

(defun parse (stream parser)
  "Parses STREAM with the given PARSER function.  Binds specials."
  (let ((*stream* stream)
        *backlog*
        (*source* :stream)
        *current-item*)
    (funcall parser)))

(defgeneric parse-test (test item)
  (:documentation "Convenience function to test the current element with TEST.
Instead of writing: (apply test-fn item), we use (parse-test test item), where ~
TEST may have various correspondence to TEST-FN, like:
 * for chars: CHAR -> #`(char= CHAR _)
 * for functions: FN -> FN
 * for lists: LIST -> #`(apply (car LIST) _ (cdr LIST))
")
  (:method ((test character) (item character))
    (char= test item))
  (:method ((test function) item)
    (funcall test item))
  (:method ((test list) item)
    (apply (car test) item (cdr test))))

(defmacro mkparser (test &optional (return :parsed))
  "Creates the parser, based on PARSE-TEST, that implemets the following logic:
it reads the next item with NEXT-ITEM, checks it with PARSE-TEST,
 * if test passes, it signals PARSEC-SUCCESS and returns
   - the item (default case)
   - TEST (if return = :test)
   - otherwise RETURN itself
 * otherwise it performs UNREAD-LAST-ITEM and signals PARSEC-ERROR."
  (with-gensyms (cur item gtest greturn)
    `(lambda ()
       (let ((,gtest ,test)
             (,greturn ,return)
             (,cur (next-item)))
         (if (parse-test ,gtest ,cur)
             (progn (signal 'parsec-success :result ,cur)
                    (case ,greturn
                      (:parsed   ,cur)
                      (:test     ,gtest)
                      (otherwise ,greturn)))
             (progn (unread-last-item)
                    (?! `(parser ,,gtest))))))))

(declaim (inline parsecall))
(defun parsecall (test &optional (return :parsed))
  "Funcall TEST as if it was already passed to MKPARSER.  RETURN semantics ~
repeats MKPARSER's one."
  (funcall (mkparser test return)))

(defmacro defparser (name (&rest args) &body body)
  "DEFUN a parser function with the given NAME and ARGS.  BODY is wraped in ~
HANDLER-CASE, that traps PARSEC-ERROR and resignalls it with NAME added to ~
error stack.

Provides internal variable _PARSER-NAME_.

Intended for topl-level use, like DEFUN."
  `(defun ,name (,@args)
     ,(when (stringp (car body))
        (car body))
     (let ((_parser-name_ ',name))
       (handler-case
           (progn ,@body)
         (parsec-error (e) (?! ',name e))))))


;; parser DSL macros

(defmacro intercept-signals (signal-name (&rest signal-bindings) &body body)
  "Evaluate BODY, intercepting PARSEC-SIGNALs, that are specified in ~
SIGNAL-BINDINGS as: (signal . handling code) and evaluating handling code ~
inside the lambda binding of SIGNAL-NAME to current signal object.
Afterwars _PARSER-NAME_ is added to signal's features to prevent multiple ~
handling in the same parser."
  `(handler-bind
       (,@(mapcar (lambda (binding)
                    `(,(car binding)
                       (lambda (,signal-name)
                         (unless (member _parser-name_
                                         (signal-features ,signal-name))
                           ,@(cdr binding))
                         (pushnew _parser-name_
                                  (signal-features ,signal-name)))))
                  signal-bindings))
     ,@body))

(eval-always
  (defmacro mk-if-macro (name conditions)
    "Make a special-case wrapper macro for handling specific ~
CONDITIONS with the name if-NAME (like IF-ERR)."
    `(progn
       (defmacro ,(mksym name :format "if-~a") (action &body body)
         `(handler-case (progn ,@body)
            (,',conditions () ,action)))
       (export ',(mksym name :format "if-~a") (find-package :cl-parsec))))

  (mk-if-macro eof end-of-file)
  (mk-if-macro err parsec-error)
  (mk-if-macro end (or parsec-error end-of-file)))

;;; end