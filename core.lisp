;;; CL-PARSEC core

(in-package :parsec)

(locally-enable-literal-syntax :sharp-backq)


;;; conditions

(define-condition want-more (condition)
  ((features :initarg :features :initform nil :reader more-features)))

(define-condition parsec-error (simple-error)
  ((stack :initarg :stack :reader error-stack))
  (:report (lambda (condition stream)
             (format stream "Parse error.  Error stack: ~a"
                     (error-stack condition)))))

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

(defvar *cur* nil
  "Current item (acquired from *SOURCE* in NEXT-ITEM).")

(defvar *echo-p* nil
  "Is debugging to *ECHO-STREAM* on?")

(defvar *echo-stream* t
  "Stream for debugging information.")


;;; convenience macros

(define-symbol-macro next-item
    (funcall (if *echo-p* #`(progn (format *echo-stream* "~:@c" _)
                                   _)
                 #'identity)
             (setf *cur* (read-item (setf *source*
                                          (if *backlog* :backlog :stream))))))

(define-symbol-macro last-item
    *cur*)

(eval-always
  (defmacro mk-till-macro (name conditions)
    "Make a special-case wrapper macro for handling specific
CONDITIONS with the name TILL-NAME (like TILL-ERR)."
    `(defmacro ,(mksym name :format "till-~a") ((&body reaction) &body action)
       `(handler-case (progn ,@action)
          (,',conditions () (progn ,@reaction)))))

  (mk-till-macro eof end-of-file)
  (mk-till-macro err parsec-error)
  (mk-till-macro end (or parsec-error end-of-file)))


;;; reading/unreading

(defgeneric read-item (source)
  (:documentation "`Read' item from SOURCE.")
  (:method ((source (eql :stream)))
    (read-char *stream*))
  (:method ((source (eql :backlog)))
    (pop *backlog*)))

(defgeneric unread-item (item source)
  (:documentation "`Unread' item to SOURCE.")
  (:method (char (source (eql :stream)))
    (unread-char char *stream*))
  (:method (item (source (eql :backlog)))
    (push item *backlog*)))


;;; parsing

(defun parse (stream parser)
  ""
  (let ((*stream* stream)
        *cur*
        *backlog*
        (*source* :stream))
    (handler-bind ((want-more #`(invoke-restart 'parse-more)))
      (funcall parser))))

(defgeneric parse-test (test cur)
  (:documentation "")
  (:method ((test character) (cur character))
    (char= test cur))
  (:method ((test function) cur)
    (funcall test cur))
  (:method ((test list) cur)
    (apply (car test) cur (cdr test))))

(defmacro mkparser (test &optional (return :parsed))
  ""
  (with-gensyms (cur gtest greturn rez)
    `(lambda ()
       (let ((,gtest ,test)
             (,cur next-item)
             (,greturn ,return)
             ,rez)
         (restart-case
             (prog1 (if (parse-test ,gtest ,cur)
                        (setf ,rez (case ,greturn
                                     (:test     ,gtest)
                                     (:parsed   ,cur)
                                     (otherwise ,greturn)))
                        (progn (unread-item last-item *source*)
                               (?! `(parser ,,gtest))))
               (signal 'want-more))
           (parse-more () ,rez))))))

(declaim (inline parsecall))
(defun parsecall (test &optional (return :parsed))
  ""
  (funcall (mkparser test return)))

(defmacro defparser (name (&rest args) &body body)
  ""
  `(defmethod ,name (,@args)
     (handler-case
         (progn ,@body)
       (parsec-error (e) (?! ',name e)))))

;;; end