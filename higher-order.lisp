;;; CL-PARSEC higher-order parsers

(in-package :parsec)

(defparser many (parser)
  "Returns all results of PARSER's application."
  (let (rez)
    (if-end (reverse rez)
      (loop (push (funcall parser) rez)))))

(defparser many+ (parser)
  "Returns all results of PARSER's application.  PARSER should return ~
successfully at least once."
  (cons (funcall parser)
        (many parser)))

(defparser either (&rest parsers)
  "If either of the PARSERS returns non-nil, return its result. ~
Ordering matters.

LL(1) variant.  If it doesn't suit you, use this pattern: (EITHER (TRY ...))"
  (dolist (parser parsers)
    (if-err nil
      (return-from either (funcall parser))))
  (?!))

(defparser try (parser)
  "Returns the result of PARSER's application or `unreads' all the ~
read items back."
  (let (backlog)
    (prog1 (intercept-signals this-signal
               ((parsec-success (push (parsing-result this-signal) backlog))
                (try-success (setf backlog (append (parsing-backlog this-signal)
                                                   backlog))))
             (if-end (progn (setf *backlog* (append (reverse backlog) *backlog*)
                                  *source* :backlog)
                            (?!))
               (funcall parser)))
      (signal 'try-success :backlog backlog))))

(defparser maybe (parser)
  "A wrapper around TRY to ignore unsuccessfull attempts and simply return NIL."
  (if-err ()
    (try parser)))

;;; end