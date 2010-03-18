;;; CL-PARSEC higher-order parsers

(in-package :parsec)

(defparser many (parser)
  "Returns all results of PARSER's application."
  (let (rez)
    (till-end ((reverse rez))
      (loop (push (funcall parser) rez)))))

(defparser many+ (parser)
  "Returns all results of PARSER's application.  PARSER should return
successfully at least once."
  (cons (funcall parser)
        (many parser)))

(defparser either (&rest parsers)
  "If either of the PARSERS returns non-nil, return its result.
Ordering matters.

LL(1) variant.  If it doesn't suit you, use this pattern:
  (either (try ...))"
  (dolist (parser parsers)
    (till-err ()
      (when-it (funcall parser)
        (return-from either it))))
  (?!))

(defparser try (parser)
  "Returns the result of PARSER's application or `unreads' all the
read items back."
  (let ((old-backlog *backlog*)
        backlog)
    (handler-bind ((want-more
                    (lambda (c)
                      (unless (member :try (more-features c))
                        (push last-item backlog)
                        (signal 'want-more
                                :features (cons :try
                                                (more-features c)))))))
      (till-end ((setf *backlog* (if (> (length old-backlog) (length backlog))
                                     old-backlog
                                     (nreverse backlog))
                       *source* :backlog)
                 nil)
        (funcall parser)))))

;;; end