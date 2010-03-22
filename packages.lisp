;;; CL-PARSEC package definition

(defpackage :cl-parsec
  (:use :common-lisp :rutils.usr)
  (:nicknames :parsec)
  (:export :defparser
           :mkparser
           :mk-if-macro
           :parse
           :parsecall
           :parse-test
           :_parser-name_

           :next-item
           :read-item
           :unread-item
           :unread-last-item

           :*echo-p*
           :*echo-stream*

           :parsec-error
           :parsec-signal
           :parsec-success
           :parsing-result
           :parsing-backlog
           :signal-features
           :try-success
           :?!

           :intercept-signals
           :_this-signal_

           ;; parsers
           :either
           :eof
           :look-ahead
           :many
           :many+
           :maybe
           :skip
           :skip-many
           :skip-many+
           :try
           ))

(defpackage :cl-parsec-test
  (:use :common-lisp :rutils.usr
        :cl-parsec)
  (:nicknames :parsec-test)
  (:export :run-tests))

;;; end

