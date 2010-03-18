;;; CL-PARSEC item-level parsers

(in-package :parsec)

;; These are basic LL(1) parsers. It's possible to extend beyond that,
;; if you accept parsers instead of items (characters) as params

(locally-enable-literal-syntax :sharp-backq)

(defparser skip (test)
  (parsecall test)
  t)

(defparser skip-many (test)
  (till-end (t)
    (loop (parsecall test))))

(defparser skip-many+ (test)
  (till-end ((?!))
    (parsecall test))
  (skip-many test))

(defparser look-ahead (test)
  "LL(1) lookahead"
  (progn (parsecall test)
         (unread-item last-item *source*)
         t))

;;; end