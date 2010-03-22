;;; CL-PARSEC item-level parsers

(in-package :parsec)

;; These are basic LL(1) parsers. It's possible to extend beyond that,
;; if you accept parsers instead of items (characters) as params

(defparser skip (test)
  (parsecall test)
  nil)

(defparser skip-many (test)
  (if-end nil
    (loop (parsecall test))))

(defparser skip-many+ (test)
  (if-end (?!)
    (parsecall test))
  (skip-many test))

(defparser look-ahead (test)
  "LL(1) lookahead"
  (progn (parsecall test)
         (unread-last-item)
         t))

;;; end