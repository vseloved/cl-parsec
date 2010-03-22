(in-package :parsec-test)

(defparser spaces ()
  (skip-many #\Space))

(defparser expression ()
  (prog1 (many+ #'tok)
    (spaces)
    (eof)))

(defparser tok ()
  (spaces)
  (either #`(parse-integer (coerce (many+ #`(parsecall #'digit-char-p))
                                   'string))
          #`(parsecall #\( :lparen)
          #`(parsecall #\) :rparen)
          #'operator))

(defparser operator ()
  (list :op (parsecall '(member (#\- #\+ #\* #\/)))))