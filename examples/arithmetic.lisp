(in-package :parsec-test)

(defparser expression ()
  (skip-many #\Space)
  (prog1 (many+ #'tok)
    (skip-many #\Space)
    (eof)))

(defparser tok ()
  (skip-many #\Space)
  (either (mkparser #'digit-char-p)
          (mkparser #\( :lparen)
          (mkparser #\) :rparen)
          #'operator))

(defparser operator ()
  (list :op (string (either (mkparser #\-)
                            (mkparser #\+)
                            (mkparser #\*)
                            (mkparser #\/)))))