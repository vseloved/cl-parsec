;;; http://dying-sphynx.livejournal.com/66854.html

(in-package :parsec-test)

(defparser expression ()
  (skip-many #\Space)
  (prog1 (many+ #'tok)
    (eof)))

(defparser tok ()
  (prog1 (either #`(try #'func)
                 #'item
                 #'operator
                 (mkparser #\( :lparen)
                 (mkparser #\) :rparen)
                 (mkparser #\, :comma))
    (skip-many #\Space)))

(defparser item ()
  (list :atom (coerce (many+ (mkparser #'alphanumericp))
                      'string)))

(defparser operator ()
  (list :op (string (either (mkparser #\-)
                            (mkparser #\+)
                            (mkparser #\*)
                            (mkparser #\/)
                            (mkparser #\^)
                            (mkparser #\.)))))

(defparser func ()
  (list :func (coerce (second (prog1 (item)
                                (skip-many #\Space)
                                (look-ahead #\()))
                      'string)))

;;; end