;;; http://dying-sphynx.livejournal.com/66854.html

(in-package :parsec)

(defparser expression ()
  (prog1 (subexpression)
    (eof)))

(defparser subexpression ()
  (skip-many #\Space)
  (many+ #'element))

(defparser element ()
  (prog1 (either #`(try #'func)
                 #'token
                 #'operator)
    (skip-many #\Space)))

(defparser token ()
  (list :atom
        (coerce (many+ (mkparser #'alphanumericp))
                'string)))

(defparser operator ()
  (list :op
        (either (mkparser #\-)
                (mkparser #\+)
                (mkparser #\*)
                (mkparser #\/)
                (mkparser #\^)
                (mkparser #\.))))

(defparser func ()
  (nconc (list :func
               (second (prog1 (token)
                         (skip-many #\Space)
                         (parsecall #\())))
         (let ((args (prog1 (many #`(prog1 (subexpression)
                                      (either (mkparser #\,)
                                              #`(look-ahead #\)))))
                       (skip-many #\Space)
                       (parsecall #\)))))
           (when (listp args)
             args))))  

;;; end