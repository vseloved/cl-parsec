(in-package :parsec-test)

(defparser spaces ()
  (skip-many #\Space))

(defparser expression ()
  (prog1 (subexpression)
    (spaces)
    (eof)))

(defparser subexpression ()
  (spaces)
  (many+ #'element))

(defparser element ()
  (spaces)
  (either #`(try #'func)
          #'token
          #'operator))

(defparser token ()
  (list :atom
        (coerce (many+ #`(parsecall #'alphanumericp))
                'string)))

(defparser operator ()
  (list :op
        (parsecall '(member (#\- #\+ #\* #\/ #\^ #\.)))))

(defvar *more-func-args* nil)

(defparser func-arg ()
  (prog1 (subexpression)
    (setf *more-func-args* nil)
    (spaces)
    (either #`(look-ahead #\))
            #`((parsecall #\,)
               (setf *more-func-args* t)))))

(defparser func-end ()
  (spaces)
  (parsecall #\)))

(defparser func ()
  (nconc (list :func
               (second (prog1 (token)
                         (spaces)
                         (parsecall #\())))
         (let (*more-func-args*)
           (prog1 (many #'func-arg)
             (when *more-func-args*
               (?!))
             (func-end)))))

;;; end