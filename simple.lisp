;;; CL-PARSEC simple parsers (not taking any arguments)

(in-package :cl-parsec)

(defparser eof ()
  (if-eof (return-from eof t)
    (next-item))
  (?!))

;;; end
