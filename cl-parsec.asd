;;; CL-PARSEC system definition
;;; (c) Vsevolod Dyomkin.  See LICENSE file for permissions

(in-package :asdf)

(defsystem #:cl-parsec
  :name "Parser combinators"
  :version '(0 1 1)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Parser combinators in plain ANSI CL without monads."
  :depends-on (:rutils)
  :serial t
  :components ((:file "packages")
               (:file "core")
               (:file "simple")
               (:file "item-level")
               (:file "higher-order")))


#+:nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-redis))))
  (operate 'load-op '#:cl-parsec)
  (operate 'test-op '#:cl-parsec-test :force t))

#+:nuts
(defsystem #:cl-parsec-test
  :name "CL-PARSEC testsuite"
  :version '(0 0 1)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description ""
  :depends-on (:cl-parsec :nuts)
  :serial t
  :components ((:file "test")))

#+:nuts
(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-parsec-test))))
  (operate 'load-op '#:cl-parsec-test)
  (funcall (intern (symbol-name 'run-tests)
                   '#:parsec-test)))

;;; end
