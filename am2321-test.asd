#|
  This file is a part of am2321 project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage am2321-test-asd
  (:use :cl :asdf))
(in-package :am2321-test-asd)

(defsystem am2321-test
  :author ""
  :license ""
  :depends-on (:am2321
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "am2321"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
