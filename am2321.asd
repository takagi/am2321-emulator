#|
  This file is a part of am2321 project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage am2321-asd
  (:use :cl :asdf))
(in-package :am2321-asd)

(defsystem am2321
  :version "0.1"
  :author "Masayuki Takagi"
  :license "LLGPL License"
  :depends-on (:alexandria :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "am2321"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op am2321-test))))
