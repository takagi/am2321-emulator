#|
  This file is a part of am2321 project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage am2321
  (:use :cl)
  (:export #:run
           #:run-master
           #:run-slave))
(in-package :am2321)


;;;
;;; I2C Lines
;;;

(defvar *sda* :low)
(defvar *scl* :low)


;;;
;;; Registers
;;;

(defstruct (register (:constructor %make-register))
  pc tags entry-pc entry-tags stack
  r0 r1 r2 r3
  ret)

(defun make-register (pc tags)
  (%make-register :pc pc :tags tags :entry-pc pc :entry-tags tags))

(defvar *current*)                  
(defvar *master*)
(defvar *slave*)

(define-symbol-macro *pc* (register-pc *current*))
(define-symbol-macro *tags* (register-tags *current*))
(define-symbol-macro *entry-pc* (register-entry-pc *current*))
(define-symbol-macro *entry-tags* (register-entry-tags *current*))
(define-symbol-macro *stack* (register-stack *current*))
(define-symbol-macro *r0* (register-r0 *current*))
(define-symbol-macro *r1* (register-r1 *current*))
(define-symbol-macro *r2* (register-r2 *current*))
(define-symbol-macro *r3* (register-r3 *current*))
(define-symbol-macro *ret* (register-ret *current*))


;;;
;;; Low level interfaces
;;;

(defun high (pin)
  (eql pin :high))

(defun low (pin)
  (eql pin :low))

(defun set-sda (value)
  (assert (member value '(:high :low)))
  (let ((modified (not (eql *sda* value))))
    (setf *sda* value)
    (values *sda* modified)))

(defun set-scl (value)
  (assert (member value '(:high :low)))
  (let ((modified (not (eql *scl* value))))
    (setf *scl* value)
    (values *scl* modified)))

(defun set-reg (reg value)
  (ecase reg
    (:r0 (setf *r0* value))
    (:r1 (setf *r1* value))
    (:r2 (setf *r2* value))
    (:r3 (setf *r3* value))
    (:ret (setf *ret* value))))


;;;
;;; Tags
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-tags (insts)
    (let (tags)
      (do ((pc insts (cdr pc)))
          ((null pc))
        (let ((inst (car pc))
              (rest (cdr pc)))
          (if (keywordp inst)
              (push (cons inst (remove-tags rest))
                    tags))))
      tags)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-tags (insts)
    (remove-if #'keywordp insts)))

(defun tags-pc (tags tag)
  (let ((result (assoc tag tags)))
    (unless result
      (error "The tag ~S not found." tag))
    (cdr result)))


;;;
;;; Program counter
;;;

(defun inc-pc ()
  (setf *pc* (cdr *pc*)))

(defun jump-pc (tag)
  (setf *pc* (tags-pc *tags* tag)))

(defun reset-pc ()
  (setf *pc* *entry-pc*
        *tags* *entry-tags*
        *stack* nil))


;;;
;;; Stack
;;;

(defun stack-push-frame ()
  (let ((frame (list :pc *pc* :tags *tags*
                     :r0 *r0* :r1 *r1* :r2 *r2* :r3 *r3*)))
    (push frame *stack*)))

(defun stack-pop-frame ()
  (let ((frame (pop *stack*)))
    (values (getf frame :pc) (getf frame :tags)
            (getf frame :r0) (getf frame :r1)
            (getf frame :r2) (getf frame :r3))))


;;;
;;; Sub routines
;;;

(defmacro defsub (name args &body insts)
  (alexandria:with-gensyms (insts1 pc tags)
    `(progn
       (unless (listp ',args)
         (error 'type-error :datum ',args :expected-type 'list))
       (when (subroutine-macro-p ',name)
         (undef-subroutine-macro ',name))
       (let ((,insts1 (expand-submacro ',insts)))
         (let ((,pc (remove-tags ,insts1))
               (,tags (make-tags ,insts1)))
           (setf (get ',name 'defsub) t
                 (get ',name 'pc) ,pc
                 (get ',name 'tags) ,tags
                 (get ',name 'args) ',args))))))

(defun undef-subroutine (name)
  (remprop name 'defsub)
  (remprop name 'pc)
  (remprop name 'tags)
  (remprop name 'args))

(defun subroutine-p (name)
  (and (get name 'defsub)
       t))

(defun subroutine-pc (name)
  (unless (subroutine-p name)
    (error "The sub routine ~S not found." name))
  (get name 'pc))

(defun subroutine-tags (name)
  (unless (subroutine-p name)
    (error "The sub routine ~S not found." name))
  (get name 'tags))

(defun subroutine-args (name)
  (unless (subroutine-p name)
    (error "The sub routine ~S not found." name))
  (get name 'args))


;;;
;;; Subroutine macros
;;;

(defmacro defsubmacro (name args &body body)
  `(progn
     (unless (listp ',args)
       (error 'type-error :datum ',args :expected-type 'list))
     (when (subroutine-p ',name)
       (undef-subroutine ',name))
     (setf (get ',name 'defsubmacro)
           #'(lambda ,args ,@body))))

(defun undef-subroutine-macro (name)
  (remprop name 'defsubmacro))

(defun subroutine-macro-p (name)
  (and (get name 'defsubmacro)
       t))

(defun subroutine-macro-expander (name)
  (unless (subroutine-macro-p name)
    (error "The subroutine macro ~S not found." name))
  (get name 'defsubmacro))


;;;
;;; Subroutine macro expansion
;;;

(defun unique-keyword (keyword)
  (unless (keywordp keyword)
    (error 'type-error :datum keyword :expected-type 'keyword))
  (alexandria:make-keyword (gensym (princ-to-string keyword))))

(defun make-unique-tag ()
  (let (tags)
    #'(lambda (tag)
        (unless (keywordp tag)
          (error 'type-error :datum tag :expected-type 'keyword))
        (or (getf tags tag)
            (setf (getf tags tag) (unique-keyword tag))))))

(defmacro with-unique-tags (names &body body)
  (alexandria:with-gensyms (unique-tag)
    `(let ((,unique-tag (make-unique-tag)))
       (let ,(mapcar #'(lambda (name)
               `(,name (funcall ,unique-tag
                                (alexandria:make-keyword ',name))))
               names)
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-submacro (insts)
    (loop for inst in insts
       append
         (if (listp inst)
             (destructuring-bind (name . args) inst
               (if (subroutine-macro-p name)
                   (expand-submacro
                     (apply (subroutine-macro-expander name) args))
                   (list inst)))
             (list inst)))))


;;;
;;; Eval instruction
;;;

(defun eval-instruction ()
  (let ((inst (car *pc*)))
    (ecase (car inst)
      (set-sda (eval-set-sda inst))
      (set-scl (eval-set-scl inst))
      (set-reg (eval-set-reg inst))
      (jump (eval-jump inst))
      (if-jump (eval-if-jump inst))
      (wait (eval-wait inst))
      (call (eval-call inst))
      (return (eval-return inst))
      (reset (eval-reset inst))
      (error (eval-error inst))
      (print (eval-print inst)))))

(defun eval-set-sda (inst)
  (destructuring-bind (set-sda value) inst
    (unless (eql set-sda 'set-sda)
      (error "The instruction ~S is invalid." inst))
    (set-sda value)
    (inc-pc)))

(defun eval-set-scl (inst)
  (destructuring-bind (set-scl value) inst
    (unless (eql set-scl 'set-scl)
      (error "The instruction ~S is invalid." inst))
    (set-scl value)
    (inc-pc)))

(defun eval-set-reg (inst)
  (destructuring-bind (set-reg reg value) inst
    (unless (eql set-reg 'set-reg)
      (error "The instruction ~S is invalid." inst))
    (set-reg reg (eval value))
    (inc-pc)))
      
(defun eval-jump (inst)
  (destructuring-bind (jump tag) inst
    (unless (eql jump 'jump)
      (error "The instruction ~S is invalid." inst))
    (jump-pc tag)))

(defun eval-if-jump (inst)
  (destructuring-bind (if-jump pred tag) inst
    (unless (eql if-jump 'if-jump)
      (error "The instruction ~S is invalid." inst))
    (if (eval pred)
        (jump-pc tag)
        (inc-pc))))

(defun eval-wait (inst)
  (destructuring-bind (wait pred) inst
    (unless (eql wait 'wait)
      (error "The instruction ~S is invalid." inst))
    (if (eval pred)
        (inc-pc))))

(defun eval-call (inst)
  (destructuring-bind (call name . args) inst
    (unless (eql call 'call)
      (error "The instruction ~S is invalid." inst))
    (unless (alexandria:length= args (subroutine-args name))
      (error "Invalid number of arguments: ~S" (length args)))
    (let ((args1 (mapcar #'eval args)))
      (stack-push-frame)
      (setf *pc* (subroutine-pc name)
            *tags* (subroutine-tags name))
      (set-reg :r0 nil)
      (set-reg :r1 nil)
      (set-reg :r2 nil)
      (set-reg :r3 nil)
      (set-reg :ret nil)
      (loop for reg in (subroutine-args name)
            for val in args1
         do (set-reg reg val)))))

(defun eval-return (inst)
  (destructuring-bind (return . value) inst
    (unless (eql return 'return)
      (error "The instruction ~S is invalid." inst))
    (when value
      (set-reg :ret (eval (car value)))))
    (multiple-value-bind (pc tags r0 r1 r2 r3) (stack-pop-frame)
      (setf *pc* pc
            *tags* tags)
      (set-reg :r0 r0)
      (set-reg :r1 r1)
      (set-reg :r2 r2)
      (set-reg :r3 r3)
    (inc-pc)))

(defun eval-reset (inst)
  (destructuring-bind (reset) inst
    (unless (eql reset 'reset)
      (error "The instruction ~S is invalid." inst))
    (reset-pc)))

(defun eval-error (inst)
  (destructuring-bind (error . rest) inst
    (unless (eql error 'error)
      (error "The instruction ~S is invalid." inst))
    (let ((rest1 (mapcar #'eval rest)))
      (apply #'error rest1))))

(defun eval-print (inst)
  (destructuring-bind (print . rest) inst
    (unless (eql print 'print)
      (error "The instruction ~S is invalid." inst))
    (let ((rest1 (mapcar #'eval rest)))
      (apply #'format `(t ,@rest1))
      (inc-pc))))


;;;
;;; Defining Subroutine macros
;;;

(defsubmacro if-not-jump (pred tag)
  `((if-jump (not ,pred) ,tag)))

(defsubmacro when (test &rest body)
  `((unless (not ,test)
      ,@body)))

(defsubmacro unless (test &rest body)
  (with-unique-tags (end-tag)
    `((if-jump ,test ,end-tag)
      ,@body
      ,end-tag)))

(defsubmacro block (&rest body)
  body)

(defsubmacro if (predicate consequent alternative)
  (with-unique-tags (else-tag end-tag)
    `((if-not-jump ,predicate ,else-tag)
      ,consequent
      (jump ,end-tag)
      ,else-tag
      ,alternative
      ,end-tag)))

(defsubmacro follow (pred)
  `((set-reg :r0 *sda*)
    (set-reg :r1 *scl*)
    (wait (not (and (eql *sda* *r0*) (eql *scl* *r1*))))
    (unless ,pred
      (error "[SLAVE] Failed to follow.~%"))))


;;;
;;; Slave
;;;

(defsub wait-start-condition ()
  (wait (and (high *sda*) (high *scl*)))
  (follow (and (low *sda*) (high *scl*)))
  (follow (and (low *sda*) (low *scl*)))
  (print "[SLAVE] Start condition received.~%")
  (return))

(defsub wait-stop-condition ()
  (wait (and (low *sda*) (low *scl*)))
  (follow (and (low *sda*) (high *scl*)))
  (follow (and (high *sda*) (high *scl*)))
  (print "[SLAVE] Stop condition received.~%")
  (return))

(defsub slave-send-ack ()
  (set-sda :low)
  (wait (high *scl*))
  (wait (low *scl*))
  (return))

(defsub slave-read-bit ()
  (wait (high *scl*))
  (set-reg :r0 (if (high *sda*) 1 0))
;  (print "[SLAVE] Read bit: ~S~%" *ret*)
  (wait (low *scl*))
  (return *r0*))

(defsub slave-read-byte ()
  (set-reg :r0 0)
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call slave-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (print "[SLAVE] Read byte: ~S~%" *r0*)
  (call slave-send-ack)
  (return *r0*))

;; :r0 - ACK/NACK
(defsub slave-receive-ack (:r0)
  (wait (high *scl*))
  (if (not *r0*)
    (block                              ; ACK
      (unless (low *sda*)
        (error "[SLAVE] miss ACK.~%"))
      (print "[SLAVE] ACK received.~%"))
    (block                              ; NACK
      (unless (high *sda*)
        (error "[SLAVE] miss NACK.~%"))
      (print "[SLAVE] NACK received.~%")))
  (wait (low *scl*))
  (return))

;; :r0 - bit to be written
(defsub slave-write-bit (:r0)
  (unless (member *r0* '(0 1) :test #'=)
    (error "[SLAVE] The value ~S is invalid." *r0*))
  (if (= *r0* 0)
    (set-sda :low)
    (set-sda :high))
  (wait (high *scl*))
  (wait (low *scl*))
  (return))

;; :r0 - byte to be written
;; :r1 - ACK/NACK
(defsub slave-write-byte (:r0 :r1)
  (print "[SLAVE] Sending byte: ~S~%" *r0*)
  (call slave-write-bit (ldb (byte 1 7) *r0*))
  (call slave-write-bit (ldb (byte 1 6) *r0*))
  (call slave-write-bit (ldb (byte 1 5) *r0*))
  (call slave-write-bit (ldb (byte 1 4) *r0*))
  (call slave-write-bit (ldb (byte 1 3) *r0*))
  (call slave-write-bit (ldb (byte 1 2) *r0*))
  (call slave-write-bit (ldb (byte 1 1) *r0*))
  (call slave-write-bit (ldb (byte 1 0) *r0*))
  (call slave-receive-ack *r1*)
  (return))

(defsub slave ()
  (print "[SLAVE] Ready.~%")
  ;; request to measure temperature
  (call wait-start-condition)
  (call slave-read-byte)
  (call slave-read-byte)
  (call slave-read-byte)
  (call slave-read-byte)
  (call wait-stop-condition)
  ;; receive measured temperature
  (call wait-start-condition)
  (call slave-read-byte)
  (call slave-write-byte #X03 nil)
  (call slave-write-byte #X02 nil)
  (call slave-write-byte #X00 nil)
  (call slave-write-byte #X02 nil)
  (call slave-write-byte #XFF nil)
  (call slave-write-byte #XFF t)
  (call wait-stop-condition)
  (reset))


;;;
;;; Master
;;;

(defsub delay ()
  (set-reg :r0 0)
  :loop
  (if-not-jump (< *r0* 1000) :endloop)
  (set-reg :r0 (+ *r0* 1))
  (jump :loop)
  :endloop
  (return))

(defsub send-start-condition ()
  (print "[MASTER] Sending start condition.~%")
  (set-sda :high) (call delay)
  (set-scl :high) (call delay)
  (set-sda :low) (call delay)
  (set-scl :low) (call delay)
  (return))

(defsub send-stop-condition ()
  (print "[MASTER] Sending stop condition.~%")
  (set-sda :low) (set-scl :low) (call delay)
  (set-scl :high) (call delay)
  (set-sda :high) (call delay)
  (return))

(defsub master-receive-ack ()
  (set-scl :high) (call delay)
  (unless (low *sda*)
    (error "[MASTER] miss ACK.~%"))
  (print "[MASTER] ACK received.~%")
  (set-scl :low) (call delay)
  (return))

;; :r0 - bit to be written
(defsub master-write-bit (:r0)
  (unless (member *r0* '(0 1) :test #'=)
    (error "[MASTER] The value ~S is invalid." *r0*))
  (if (= *r0* 0)
      (set-sda :low)
      (set-sda :high))
  (set-scl :high) (call delay)
  (set-scl :low) (call delay)
  (return))

;; :r0 - byte to be written
(defsub master-write-byte (:r0)
  (print "[MASTER] Sending byte: ~S~%" *r0*)
  (call master-write-bit (ldb (byte 1 7) *r0*))
  (call master-write-bit (ldb (byte 1 6) *r0*))
  (call master-write-bit (ldb (byte 1 5) *r0*))
  (call master-write-bit (ldb (byte 1 4) *r0*))
  (call master-write-bit (ldb (byte 1 3) *r0*))
  (call master-write-bit (ldb (byte 1 2) *r0*))
  (call master-write-bit (ldb (byte 1 1) *r0*))
  (call master-write-bit (ldb (byte 1 0) *r0*))
  (call master-receive-ack)
  (return))

;; :r0 - ACK/NACK
(defsub master-send-ack (:r0)
  (if (null *r0*)
      (set-sda :low)
      (set-sda :high))
  (set-scl :high) (call delay)
  (set-scl :low) (call delay)
  (return))

(defsub master-read-bit ()
  (set-scl :high) (call delay)
  (set-reg :r0 (if (high *sda*) 1 0))
;  (print "[MASTER] Read bit: ~S~%" *ret*)
  (set-scl :low) (call delay)
  (return *r0*))

;; r0: ACK/NACK
(defsub master-read-byte (:r0)
  (set-reg :r1 0)
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (call master-read-bit) (set-reg :r1 (logior (ash *r1* 1) *ret*))
  (print "[MASTER] Read byte: ~S~%" *r1*)
  (call master-send-ack *r0*)
  (return *r1*))

(defsub master ()
  (call send-start-condition)
  (call master-write-byte #XB8)
  (call master-write-byte #X03)
  (call master-write-byte #X02)
  (call master-write-byte #X02)
  (call send-stop-condition)
  (call send-start-condition)
  (call master-write-byte #X09)
  (call master-read-byte nil)
  (call master-read-byte nil)
  (call master-read-byte nil)
  (call master-read-byte nil)
  (call master-read-byte nil)
  (call master-read-byte t)
  (call send-stop-condition)
  (reset))


;;;
;;; Main
;;;

(defun run ()
  (let ((*master* (make-register (subroutine-pc 'master)
                                 (subroutine-tags 'master)))
        (*slave* (make-register (subroutine-pc 'slave)
                                (subroutine-tags 'slave))))
    (loop
      (let ((*current* *slave*))
        (eval-instruction))
      (let ((*current* *master*))
        (eval-instruction)))))

(defun run-slave ()
  (bt:make-thread #'(lambda ()
    (setf *slave* (make-register (subroutine-pc 'slave)
                                 (subroutine-tags 'slave)))
    (loop while (register-pc *slave*)
       do (let ((*current* *slave*))
            (eval-instruction))))))

(defun run-master ()
  (bt:make-thread #'(lambda ()
    (setf *master* (make-register (subroutine-pc 'master)
                                  (subroutine-tags 'master)))
    (loop while (register-pc *master*)
       do (let ((*current* *master*))
            (eval-instruction))))))
