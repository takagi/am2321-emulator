#|
  This file is a part of am2321 project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage am2321
  (:use :cl))
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
  pc tags stack entry
  r0 r1 r2 r3
  arg0 arg1 arg2 arg3
  ret)

(defun make-register (pc tags)
  (%make-register :pc pc :tags tags :entry pc))

(defvar *current*)                  
(defvar *master*)
(defvar *slave*)

(define-symbol-macro *pc* (register-pc *current*))
(define-symbol-macro *tags* (register-tags *current*))
(define-symbol-macro *stack* (register-stack *current*))
(define-symbol-macro *entry* (register-entry *current*))
(define-symbol-macro *r0* (register-r0 *current*))
(define-symbol-macro *r1* (register-r1 *current*))
(define-symbol-macro *r2* (register-r2 *current*))
(define-symbol-macro *r3* (register-r3 *current*))
(define-symbol-macro *arg0* (register-arg0 *current*))
(define-symbol-macro *arg1* (register-arg1 *current*))
(define-symbol-macro *arg2* (register-arg2 *current*))
(define-symbol-macro *arg3* (register-arg3 *current*))
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
    (:arg0 (setf *arg0* value))
    (:arg1 (setf *arg1* value))
    (:arg2 (setf *arg2* value))
    (:arg3 (setf *arg3* value))
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
  (setf *pc* *entry*))


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
  (declare (ignore args))
  (alexandria:with-gensyms (pc tags)
    `(let ((,pc (remove-tags ',insts))
           (,tags (make-tags ',insts)))
       (setf (get ',name 'defsub) t
             (get ',name 'pc) ,pc
             (get ',name 'tags) ,tags))))

(defun subroutine-pc (name)
  (unless (get name 'defsub)
    (error "The sub routine ~S not found." name))
  (get name 'pc))

(defun subroutine-tags (name)
  (unless (get name 'defsub)
    (error "The sub routine ~S not found." name))
  (get name 'tags))


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
  (destructuring-bind (call name) inst
    (unless (eql call 'call)
      (error "The instruction ~S is invalid." inst))
    (stack-push-frame)
    (setf *pc* (subroutine-pc name)
          *tags* (subroutine-tags name)
          *r0* nil
          *r1* nil
          *r2* nil
          *r3* nil)))

(defun eval-return (inst)
  (destructuring-bind (return) inst
    (unless (eql return 'return)
      (error "The instruction ~S is invalid." inst))
    (multiple-value-bind (pc tags r0 r1 r2 r3) (stack-pop-frame)
      (setf *pc* pc
            *tags* tags
            *r0* r0
            *r1* r1
            *r2* r2
            *r3* r3))
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
;;; Slave
;;;

(defsub wait-start-condition ()
  ;; SDA = :high and SCL = :high
  (wait (and (high *sda*) (high *scl*)))
  ;; SDA = :low and SCL = :high
  (wait (not (and (high *sda*) (high *scl*))))
  (if-jump (not (and (low *sda*) (high *scl*))) :failed-to-follow)
  ;; SDA = :low and SCL = :low
  (wait (not (and (low *sda*) (high *scl*))))
  (if-jump (not (and (low *sda*) (low *scl*))) :failed-to-follow)
  (print "[SLAVE] Start condition received.~%")
  (return)
  ;; Failed to follow
  :failed-to-follow
  (error "[SLAVE] Failed to follow.~%"))

;; (defsubmacro follow (pred)
;;   `((set-reg :r0 *sda*)
;;     (set-reg :r1 *scl*)
;;     (wait (not (and (eql *sda* *r0*) (eql *scl* *r1*))))
;;     (if-not-jump ,pred :failed-to-follow)
;;     (return)
;;     :failed-to-follow
;;     (error "[SLAVE] Failed to follow.~%")))

;; (defsub wait-start-condition ()
;;   (wait (and (high *sda*) (high *scl*)))
;;   (follow (and (low *sda*) (high *scl*)))
;;   (follow (and (low *sda*) (low *scl*)))
;;   (print "[SLAVE] Start condition received.~%")
;;   (return))

(defsub wait-stop-condition ()
  ;; SDA = :low and SCL = :low
  (wait (and (low *sda*) (low *scl*)))
  ;; SDA = :low and SCL = :high
  (wait (not (and (low *sda*) (low *scl*))))
  (if-jump (not (and (low *sda*) (high *scl*))) :failed-to-follow)
  ;; SDA = :high and SCL = :high
  (wait (not (and (low *sda*) (high *scl*))))
  (if-jump (not (and (high *sda*) (high *scl*))) :failed-to-follow)
  (print "[SLAVE] Stop condition received.~%")
  (return)
  ;; Failed to follow
  :failed-to-follow
  (error "[SLAVE] Failed to follow.~%"))

(defsub slave-send-ack ()
  (set-sda :low)
  (wait (high *scl*))
  (wait (low *scl*))
  (return))

(defsub slave-read-bit ()
  (wait (high *scl*))
  (set-reg :ret (if (high *sda*) 1 0))
  (wait (low *scl*))
;  (print "[SLAVE] Read bit: ~S~%" *ret*)
  (return))

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
  (return))

;; arg0: ACK/NACK
(defsub slave-receive-ack ()
  (wait (high *scl*))
  (if-jump *arg0* :nack)
  (if-jump (low *sda*) :received-ack)
  (error "[SLAVE] miss ACK.~%")
  :received-ack
  (print "[SLAVE] ACK received.~%")
  (jump :endif)
  :nack
  (if-jump (high *sda*) :received-nack)
  (error "[SLAVE] miss NACK.~%")
  :received-nack
  (print "[SLAVE] NACK received.~%")
  :endif
  (wait (low *scl*))
  (return))

;; arg0: bit to be written
(defsub slave-write-bit ()
  (if-jump (not (= *arg0* 0)) :high)
  (set-sda :low)
  (jump :endif)
  :high
  (if-jump (not (= *arg0* 1)) :error)
  (set-sda :high)
  (jump :endif)
  :error
  (error "[SLAVE] The value ~S is invalid.~%" *arg0*)
  :endif
  (wait (high *scl*))
  (wait (low *scl*))
  (return))

;; arg0: byte to be written, arg1: ACK/NACK
(defsub slave-write-byte ()
  (print "[SLAVE] Sending byte: ~S~%" *arg0*)
  (set-reg :r0 *arg0*)
  (set-reg :arg0 (ldb (byte 1 7) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 6) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 5) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 4) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 3) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 2) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 1) *r0*)) (call slave-write-bit)
  (set-reg :arg0 (ldb (byte 1 0) *r0*)) (call slave-write-bit)
  (set-reg :arg0 *arg1*) (call slave-receive-ack)
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
  (set-reg :arg0 #X03) (set-reg :arg1 nil)
  (call slave-write-byte)
  (set-reg :arg0 #X02) (set-reg :arg1 nil)
  (call slave-write-byte)
  (set-reg :arg0 #X00) (set-reg :arg1 nil)
  (call slave-write-byte)
  (set-reg :arg0 #XA2) (set-reg :arg1 nil)
  (call slave-write-byte)
  (set-reg :arg0 #XFF) (set-reg :arg1 nil)
  (call slave-write-byte)
  (set-reg :arg0 #XFF) (set-reg :arg1 t)
  (call slave-write-byte)
  (call wait-stop-condition)
  (reset))


;;;
;;; Master
;;;

(defsub delay ()
  (set-reg :r0 0)
  :loop
  (if-jump (not (< *r0* 1000)) :endloop)
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
  (if-jump (not (low *sda*)) :error)
  (print "[MASTER] ACK received.~%")
  (set-scl :low) (call delay)
  (return)
  :error
  (error "[MASTER] miss ACK.~%"))

;; arg0: bit to be written
(defsub master-write-bit ()
  (if-jump (not (= *arg0* 1)) :low)
  (set-sda :high)
  (jump :endif)
  :low
  (set-sda :low)
  :endif
  (set-scl :high) (call delay)
  (set-scl :low) (call delay)
  (return))

;; arg0: byte to be written
(defsub master-write-byte ()
  (print "[MASTER] Sending byte: ~S~%" *arg0*)
  (set-reg :r0 *arg0*)
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (set-reg :r0 (ash *r0* 1))
  (set-reg :arg0 (ldb (byte 1 7) *r0*))
  (call master-write-bit)
  (call master-receive-ack)
  (return))

(defsub master-send-ack ()
  (if-jump (not (null *arg0*)) :nack)
  (set-sda :low)
  (jump :endif)
  :nack
  (set-sda :high)
  :endif
  (set-scl :high) (call delay)
  (set-scl :low) (call delay)
  (return))

(defsub master-read-bit ()
  (set-scl :high) (call delay)
  (if-jump (not (high *sda*)) :low)
  (set-reg :ret 1)
  (jump :endif)
  :low
  (set-reg :ret 0)
  :endif
;  (print "[MASTER] Read bit: ~S~%" *ret*)
  (set-scl :low) (call delay)
  (return))

;; arg0: ACK/NACK
(defsub master-read-byte ()
  (set-reg :r0 0)
  (set-reg :r1 *arg0*)
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (call master-read-bit) (set-reg :r0 (logior (ash *r0* 1) *ret*))
  (print "[MASTER] Read byte: ~S~%" *r0*)
  (set-reg :arg0 *r1*) (call master-send-ack)
  (return))

(defsub master ()
  (call send-start-condition)
  (set-reg :arg0 #XB8) (call master-write-byte)
  (set-reg :arg0 #X03) (call master-write-byte)
  (set-reg :arg0 #X02) (call master-write-byte)
  (set-reg :arg0 #X02) (call master-write-byte)
  (call send-stop-condition)
  (call send-start-condition)
  (set-reg :arg0 #XB9) (call master-write-byte)
  (set-reg :arg0 nil) (call master-read-byte)
  (set-reg :arg0 nil) (call master-read-byte)
  (set-reg :arg0 nil) (call master-read-byte)
  (set-reg :arg0 nil) (call master-read-byte)
  (set-reg :arg0 nil) (call master-read-byte)
  (set-reg :arg0 t) (call master-read-byte)
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