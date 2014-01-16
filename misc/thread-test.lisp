(defvar *prev* :master)
(defvar *cnt* 0)
(defvar *master-cnt* 0)
(defvar *slave-cnt* 0)

(defun run-master ()
  (bt:make-thread #'(lambda ()
    (loop
      (incf *master-cnt*)
      (ecase *prev*
        (:master (incf *cnt*))
        (:slave (when (> *cnt* 10000)
                  (format t "Slave succeeded ~S times.~%" *cnt*)
                  (force-output))
                (setf *prev* :master)
                      *cnt* 1))))))

(defun run-slave ()
  (bt:make-thread #'(lambda ()
    (loop
      (incf *slave-cnt*)
      (ecase *prev*
        (:slave (incf *cnt*))
        (:master (when (> *cnt* 10000)
                   (format t "Master succeeded ~S times.~%" *cnt*)
                   (force-output))
                 (setf *prev* :slave
                       *cnt* 1)))))))
