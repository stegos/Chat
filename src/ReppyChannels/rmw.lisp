;; rmw.lisp -- Synchronous Key-Value service via Reppy Channels
;;
;; Synchronous is much simpler than asynchronous code. But synchronous
;; server ties up a thread.
;;
;; DM/RAL  11/19
;; ---------------------------------------------------------------------------------------------

(defpackage :rmw
  (:use :cl :rch)
  (:import-from :actors
   :pr)
  (:import-from :useful-macros
   :dcase
   :curry)
  (:export
   :make-sync-var-server
   :kill-server
   :get-var
   :set-var
   :rmw-var
   :rem-var
   :get-vars
   :set-vars
   :rmw-vars
   :rem-vars
   :transact
   :transaction-failure
   ))

(in-package :rmw)

;; -------------------------------------------------------------------
;; Sync Thread-based Key/Value server

(defvar *vartbl*  (make-hash-table
                   :test #'equal)) ;; to allow for case-sensitive strings as keys

(defvar *timeout* 1)

(defun timed-evt (ev &key (msg "Channel timeout") (timeout *timeout*))
  (select* ev
           (wrap (timerEvt timeout)
                 (lambda (_)
                   (declare (ignore _))
                   (pr msg)) ;; returns NIL
                 )))

(defun run-sync-var-server (reqCh)
  ;; synchronous code - needs to run as a process Thread
  (labels ((get-name (name default)
             (gethash name *vartbl* default))
           (set-name (name val)
             (setf (gethash name *vartbl*) val))
           (remove-name (name)
             (remhash name *vartbl*))

           (get-names (names default)
             (mapcar (lambda (name)
                       `(,name ,(get-name name default)))
                     names))
           (set-names (pairs)
             (dolist (pair pairs pairs)
               (apply #'set-name pair)))
           (remove-names (names)
             (dolist (name names t)
               (remove-name name))))
    (loop do
          (sync (wrap (recvEvt reqCh)
                      (lambda (msg)
                        (dcase msg
                          (:gets (replyCh names default)
                           (timed-evt (sendEvt replyCh (get-names names default))
                                      :msg  "Server GETS send timeout"))
                          (:sets (pairs)
                           (set-names pairs))
                          (:rmws (replyCh names default)
                           (timed-evt (sendEvt replyCh (get-names names default))
                                      :msg "Server RMW send timeout")
                           (timed-evt
                            (wrap (recvEvt replyCh)
                                  (lambda (pairs)
                                    (when (consp pairs)
                                      (set-names pairs))))
                            :msg "Server RMW recv timeout"))
                          (:rems (names)
                           (remove-names names))
                          (:kill ()
                           (loop-finish))
                          )))
                ))))

;; ----------------------------------------------------------------------------

(defvar *var-server*     nil)
(defvar *server-channel* nil)

(defun kill-server ()
  (when *var-server*
    (timed-evt
     (wrap-abort
      (sendEvt *server-channel* `(:kill))
      (lambda ()
        (mp:process-terminate *var-server*))))
    (setf *var-server*     nil
          *server-channel* nil)))

(defun make-sync-var-server ()
  (kill-server)
  (let ((reqCh  (make-channel)))
    (setf *var-server*     (spawn-process (curry #'run-sync-var-server reqCh)
                                          :name "SyncVarServer")
          *server-channel* reqCh)))

;; ----------------------------------------------------------------------------

(defun get-vars (names &optional default)
  (let ((replyCh (make-channel)))
    (send *server-channel* `(:gets ,replyCh ,names ,default))
    (timed-evt (recvEvt replyCh)
               :msg "Client GET-VARS recv timeout")))

(defun set-vars (names)
  (send *server-channel* `(:sets ,names))
  names)

(defun rmw-vars (names fn &optional default)
  (let ((replyCh (make-channel)))
    (send *server-channel* `(:rmws ,replyCh ,names ,default))
    (timed-evt (wrap
                (recvEvt replyCh)
                (lambda (pairs)
                  (handler-case
                      (timed-evt (sendEvt replyCh (funcall fn pairs))
                                 :msg "Client RMW send timeout")
                    (error (e)
                      (poke replyCh nil)
                      (error e))
                    )))
               :msg "Client RMW recv timeout")
    ))

(defun rem-vars (names)
  (send *server-channel* `(:rems ,names)))

;; ----------------------------------------------------------------------------

(defun return-item-val (pairs)
  ;; return the val from association in the single-association list
  (when (consp pairs)
    (destructuring-bind ((_ val)) pairs
      (declare (ignore _))
      val)))

(defun get-var (name &optional default)
  (return-item-val (get-vars `(,name) default)))

(defun set-var (name val)
  (return-item-val (set-vars `((,name ,val)))))

(defun rmw-var (name fn &optional default)
  (return-item-val (rmw-vars `(,name)
                             (lambda (pairs)
                               (destructuring-bind (_ val) (car pairs)
                                 (declare (ignore _))
                                 `((,name ,(funcall fn val)))
                                 ))
                             default)))
 
(defun rem-var (name)
  (rem-vars `(,name)))

;; ---------------------------------------------------------

(define-condition rollback (error)
  ())

(define-condition transaction-failure (error)
  ())

(defun transact (names fn &key default (retries 5))
  ;; The names arg is a list of input vars that the result will depend
  ;; upon. We grab copies of them, perform some work, then just before
  ;; sending back the results we double check quickly that the inputs
  ;; have not changed.
  (let ((old-pairs  (get-vars names default))
        (ok         t)) ;; a non-list
    (loop repeat retries do
          (let* ((new-pairs (funcall fn old-pairs))
                 (checker   (lambda (cur-pairs)
                              (cond ((every (lambda (old cur)
                                              (eql (cadr old) (cadr cur)))
                                            old-pairs cur-pairs)
                                     ;; data has not changed, so commit mutations
                                     new-pairs)
                                    (t
                                     ;; data has changed beneath us - rollback
                                     (setf old-pairs cur-pairs)
                                     (error (make-condition 'rollback)))))
                            ))
            (handler-case
                (progn
                  (setf ok (rmw-vars names checker default))
                  (loop-finish))
              
              (rollback ())
              )))
    (if (listp ok) ;; allows for null list as valid, but pointless, answer
        ok
      ;; else
      (error (make-condition 'transaction-failure)))
    ))

;; --------------------------------------------------------------------------

(defun tst ()
  (labels ((incr (x)
             (1+ x))
           (tstr (ix)
             (loop repeat 5 do
                   (pr (format nil "proc ~A  x = ~A" ix (rmw-var :x #'incr 15)))
                   (sleep 0))))
    (rem-var :x)
    (dotimes (ix 10)
      (spawn-process (curry #'tstr ix))
      )))

#|
(transact '(:a :b :c)
          (lambda (_)
            '((:a 1) (:b 2) (:c 3))))

(rem-vars '(:a :b :c))

|#

(defun dropout-test (&optional (n 100))
  (pr "start")
  (loop repeat n do
        (rmw-var :x 'identity 15)
        (get-var :x)
        (set-var :x 15)
        (rem-var :x)
        )
  (pr :finished))
