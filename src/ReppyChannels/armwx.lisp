;; armw.lisp -- Asynchronous Key-Value service via Reppy Channels
;;
;; Synchronous is much simpler than asynchronous code. But synchronous
;; server ties up a thread.
;;
;; In asynchronous code, nothing has any effect unless it happens in a
;; callback routine. Nothing useful is returned to Actor clients in
;; direct calls to these functions. But non-Actor thread clients can
;; obtain useful information by surrounding the function calls with
;; (WITH-CONT (functionm call...))
;;
;; DM/RAL  11/19
;; ---------------------------------------------------------------------------------------------


(defpackage :armwx
  (:use :cl :rch)
  (:import-from :ac
   :make-actor
   :=defun
   :with-future
   :with-indefinite-future
   :with-cont
   :=values
   :=values-callback
   :terminate-actor
   :on-failure
   :*futures-timeout*
   :forcibly-unblocked-p
   :handle-fail
   :pr)
  (:import-from :useful-macros
   :dlambda
   :nlet)
  (:export
   :make-async-var-server
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

(in-package :armwx)

;; -------------------------------------------------------------------
;; ASync Actor-based Key/Value server

(defvar *vartbl*  (make-hash-table
                   :test #'equal)) ;; to allow for case-sensitive strings as keys

(defvar *timeout* 1)
(defvar *initial-timeout* 2)

(=defun timed-evt (ev &key (msg "Channel timeout") (timeout *timeout*))
  (async (cancelAfterEvt timeout ev :msg msg)))

(defvar *var-server*  nil)

(defun make-async-var-server ()
  ;; asynchronous code - intended to run as an Actor
  ;;
  ;; Now that we have blocking Actors, pending callbacks,
  ;; we can provide a hybrid server based on DLAMBDA, and using
  ;; private channels when we need responses.
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
    ;; the main Actor body code-char
    (setf *var-server*
          (make-actor
           (dlambda
             (:gets (replyCh names default)
              (with-cont
                (timed-evt (sendEvt replyCh (get-names names default))
                           :msg "Server GETS send timeout")))
             (:sets (pairs)
              (set-names pairs))
             (:rmws (replyCh names default)
              (with-future (ans)
                  (timed-evt (sendEvt replyCh (get-names names default))
                             :msg "Server RMW send timeout")
                (when (had-rendezvous ans)
                  (with-future (pairs)
                      (timed-evt (recvEvt replyCh)
                                 :msg "Server RMW recv timeout")
                    (when (consp pairs)
                      (set-names pairs)))
                  )))
             (:rems (names)
              (remove-names names))
             )))))

;; ----------------------------------------------------------------------------
;; From Lisetener, call all of these functions using (WITH-CONT (fn...))

(=defun get-vars (names &optional default)
  (let ((replyCh (make-channel)))
    (ac:send *var-server* :gets replyCh names default)
    (timed-evt (recvEvt replyCh)
               :msg "Client GET-VARS recv timeout"
               :timeout *initial-timeout*)
    ))

(defun set-vars (pairs)
  (ac:send *var-server* :sets pairs)
  pairs)

(=defun rmw-vars (names fn &optional default)
  (let ((replyCh (make-channel)))
    (ac:send *var-server* :rmws replyCh names default)
    (with-future (ans)
        (timed-evt (recvEvt replyCh)
                   :msg "Client RMW-VARS recv timeout"
                   :timeout *initial-timeout*)
      (if (had-rendezvous ans)
          (handler-case
              (let ((pairs (funcall fn ans))) ;; this could trigger errors
                (timed-evt (sendEvt replyCh pairs)
                           :msg "Client RMW-VARS send timeout"))
            (error (e)
              (with-future ()
                  (timed-evt (sendEvt replyCh nil)
                             :msg "Client RMW-vars send timeout")
                (=values e))))
        ;; else - no rendezvous
        (=values ans)))
    ))

(defun rem-vars (names)
  (ac:send *var-server* :rems names))

;; ----------------------------------------------------------------------------

(=defun return-item-val (pairs)
  ;; return the val of the single association in a single-association
  ;; list
  (if (consp pairs)
      (destructuring-bind ((_ val)) pairs
        (declare (ignore _))
        (=values val))
    ;; else
    (=values pairs)))
  
(=defun get-var (name &optional default)
  (with-future (pairs)
      (get-vars `(,name) default)
    (return-item-val pairs)))

(defun set-var (name val)
  (ac:send *var-server* :sets `((,name ,val)))
  val)

(=defun rmw-var (name fn &optional default)
  (with-future (ans)
      (rmw-vars `(,name)
                (lambda (pairs)
                  (destructuring-bind ((_ val)) pairs
                    (declare (ignore _))
                    `((,name ,(funcall fn val)))
                    ))
                default)
    (return-item-val ans)))

(defun rem-var (name)
  (ac:send *var-server* :rems `(,name)))

;; ---------------------------------------------------------

(define-condition rollback (error)
  ())

(define-condition transaction-failure (error)
  ())

(=defun transact (names fn &key default (retries 5))
  ;; The names arg is a list of input vars that the result will depend
  ;; upon. We grab copies of them, perform some work, then just before
  ;; sending back the results we double check quickly that the inputs
  ;; have not changed.
  (with-future (old-pairs)
      (get-vars names default)
    (if (had-rendezvous old-pairs)
        (nlet iter ((ct retries))
          (cond
           ((plusp ct)
            (handler-case
                (let* ((new-pairs (funcall fn old-pairs)) ;; this could trigger error
                       (checker   (lambda (cur-pairs)
                                    (cond
                                     ((every (lambda (old cur)
                                               (eql (cadr old) (cadr cur)))
                                             old-pairs cur-pairs)
                                      ;; data has not changed, so commit mutations
                                      new-pairs)
                                     (t
                                      ;; data has changed beneath us - rollback
                                      (setf old-pairs cur-pairs)
                                      (error (make-condition 'rollback)))))
                                  ))
                  (with-future (ans)
                      (rmw-vars names checker default)
                    (if (listp ans)
                        (=values ans)
                      ;; else - was our rollback
                      (iter (1- ct)))
                    ))
              
              (error (e)
                ;; error was triggered by mutator fn
                (=values e))
              ))
           (t
            ;; out of retries
            (=values (make-condition 'transaction-failure)))
           ))
      ;; else - no rendezvous
      (=values old-pairs))
    ))

;; --------------------------------------------------------------------------

(defun tst ()
  (labels ((incr (x)
             (1+ x))
           (tstr (ix)
             (nlet iter ((ct 5))
               (when (plusp ct)
                 (with-future (ans)
                     (rmw-var :x #'incr 15)
                   (pr (format nil "Actor ~A  x = ~A" ix ans))
                   ;; Without randomized sleep, they all perform in
                   ;; strict sequence.  Actors do no need sleep in
                   ;; order to cycle among themselves, but with
                   ;; constant sleep, or without sleep, they cycle in
                   ;; strict seequence.
                   (sleep (random 0.1))
                   (iter (1- ct))))
               )))
    (rem-var :x)
    (dotimes (ix 10)
      (spawn #'tstr ix)
      )))

#|
(with-cont
  (transact '(:a :b :c)
            (lambda (_)
              '((:a 1) (:b 2) (:c 3)))))

(with-cont
  (rem-vars '(:a :b :c)))

|#

(defun dropout-test (&optional (n 100))
  (spawn (lambda ()
           (pr "start")
           (nlet iter ((ct n))
             (if (plusp ct)
               (with-future (ans)
                   (rmw-var :x 'identity 15)
                   ;; (get-var :x 15)
                 (unless (realp ans)
                   (pr (format nil "Aberrant ans = ~A" ans)))
                 ;; (pr ans)
                 (iter (1- ct)))
               (pr :finished))
             ))))

#|
(spawn (lambda ()
         (with-future (ans)
             (rmw-var :x 'identity 15)
           (assert (ac::unblocked-p (ac:current-actor)))
           (assert (null (ac::actor-blocking-timer (ac:current-actor))))
           (pr ans))))
  
(let ((ch (make-channel)))
  (spawn (lambda ()
           ;; (poke ch 24)
           (with-future (ans)
               (async (wrap-abort
                       (wrap-abort (recvEvt ch :async t)
                                   (lambda ()
                                     (pr :first-abort)))
                       (lambda ()
                         (pr :second-abort))))
             (pr ans)
             (if (had-rendezvous ans)
                 (pr :ok))))))

;; Tail Call Optimization (TCO)
(defun foo () (foo))
(defun bar () (format t "") (foo))
(disassemble #'foo)
(disassemble #'bar)

(spawn (lambda ()
         (let ((*futures-timeout* 2))
           (on-failure
             (assert (forcibly-unblocked-p))
             (pr :Hey!))
           (with-future (x)
               (spawn (lambda (kcont)
                        (sleep 3)
                        (funcall kcont 15))
                      =values-callback)
             (unless (forcibly-unblocked-p)
               (pr x)))
           )))

(spawn (lambda ()
         (let ((*futures-timeout* 2))
           (handle-fail
            (with-future (x)
                (spawn (lambda (kcont)
                         (sleep 1)
                         (funcall kcont 15))
                       =values-callback)
              (pr x))
            ;; fail handler
            (pr :Hey!))
           )))

|#

