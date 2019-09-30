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


(defpackage :armw
  (:use :cl :rch)
  (:import-from :ac
   :=defun
   :with-future
   :with-indefinite-future
   :with-cont
   :=values
   :=values-callback
   :terminate-actor
   :pr)
  (:import-from :useful-macros
   :dcase
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

(in-package :armw)

;; -------------------------------------------------------------------
;; ASync Actor-based Key/Value server

(defvar *vartbl*  (make-hash-table
                   :test #'equal)) ;; to allow for case-sensitive strings as keys

(defvar *timeout* 1)
(defvar *initial-timeout* 2)

(=defun timed-evt (ev &key (msg "Channel timeout") (timeout *timeout*))
  (async (cancelAfterEvt timeout ev :msg msg)))

(defun run-async-var-server (reqCh)
  ;; asynchronous code - intended to run as an Actor
  ;;
  ;; Unlike many Actor bodies, this one does not accept messages sent
  ;; directly to the Actor. (Those usually have a DLAMBDA body to
  ;; dispatch incoming direct messages.)
  ;;
  ;; Instead this Actor gets all client messages via a Reppy Channel.
  ;; That is the only way to prevent interposing messages in the
  ;; middle of the RMW handshake dance. During that dance, it listens
  ;; only to the private channel between it and the client code.
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
    (nlet iter ()
      (with-indefinite-future (msg)
          (arecv reqCh)
        (when (had-rendezvous msg)
          (dcase msg
            (:gets (replyCh names default)
             (with-future ()
                 (timed-evt (sendEvt replyCh (get-names names default))
                            :msg "Server GETS send timeout")
               (iter)))
            (:sets (pairs)
             (set-names pairs)
             (iter))
            (:rmws (replyCh names default)
             (with-future (ans)
                 (timed-evt (sendEvt replyCh (get-names names default))
                            :msg "Server RMW send timeout")
               (if (had-rendezvous ans)
                   (with-future (pairs)
                       (timed-evt (recvEvt replyCh)
                                  :msg "Server RMW recv timeout")
                     (when (consp pairs)
                       (set-names pairs))
                     (iter))
                 ;; else
                 (iter))))
            (:rems (names)
             (remove-names names)
             (iter))
            (:kill ()
             ;; no (iter) here... no more listening for
             ;; client messages on the reqCh
             )
            ))))
    ))

;; ----------------------------------------------------------------------------
;; From Lisetener, call all of these functions using (WITH-CONT (fn...))

(defvar *var-server*     nil)
(defvar *server-channel* nil)

(=defun kill-server ()
  (if *var-server*
      (with-future ()
          (timed-evt (wrap-abort
                      (sendEvt *server-channel* `(:kill))
                      (lambda ()
                        (terminate-actor *var-server*)))
                     :msg     "Kill timeout"
                     :timeout *initial-timeout*)
        (setf *var-server*     nil
              *server-channel* nil)
        (=values))
    ;; else
    (=values)))
  
(=defun make-async-var-server ()
  (with-future ()
      (kill-server)
    (let ((reqCh  (make-channel)))
      (=values (setf *var-server*     (spawn #'run-async-var-server reqCh)
                     *server-channel* reqCh)))
    ))
                
;; ----------------------------------------------------------------------------

(=defun get-vars (names &optional default)
  (let ((replyCh (make-channel)))
    (with-future (ans)
        (timed-evt (sendEvt *server-channel* `(:gets ,replyCh ,names ,default))
                   :msg     "Client GET-VARS timeout"
                   :timeout *initial-timeout*)
      (if (had-rendezvous ans)
          (timed-evt (recvEvt replyCh)
                     :msg "Client GET-VARS recv timeout")
        (=values nil))
      )))

(=defun set-vars (pairs)
  (with-future (ans)
      (timed-evt (sendEvt *server-channel* `(:sets ,pairs))
                 :msg     "Client SET-VARS timeout"
                 :timeout *initial-timeout*)
    (=values (if (had-rendezvous ans)
                 (second ans)
               ans))
    ))

(=defun rmw-vars (names fn &optional default)
  (let ((replyCh (make-channel)))
    (with-future (ans)
        (timed-evt (sendEvt *server-channel* `(:rmws ,replyCh ,names ,default))
                   :msg     "Client RMW-VARS timeout"
                   :timeout *initial-timeout*)
      (if (had-rendezvous ans)
          (with-future (ans)
              (timed-evt (recvEvt replyCh)
                         :msg "Client RMW-VARS recv timeout")
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
              ;; else
              (=values ans)))
        ;; else
        (=values ans))
      )))

(=defun rem-vars (names)
  (timed-evt (sendEvt *server-channel* `(:rems ,names))
             :msg     "Client REM-VARS timeout"
             :timeout *initial-timeout*))

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

(=defun set-var (name val)
  (with-future (ans)
      (set-vars `((,name ,val)))
    (return-item-val ans)))

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

(=defun rem-var (name)
  (rem-vars `(,name)))

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
       ))))

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
    (with-future ()
        (rem-var :x)
      (dotimes (ix 10)
        (spawn #'tstr ix)
        ))))

#|
(with-cont
  (transact '(:a :b :c)
            (lambda (_)
              '((:a 1) (:b 2) (:c 3)))))

(with-cont
  (rem-vars '(:a :b :c)))

|#

(defvar *iter-count* 0)
(defun dropout-test (&optional (n 100))
  (spawn (lambda ()
           (pr "start")
           (setf *iter-count* 0)
           (nlet iter ((ct n))
             (if (plusp ct)
               (with-future (ans)
                   (rmw-var :x 'identity 15)
                   ;; (get-var :x 15)
                 (unless (realp ans)
                   (pr (format nil "Aberrant ans = ~A" ans)))
                 (incf *iter-count*)
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
|#

