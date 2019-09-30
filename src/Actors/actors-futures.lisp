;; actors-futures.lisp -- WITH-FUTURE for Actors
;;
;; DM/RAL 12/19
;; ----------------------------------------------------------

(in-package :actors)

#|
There actually seems little purpose in writing code this way. An Actor
needs strictly enforced single-thread semantics.

Once a futures computation is launched, we are merely passing off
possibly-blocking behavior to another thread. And while our Actor is
no longer blocking, it cannot safely handle any incoming messages in
parallel, because that could mess up the context expected by the
continuation.

So our Actor goes into a selective RECV operation that only handles
messages expected by the RECV block. All other messages are enqueued
for later handling by the Actor, after the RECV block has completed.

So why not just have the Actor perform the blocking activity itself
and be done with it. Some task always has to block, somewhere.
 |#

#|
(defmacro with-future ((&rest args) form &body body)
  ;; should always be in tail position
  (let ((g!block (gensym)))
    `(do-with-future
      (lambda ()
        (block ,g!block
          (flet ((=values (&rest args)
                   (return-from ,g!block (values-list args))))
            ,form)))
      ,(if (member '&rest args)
           `(lambda (&optional ,@args)
              ,@body)
         ;; else
         `(lambda (&optional ,@args &rest #1=ignored)
            (declare (ignore #1#))
            ,@body)))))

(defun do-with-future (exec-fn cont-fn)
  (if-let (me (current-actor))
      (let ((xid        (uuid:make-v1-uuid))
            (my-timeout *timeout*))
        (spawn-worker
         (lambda ()
           (let ((*current-actor*  me)
                 (*timeout*        my-timeout))
             (send me :cont-future-{7c434b50-1a12-11ea-96cb-787b8acbe32e}
                   xid
                   (capture-ans-or-exn exec-fn)))))
        (recv
          ((list ':cont-future-{7c434b50-1a12-11ea-96cb-787b8acbe32e} id ans)
           when (uuid:uuid= id xid)
           (multiple-value-call cont-fn (recover-ans-or-exn ans)))

          :timeout *timeout*
          ))
    ;; else
    (multiple-value-call cont-fn (funcall exec-fn))))
|#

;; --------------------------------------------------------------------
#|

  Now... if your Actor code is reentrant, meaning no (or protected)
mutable state, then you can safely launch futures activity to another
thread, and remain non-blocking for parallel processing of incoming
messages. All state is passed in as arguments to the Actor message
handlers, or protected against SMP multiple access.

  There seems no way to enforce reentrancy in the Actor function, so
buyer beware...

  - Don't mutate local or global bindings without first protecting with a lock.

But if the Actor code is truly reentrant, then there is no need to
enforce single-thread semantics, and the continuation code could be
performed as well in the spawned worker thread. There is no need to
send a message containing the continuation back to the original Actor
for execution.

|#

;; ------------------------------------------
;; Create a callback on the function argument

(defclass callback-function ()
  ()
  (:metaclass #+:LISPWORKS clos:funcallable-standard-class
              #+:ALLEGRO   mop:funcallable-standard-class
              #+:CLOZURE   ccl:funcallable-standard-class
              #+:SBCL      sb-mop:funcallable-standard-class
              ))

(defmethod initialize-instance :after ((obj callback-function) &key behavior &allow-other-keys)
  (#+:LISPWORKS clos:set-funcallable-instance-function
   #+:ALLEGRO   mop:set-funcallable-instance-function
   #+:CLOZURE   ccl:set-funcallable-instance-function
   #+:SBCL      sb-mop:set-funcallable-instance-function
   obj behavior))

(defmethod =cont ((contfn callback-function))
  contfn)

(defmethod =cont ((contfn symbol))
  (=cont (symbol-function contfn)))

(defmethod =cont ((contfn function))
  (if-let (self (current-actor))
      ;;
      ;; If the callback originated from inside an Actor, we ensure
      ;; that it will later execute inside that Actor only when that
      ;; Actor is alive.
      ;;
      ;; Code inside an Actor should only be executing on one thread
      ;; at a time, in order to preserve SMP single-thread semantics.
      ;;
      (make-instance 'callback-function
                     :behavior (lambda (&rest args)
                                 (if (eq self (current-actor))
                                     (apply contfn args)
                                   (apply 'send self :continuation-{14AFB5F8-D01F-11E7-A1BE-985AEBDA9C2A} contfn args))))
                     
    ;; else - not originating from inside of an Actor, just return the
    ;; function unchanged
    contfn))

;; --------------------------------------------------------

(defmacro with-future ((&rest args) form &body body)
  ;; Should always be in tail position
  ;;
  ;; Result of exec-form indicated by calling =VALUES, or else result
  ;; of exec form.
  ;;
  `(do-with-future
    (=lambda ()
      ,form)
    ,(if (member '&rest args)
         `(lambda (&optional ,@args)
            ,@body)
       ;; else
       `(lambda (&optional ,@args &rest #1=ignored)
          (declare (ignore #1#))
          ,@body))
    ))

#|
(defun do-with-future (exec-fn cont-fn)
  (flet ((doit ()
           (multiple-value-call cont-fn
             (funcall exec-fn))))
    (if-let (me (current-actor))
        (let ((my-timeout *timeout*))
          (spawn-worker
           (lambda ()
             (let ((*current-actor* me)
                   (*timeout*       my-timeout))
               (doit)))
           ))
      ;; else
      (doit))))
|#

(defun do-with-future (exec-fn cont-fn)
  ;; perform exec-fn in another thread,
  ;; have continuation performed back in caller thread
  (let ((kexec-fn  (lambda ()
                     (with-cont
                       (=funcall exec-fn)))))
    (if-let (me (current-actor))
        (let ((kcont-fn (=cont (lambda (arg)
                                 (multiple-value-call cont-fn
                                   (recover-ans-or-exn arg)))
                               )))
          (spawn-actor-as-worker
           (lambda ()
             (funcall kcont-fn (capture-ans-or-exn kexec-fn)))))
      ;; else
      (multiple-value-call cont-fn
        (funcall kexec-fn)))))

#|
(spawn
 (lambda ()
   (with-future (&rest args)
       (progn
         (sleep 1)
         (=values 15 16))
     (pr args))))

(with-future (&rest args)
    (progn
      (sleep 1)
      (=values 15 16))
  (pr args))

(defvar *x*  15)

(spawn
 (lambda ()
   (let* ((*x* 32)
          (xx  *x*))
     (with-future (x)
         (progn
           (sleep 1)
           (=values xx))
       (pr `(,x ,*x*))))))
 |#
