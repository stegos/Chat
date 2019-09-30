#|
MIT License Terms:

Copyright (c) 2017, Refined Audiometrics Laboratory, LLC

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

(in-package :actors)

;; ------------------------------------------------------------------
;; CAUTION - Any code that makes use of parallel execution from inside
;; an Actor body code sets up a potential violation of the invariant
;; that Actor local state is mutable only by the thread executing the
;; Actor.
;;
;; You must take care not to allow mutation of Actor internal state
;; from both the Actor body code and the parallel forms which could be
;; executed by another thread concurrently with the Actor body code.
;;
;; In such cases, take care to use mutually exclusive mutation, using
;; e.g., RMW for atomic read-modify-write on such state bindings.
;;
;; The same precautions must be taken for any internal state bindings
;; that are handed out to ASK'ing clients.
;; -------------------------------------------------------------------

(defun pmapcar (fn &rest lists)
  ;; Parallel map of fn over list of params. First group is performed
  ;; by our own Actor in parallel with workers acting on other groups.
  (when lists
    (let* ((grps  (apply #'um:zip lists)))
      (if (um:single grps)
          (apply fn (car grps))
        ;; else
        (let* ((len   (length grps))
               (count (list len))
               (ansv  (make-array len))
               (mbox  (mp:make-mailbox)))
          (flet ((done (ix ans)
                   (setf (aref ansv ix) ans)
                   (when (zerop (mpcompat:atomic-decf (car (the cons count))))
                     (mp:mailbox-send mbox ansv))))
            (let ((actors (loop for grp in (cdr grps)
                                for ix from 1
                                collect
                                (spawn-actor-as-worker
                                 (lambda (ix args)
                                   (done ix (apply 'capture-ans-or-exn fn args)))
                                 ix grp))
                          ))
              ;; perform the first of the branchees ourself
              (done 0 (apply 'capture-ans-or-exn fn (car grps)))
              ;; blocking, in this case, means the wait for worker threads
              ;; to complete after we have performed the first funcall
              (map 'list 'recover-ans-or-exn
                   (handler-case
                       (read-mbox-with-timeout mbox)
                     (timeout (c)
                       (map nil 'terminate-actor actors)
                       (error c))))
              )))))))

(defmacro par (&rest forms)
  `(pmapcar 'funcall
            (list ,@(mapcar #`(lambda ()
                                ;; WITH-CONT allows form to use =VALUES
                                (with-cont
                                  ,a1))
                            forms))))

;; -------------------------------------------------------------------

#|
(par
  (print :doit1)
  (print :doit2))
(pmapcar 'print '(:doit1 :doit2))
|#  

(defmacro parlet (bindings &body body)
  ;; either directly, or eventually
  (let ((args  (mapcar 'first bindings))
        (forms (mapcar 'second bindings)))
    `(destructuring-bind ,args
         (par ,@forms)
       ,@body)))

#+:LISPWORKS
(editor:setup-indent "parlet" 2)

