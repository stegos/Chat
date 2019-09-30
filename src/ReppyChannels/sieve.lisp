
(in-package :rch)

(defun counter (n)
  (let ((outch  (make-channel)))
    (labels ((loopfn (n)
               (send (outch n)
                 (:on-rendezvous (_)
                  (loopfn (+ n 1))))
               ))
      (spawn #'loopfn n)
      outch)))

(defun filter (p inch)
  (let ((outch (make-channel)))
    (labels ((looper ()
               (recv (inch)
                 (:on-rendezvous (i)
                  (if (zerop (mod i p))
                      (looper)
                    (send (outch i)
                          (:on-rendezvous (_)
                           (looper)))
                    )))
               ))
      (spawn #'looper)
      outch)))

(defun sieve ()
  (let ((primes (make-channel)))
    (labels ((head (ch)
               (recv (ch)
                 (:on-rendezvous (p)
                  (send (primes p)
                        (:on-rendezvous (_)
                         (head (filter p ch))))
                  ))))
      (spawn #'head (counter 2))
      primes)))

(defun primes (n)
  (let ((ch  (sieve)))
    (labels ((looper (n lst)
               (if (zerop n)
                   (nreverse lst)
                 (recv (ch)
                   (:on-rendezvous (x)
                    (looper (1- n) (cons x lst))))
                 )))
      (looper n nil))))

;; ------------------------------------------------
;; Mutable Cell

(defstruct cell
  (reqCh   (make-channel))
  (replyCh (make-channel)))

(defun get-cell (cell contfn)
  (send ((cell-reqCh cell) '(:get))
        (:on-rendezvous (_)
         (recv ((cell-replyCh cell))
           (:on-rendezvous (x)
            (funcall contfn x)))
         )))

(defun set-cell (cell x)
  (send ((cell-reqCh cell) `(:put ,x))))

(defun new-cell (x)
  (let ((cell  (make-cell)))
    (labels ((server (x)
               (recv ((cell-reqCh cell))
                 (:on-rendezvous (msg)
                  (dcase msg
                    (:get ()
                     (send ((cell-replyCh cell) x)
                           (:on-rendezvous (_)
                            (server x))))
                    (:put (x-new)
                     (server x-new))
                    )))))
      (spawn #'server x)
      cell)))

;; ------------------------------------------------

(defmacro =lambda (parms &body body)
  `(lambda (%sk ,@parms) #F ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (um:symb '= name)))
    `(progn
       (defmacro ,name ,parms
         `(locally #F
            (,',f %sk ,,@parms)))
       (defun ,f (%sk ,@parms) #F ,@body))))

(defmacro =bind (parms expr &body body)
  `(locally #F
     (let ((%sk (ac:=cont (lambda ,parms ,@body))))
       ,expr)))

(defmacro =values (&rest retvals)
  `(locally #F
     (funcall %sk ,@retvals)))

(defmacro =funcall (fn &rest args)
  `(locally #F
     (funcall ,fn %sk ,@args)))

(defmacro =apply (fn &rest args)
  `(locally #F
     (apply ,fn %sk ,@args)))

(defmacro with-cont (&body body)
  `(let ((%sk #'values))
     (locally #F
       ,@body)))

;; --------------------------------------------------

