
#|
;; Try using select to determine relative speed of assoc list vs hashtable for various sizes
;; Result: over 100M Iters, Assoc wins for fewer than 7 items, else Hashtable
(defun tst-ah (&key (nitems 15) (niters 1000000))
  (let* ((ch    (make-channel))
         (htbl  (make-hash-table))
         (keys  (remove-duplicates
                 (loop repeat (* 10 nitems) collect (random nitems))))
         (alist (loop for key in keys collect
                      (progn
                        (setf (gethash key htbl) key)
                        (list key key))))
         (plist (loop for key in keys nconc
                      (list key key)))
         (tree  (let ((tree (sets:empty)))
                  (loop for key in keys do
                        (setf tree (maps:add key key tree))))))
    (labels ((tst-assoc ()
               (loop for key from 0 below niters do
                     (assoc (mod key (* 2 nitems)) alist))
               (poke ch "Assoc"))
             (tst-htbl ()
                (loop for key from 0 below niters do
                      (gethash (mod key (* 2 nitems)) htbl))
                (poke ch "HTbl"))
             (tst-plist ()
               (loop for key from 0 below niters do
                     (getf plist (mod key (* 2 nitems))))
               (poke ch "PList"))
             (tst-tree ()
               (loop for key from 0 below niters do
                     (maps:find (mod key (* 2 nitems)) tree))
               (poke ch "Tree"))
             (time (fn)
               (let ((start (get-universal-time))
                     stop)
                 (funcall fn)
                 (setf stop (get-universal-time))
                 (- stop start))))

      #|
      (sync (guard #'(lambda ()
                       (spawn #'tst-assoc)
                       ;; (spawn #'tst-htbl)
                       ;; (spawn #'tst-plist)
                       (spawn #'tst-tree)
                       (recvEvt ch))))
      |#
      (list :AList (time #'tst-assoc)
            :PList (time #'tst-plist)
            :Tree  (time #'tst-tree)
            :HTbl  (time #'tst-htbl))
      )))
|#
#|
(defun xtst ()
  (let ((ch (make-channel)))
    (sync (choose* (wrap-abort (recvEvt ch)
                               #'(lambda ()
                                   (print "WrapAbort fired")))
                   (timeoutEvt 3)))
    ))
|#
;; ----------------------------------------------------------
;; Equivalent(?) server using async mailboxes already in Lisp...

#| -- for LispWorks only...
(progn
  (defun mbrpc? (sbox req &key timeout replyMB)
    (let ((rbox (or replyMB
                    (mp:make-mailbox))))
      (mp:mailbox-send sbox (list req rbox))
      (mp:mailbox-read rbox :rpc timeout)))
  
  (defun mbrpc! (rbox ans)
    (mp:mailbox-send rbox ans)
    ans)
  
  (defun make-mbserver (mbox)
    (spawn (lambda ()
             (loop for (req replymb) = (mp:mailbox-read mbox)
                   until (and (eq req :kill)
                              (mbrpc! replymb :OKAY))
                   do
                   (case req
                     (:primes (mbrpc! replymb '(1 2 3 5 7 "9?")))
                     (:time   (mbrpc! replymb (get-universal-time)))
                     (t       (mbrpc! replymb "Eh?"))
                     )))
           ))
  
  (defun mb-tst1 ()
    (let ((smb (mp:make-mailbox)))
      (make-mbserver smb)
      (print (mbrpc? smb :time))
      (print (mbrpc? smb :primes))
      (print (mbrpc? smb :what?))
      (print (mbrpc? smb :kill))))

  (defun make-mbserver-spd (mbox)
    (spawn (lambda ()
             (loop for (req replymb) = (mp:mailbox-read mbox)
                   until (and (eq req :kill)
                              (mbrpc! replymb :OKAY))
                   do
                   (mbrpc! replymb :ok)))
           ))
  
  (defun mb-tst-spd (&optional (count 1000000))
    (let ((sbox (mp:make-mailbox)))
      (make-mbserver-spd sbox)
      ;; 8.2 microsec/RPC = 122 kRPC/sec
      (time (loop repeat count do
                  (mbrpc? sbox :time)))
      (mbrpc? sbox :kill)))
  ) ;; progn
|#

;; ----------------------------------------------------------
;; Equivalent(?) server using async process mailboxes already in Lisp...
;; This is definitely the way to go if you can avoid SELECT
#|
(defun pmbrpc? (proc req)
  (mp:process-send proc (list req mp:*current-process*))
  (mp:process-wait-for-event))

(defun pmbrpc! (rproc ans)
  (mp:process-send rproc ans)
  ans)

(defun make-pmbserver ()
  (spawn #'(lambda ()
             (loop for (req replyto) = (mp:process-wait-for-event)
                   until (and (eq req :kill)
                              (pmbrpc! replyto :OKAY))
                   do
                   (case req
                     (:primes (pmbrpc! replyto '(1 2 3 5 7 "9?")))
                     (:time   (pmbrpc! replyto (get-universal-time)))
                     (t       (pmbrpc! replyto "Eh?"))
                     )))
         ))
  
#|
(let ((srv (make-pmbserver)))
  (print (pmbrpc? srv :time))
  (print (pmbrpc? srv :primes))
  (print (pmbrpc? srv :what?))
  (print (pmbrpc? srv :kill)))

(let ((srv (make-pmbserver)))
  ;; 8.7 microsec/rpc = 111k rpc/sec
  (time (loop repeat 1000000 do
              (pmbrpc? srv :time)))
  (pmbrpc? srv :kill))
|#
|# ;; -- end of for LispWorks only --

;; -------------------------------------------------------------------------
#|
(defun tst ()
  (let ((ch  (make-channel))
        (ans (make-channel)))
    (spawn #'(lambda ()
               (sync (choose (wrap (recvEvt ch) #'(lambda (x) (send ans (list 1 x))))
                             (wrap (recvEvt ch) #'(lambda (y) (send ans (list 2 y))))))))
    (send ch 15)
    (recv ans)))

(defun tst ()
  (let ((ch1 (make-channel))
        (ch2 (make-channel))
        (ans (make-channel)))
    (spawn #'(lambda ()
               (let* ((val (select (recvEvt ch1)
                                   (recvEvt ch2)
                                   (timeoutEvt 2)
                                   )))
                 (send ans (list 1 val)))))
    (spawn #'(lambda ()
               (let* ((val (sync (choose (recvEvt ch2)
                                         (recvEvt ch1)
                                         (timeoutEvt 2))
                                 )))
                 (send ans (list 2 val)))))
    (spawn #'(lambda () (select (sendEvt ch1 15)
                                (timeoutEvt 2))))
    (spawn #'(lambda () (select (sendEvt ch2 16)
                                (timeoutEvt 2))))
    (list (recv ans)
          (recv ans))
    ))
|#
