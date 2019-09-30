
(in-package :rch)

(let* ((ch  (make-channel))
       (thr (spawn (lambda ()
                     (sleep 2)
                     (sync (wrap (recvEvt ch)
                                 (lambda (val)
                                   (fac:pr :you-sent val)))
                           )))))
  (send ch '(1 2 3))
  ;; (funcall thr :introspect)
  ;; (values)
  )

(um:rmw