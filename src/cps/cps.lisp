
(in-package :cps)

(define-symbol-macro =bind-cont %sk)

(defmacro =lambda (parms &body body)
  ;; define an anonymous CPS function
  `#'(lambda (%sk ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  ;; define a named CPS function
  (let* ((fn  (um:symb '= name))
         (macro-body (parse-and-assemble-args-from-lambda-list fn parms)))
    `(progn
       (defmacro ,name ,parms
         ,macro-body)
       (defun ,fn (%sk ,@parms) ,@body))))

(defmacro =defmethod (name parms &body body)
  ;; define a named CPS method
  (let ((fn  (um:symb '= name)))
    (cond ((fboundp fn)
           `(defmethod ,fn (%sk ,@parms) ,@body))
          (t
           (let ((macro-body (parse-and-assemble-args-from-lambda-list fn parms)))
             `(progn
                (defmacro ,name ,(method-macro-parms parms)
                  ,macro-body)
                (defmethod ,fn (%sk ,@parms) ,@body))))
          )))

(defmacro =flet (bindings &body body)
  (let* ((names  (mapcar 'first bindings))
         (args   (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (um:symb '= name))
                         names)))
    `(flet ,(mapcar (lambda (fn args body)
                      `(,fn (%sk ,@args) ,@body))
                    fns args bodies)
       (macrolet ,(mapcar (lambda (name parms fn)
                            `(,name ,args ,(parse-and-assemble-args-from-lambda-list fn parms)))
                          names args fns)
         ,@body))))

(defmacro =labels (bindings &body body)
  (let* ((names  (mapcar 'first bindings))
         (args   (mapcar 'second bindings)) 
         (bodies (mapcar 'cddr bindings))
         (fns    (mapcar (lambda (name)
                           (um:symb '= name))
                         names)))
    `(macrolet ,(mapcar (lambda (name parms fn)
                          `(,name ,args ,(parse-and-assemble-args-from-lambda-list fn parms)))
                        names args fns)
       (labels ,(mapcar (lambda (fn args body)
                          `(,fn (%sk ,@args) ,@body))
                        fns args bodies)
         ,@body))))

#+:LISPWORKS
(editor:setup-indent "=flet" 1 nil nil 'flet)
#+:LISPWORKS
(editor:setup-indent "=labels" 1 nil nil 'flet)


(defmacro =values (&rest retvals)
  ;; invoke a continuation. This should generally be in tail position
  `(funcall %sk ,@retvals))

(defmacro =funcall (fn &rest args)
  ;; invoke a CPS function
  `(funcall ,fn %sk ,@args))

(defmacro =apply (fn &rest args)
  ;; invoke a CPS function
  `(apply ,fn %sk ,@args))

(defmacro with-cont (&body body)
  ;; for REPL toplevel call to function defined with =defun
  (let ((g!block (gensym)))
    `(block ,g!block
       (let ((%sk (lambda (&rest args)
                    (return-from ,g!block (values-list args)))))
         ,@body))))

(defmacro =bind (args expr &body body)
  `(multiple-value-call
    ,(if (member '&rest args)
         `(lambda ,args
            ,@body)
       ;; else - as in multiple-value-bind
       `(lambda (&optional ,@args &rest #1=#:ignored)
          (declare (ignore #1#))
          ,@body))
     (with-cont
       ,expr)))

