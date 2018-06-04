;;; hemlock-log-streams.lisp
;;; 28-Sep-2016 SVS
;;; Makes log streams in Hemlock with CCL IDE, so you can just
;;; write to Hemlock windows as if they were streams.

(in-package :gui)

(export '(MAKE-LOG-WINDOW) :gui)

#|
ALL modifications to buffers should be done from the gui thread.
gui::execute-in-gui or gui::queue-for-gui.
Reads from buffers can be done from any thread, but beware that
they might not be consistent since there is no locking.
|#

(defmethod stream-from-window ((window hemlock-frame))
  (let ((mark (hi::buffer-end-mark ; this is ridiculously complex
               (hi::hemlock-view-buffer
                (hemlock-view window)))))
    (hi::make-hemlock-output-stream mark :line)))

; In Listener, do
; (setf s (stream-from-window (target)))
; (execute-in-gui (lambda () (format s "foo~%")))
; Now foo should show up at end of second window.
; Window will note that it has been changed, and undo works on that window to eliminate
; what you wrote. Nice.

#|
Because you can't just call format on a hemlock-output-stream without
wrapping the call in one of the gui forms above, this breaks the
purity of the stream abstraction. So we wrap a gray stream
around this thing to get back that purity.
|#

(defclass highlevel-hemlock-output-stream (hi::hemlock-output-stream)
  ())

(defmethod ccl::stream-write-char ((stream highlevel-hemlock-output-stream) char)
  (queue-for-gui
   (lambda () (call-next-method stream char))))

(defmethod ccl::stream-write-string ((stream highlevel-hemlock-output-stream) string
                                &optional
                                (start 0)
                                (end (length string)))
  (queue-for-gui
   (lambda () (call-next-method stream string start end))))

(defmethod highlevel-stream-from-window ((window hemlock-frame))
  "Returns a highlevel Hemlock output stream to the end of the given window.
   The word 'highlevel' here means you can format output to this stream with
   ordinary output functions like format, and not have to worry about enqueing
   these functions onto the gui process. That happens automatically."
  (let ((lowstream (stream-from-window window)))
    (when lowstream
      (change-class lowstream 'highlevel-hemlock-output-stream))))

; (setf s (highlevel-stream-from-window (target)))
; (format s "Rain in spain")
; Now 'Rain in spain' should show up at end of second window.

(defun make-log-window (&optional (title "Log"))
  "Makes a log window and returns an output stream to it."
 (let ((w (NEXTSTEP-FUNCTIONS:|window| (HI::HEMLOCK-VIEW-PANE (COCOA-EDIT)))))
   (when title (NEXTSTEP-FUNCTIONS:|setTitle:| W (%MAKE-NSSTRING TITLE)))
   (highlevel-stream-from-window w)))

; (setf s (make-log-window))
; (format s "Foo")

