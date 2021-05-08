;;; core-macros.el -*- lexical-binding: t -*-

(require 'cl-macs)

(defmacro k-time! (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defmacro no-hook! (f hooks)
  "Call function F while temporarily removing HOOKS."
  `(lambda (&rest args)
     (let ((tbl (cl-loop for hook in ,hooks collect `(,(gensym) . ,hook))))
       (prog2
           (dolist (pair tbl)
             (eval `(setq ,(car pair) ,(cdr pair)))
             (eval `(setq ,(cdr pair) nil)))
           (apply ,f args)
         (dolist (pair tbl)
           (eval `(setq ,(cdr pair) ,(car pair))))))))

(defmacro with-os! (os &rest body)
  "Execute BODY if current os is OS."
  (declare (indent 1))
  `(when (if (consp ',os) (memq system-type ',os) (eq system-type ',os))
     ,@body))

(defmacro do-once-1-sec-after-emacs-startup (&rest body)
  `(run-with-idle-timer 1 ; run this after emacs is idle for 1 second
                        nil ; do this just once; don't repeat
                        (lambda () ,@body)))

(provide 'core-macros)
;;; core-macros.el ends here
