;;; core-gc.el --- Handle garbage collection while idle -*- lexical-binding: t -*-
;;; Commentary:
;; Some magic of gcmh without the entire package, courtesy of its author + some custom magic of course.
;;; Code:

(require 'core-util)

(defvar core-gc--k-debug-p t
  "Boolean to determine whether to echo message for gc or not.")

(defvar core-gc--timeout 45
  "Time limit for idleness until gc starts.")
(defvar core-gc--timer nil
  "Timer which periodically runs gc logic.  nil if not active.")

(defun core-gc|collect()
  "Run gc and outputs messages if debugging."
  (if core-gc--k-debug-p
      (message "Garbage Collector has run for %.06fsec"
               (k-time! (garbage-collect)))
    (garbage-collect)))

(defun core-gc--start ()
  "Start watching for when idle for `core-gc--timeout' seconds to run the GC."
  (interactive)
  (unless core-gc--timer
    (setq core-gc--timer (run-with-idle-timer core-gc--timeout t 'core-gc|collect))))

(defun core-gc--cancel ()
  "Stop idle gc."
  (interactive)
  (when core-gc--timer
    (cancel-timer core-gc--timer)
    (setq core-gc--timer nil)))

(add-hook 'after-init-hook 'core-gc--start)

(provide 'core-gc)
;;; core-gc.el ends here
