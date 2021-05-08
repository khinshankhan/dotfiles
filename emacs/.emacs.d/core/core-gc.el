;;; core-gc.el --- Handle garbage collection while idle -*- lexical-binding: t -*-
;;; Commentary:
;; Some magic of gcmh without the entire package, courtesy of its author + some custom magic of course.
;;; Code:

(require 'core-macros)

(defvar shan--k-gc-debug-p t
  "Boolean to determine whether to echo message for gc or not.")

(defvar shan--gc-timeout 45
  "Time limit for idleness until gc starts.")
(defvar shan--gc-timer nil
  "Timer which periodically runs gc logic.  nil if not active.")

(defun shan|gc-collect()
  "Run gc and outputs messages if debugging."
  (if shan--k-gc-debug-p
      (message "Garbage Collector has run for %.06fsec"
               (k-time! (garbage-collect)))
    (garbage-collect)))

(defun shan--gc-start ()
  "Start watching for when idle for shan--gc-timeout seconds to run the GC."
  (interactive)
  (unless shan--gc-timer
    (setq shan--gc-timer (run-with-idle-timer shan--gc-timeout t 'shan|gc-collect))))

(defun shan--gc-cancel ()
  "Stop idle gc."
  (interactive)
  (when shan--gc-timer
    (cancel-timer shan--gc-timer)
    (setq shan--gc-timer nil)))

(add-hook 'after-init-hook 'shan--gc-start)

(provide 'core-gc)
;;; core-gc.el ends here
