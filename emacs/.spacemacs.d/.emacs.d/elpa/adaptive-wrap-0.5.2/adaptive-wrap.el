;;; adaptive-wrap.el --- Smart line-wrapping with wrap-prefix

;; Copyright (C) 2011-2018  Free Software Foundation, Inc.

;; Author: Stephen Berman <stephen.berman@gmx.net>
;;         Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.5.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `adaptive-wrap-prefix-mode' minor mode which sets
;; the wrap-prefix property on the fly so that single-long-line paragraphs get
;; word-wrapped in a way similar to what you'd get with M-q using
;; adaptive-fill-mode, but without actually changing the buffer's text.

;;; Code:

(require 'easymenu)

(defcustom adaptive-wrap-extra-indent 0
  "Number of extra spaces to indent in `adaptive-wrap-prefix-mode'.

`adaptive-wrap-prefix-mode' indents the visual lines to
the level of the actual line plus `adaptive-wrap-extra-indent'.
A negative value will do a relative de-indent.

Examples:

actual indent = 2
extra indent = -1

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
 eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
 enim ad minim veniam, quis nostrud exercitation ullamco laboris
 nisi ut aliquip ex ea commodo consequat.

actual indent = 2
extra indent = 2

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
    eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
    enim ad minim veniam, quis nostrud exercitation ullamco laboris
    nisi ut aliquip ex ea commodo consequat."
  :type 'integer
  :safe 'integerp
  :group 'visual-line)
(make-variable-buffer-local 'adaptive-wrap-extra-indent)

(defun adaptive-wrap-fill-context-prefix (beg en)
  "Like `fill-context-prefix', but with length adjusted by `adaptive-wrap-extra-indent'."
  ;; Note: fill-context-prefix may return nil; See:
  ;; http://article.gmane.org/gmane.emacs.devel/156285
  (let* ((fcp (or (fill-context-prefix beg en) ""))
         (fcp-len (string-width fcp))
         (fill-char (if (< 0 fcp-len)
                        (string-to-char (substring fcp -1))
                      ?\ )))
    (cond
     ((= 0 adaptive-wrap-extra-indent)
      fcp)
     ((< 0 adaptive-wrap-extra-indent)
      (concat fcp
              (make-string adaptive-wrap-extra-indent fill-char)))
     ((< 0 (+ adaptive-wrap-extra-indent fcp-len))
      (substring fcp
                 0
                 (+ adaptive-wrap-extra-indent fcp-len)))
     (t
      ""))))

(defun adaptive-wrap-prefix-function (beg end)
  "Indent the region between BEG and END with adaptive filling."
  ;; Any change at the beginning of a line might change its wrap prefix, which
  ;; affects the whole line.  So we need to "round-up" `end' to the nearest end
  ;; of line.  We do the same with `beg' although it's probably not needed.
  (goto-char end)
  (unless (bolp) (forward-line 1))
  (setq end (point))
  (goto-char beg)
  (forward-line 0)
  (setq beg (point))
  (while (< (point) end)
    (let ((lbp (point)))
      (put-text-property (point)
                         (progn (search-forward "\n" end 'move) (point))
                         'wrap-prefix
			 (let ((pfx (adaptive-wrap-fill-context-prefix
				     lbp (point))))
			   ;; Remove any `wrap-prefix' property that
			   ;; might have been added earlier.
			   ;; Otherwise, we end up with a string
			   ;; containing a `wrap-prefix' string
			   ;; containing a `wrap-prefix' string ...
			   (remove-text-properties
			    0 (length pfx) '(wrap-prefix) pfx)
			   pfx))))
  `(jit-lock-bounds ,beg . ,end))

;;;###autoload
(define-minor-mode adaptive-wrap-prefix-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  :group 'visual-line
  (if adaptive-wrap-prefix-mode
      (progn
        ;; HACK ATTACK!  We want to run after font-lock (so our
        ;; wrap-prefix includes the faces applied by font-lock), but
        ;; jit-lock-register doesn't accept an `append' argument, so
        ;; we add ourselves beforehand, to make sure we're at the end
        ;; of the hook (bug#15155).
        (add-hook 'jit-lock-functions
                  #'adaptive-wrap-prefix-function 'append t)
        (jit-lock-register #'adaptive-wrap-prefix-function))
    (jit-lock-unregister #'adaptive-wrap-prefix-function)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(define-key-after (lookup-key menu-bar-options-menu [line-wrapping])
  [adaptive-wrap]
  '(menu-item "Adaptive Wrap" adaptive-wrap-prefix-mode
	      :visible (menu-bar-menu-frame-live-and-visible-p)
	      :help "Show wrapped long lines with an adjustable prefix"
	      :button (:toggle . (bound-and-true-p adaptive-wrap-prefix-mode)))
  word-wrap)

;;;; ChangeLog:

;; 2018-03-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* adaptive-wrap/adaptive-wrap.el: Fix use without font-lock
;; 
;; 	(adaptive-wrap-prefix-function): Work on whole lines. Fix a kind of
;; 	memory leak.
;; 
;; 2017-05-04  Noam Postavsky  <npostavs@users.sourceforge.net>
;; 
;; 	Mark adaptive-wrap-extra-indent as safe if integerp (Bug#23816)
;; 
;; 	* packages/adaptive-wrap/adaptive-wrap.el: Bump version, copyright.
;; 	(adaptive-wrap-extra-indent): Mark as safe if integerp.
;; 
;; 2013-08-24  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* adaptive-wrap.el (adaptive-wrap-mode): Move after font-lock
;; 	(bug#15155).
;; 
;; 2013-07-31  Stephen Berman  <stephen.berman@gmx.net>
;; 
;; 	* adaptive-wrap.el: Fix bug#14974 by using define-key-after instead of
;; 	easy-menu-add-item.
;; 	(adaptive-wrap-unload-function): Remove.
;; 
;; 2013-07-29  Stephen Berman  <stephen.berman@gmx.net>
;; 
;; 	* adaptive-wrap.el: Require easymenu (bug#14974).
;; 
;; 2013-07-19  Rüdiger Sonderfeld  <ruediger@c-plusplus.de>
;; 
;; 	* adaptive-wrap.el (menu-bar-options-menu): Add checkbox for Adaptive
;; 	Wrap to the Line Wrapping submenu.
;; 	(adaptive-wrap-unload-function): New function.
;; 
;; 2013-02-01  Stephen Berman  <stephen.berman@gmx.net>
;; 
;; 	Fix error during redisplay: (wrong-type-argument stringp nil)
;; 
;; 2012-12-05  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* adaptive-wrap.el (adaptive-wrap-extra-indent): Fix buffer-localness. 
;; 	Reported by Jonathan Kotta <jpkotta@gmail.com>.
;; 
;; 2012-10-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Clean up copyright notices.
;; 
;; 2012-05-21  Jonathan Kotta  <jpkotta@gmail.com>
;; 
;; 	Add adaptive-wrap-extra-indent.
;; 	* adaptive-wrap/adaptive-wrap.el (adaptive-wrap-extra-indent): New var.
;; 	(adaptive-wrap-fill-context-prefix): New function.
;; 	(adaptive-wrap-prefix-function): Use it.
;; 	(adaptive-wrap-prefix-mode): Add to visual-line custom group.
;; 
;; 2012-01-05  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Rename adaptive-wrap-prefix to adaptive-wrap.
;; 
;; 	The old name overflowed the column in list-packages.
;; 


(provide 'adaptive-wrap)
;;; adaptive-wrap.el ends here
