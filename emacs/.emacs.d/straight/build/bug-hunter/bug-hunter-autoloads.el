;;; bug-hunter-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "bug-hunter" "bug-hunter.el" (0 0 0 0))
;;; Generated autoloads from bug-hunter.el

(autoload 'bug-hunter-file "bug-hunter" "\
Bisect FILE while testing ASSERTION.
All sexps in FILE are read and passed to `bug-hunter-hunt' as a
list.  See `bug-hunter-hunt' for how to use ASSERTION.

\(fn FILE &optional ASSERTION)" t nil)

(autoload 'bug-hunter-init-file "bug-hunter" "\
Test ASSERTION throughout `user-init-file'.
All sexps inside `user-init-file' are read and passed to
`bug-hunter-hunt' as a list.  See `bug-hunter-hunt' for how to use
ASSERTION.

\(fn &optional ASSERTION)" t nil)

(register-definition-prefixes "bug-hunter" '("bug-hunter-"))

;;;***

;;;### (autoloads nil nil ("bug-hunter-test.el") (0 0 0 0))

;;;***

(provide 'bug-hunter-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bug-hunter-autoloads.el ends here
