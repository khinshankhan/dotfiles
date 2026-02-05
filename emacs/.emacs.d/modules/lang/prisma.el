;;; prisma.el --- Major mode for Prisma schema files -*- lexical-binding: t; -*-

;; Custom Prisma mode that inherits from prog-mode instead of js-mode.
;; The pimeys/emacs-prisma-mode package tries to parse files as JavaScript,
;; which fails on valid Prisma syntax like `extensions = [pgstattuple(schema: "public")]`.

;; TODO: migrate logic into a package such that we can just install using straight and other can beenfit too

(require 'core-module)

;;; Customization

(defgroup prisma nil
  "Major mode for editing Prisma schema files."
  :group 'languages
  :prefix "prisma-")

(defcustom prisma-indent-offset 2
  "Number of spaces for each indentation level in Prisma mode."
  :type 'integer
  :group 'prisma
  :safe #'integerp)

;;; Syntax Table

(defvar prisma-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: // and ///
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Brackets
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Word constituents
    (modify-syntax-entry ?_ "w" table)
    ;; Punctuation
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?= "." table)
    table)
  "Syntax table for `prisma-mode'.")

;;; Font Lock

(defvar prisma-font-lock-keywords-1
  `(;; Block declaration keywords at start of line
    (,(rx line-start (0+ space)
          (group (or "model" "enum" "view" "type" "generator" "datasource"))
          (1+ space)
          (group (1+ (or word ?_))))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face)))
  "Level 1 font-lock keywords for Prisma mode (block declarations).")

(defvar prisma-font-lock-keywords-2
  `(,@prisma-font-lock-keywords-1
    ;; Scalar types
    (,(rx symbol-start
          (or "String" "Int" "BigInt" "Float" "Decimal"
              "Boolean" "DateTime" "Json" "Bytes")
          symbol-end)
     . font-lock-type-face)
    ;; Optional and array type modifiers
    (,(rx (group (or "String" "Int" "BigInt" "Float" "Decimal"
                     "Boolean" "DateTime" "Json" "Bytes"
                     (seq upper (0+ (or word ?_)))))
          (group (or "?" "[]")))
     (1 font-lock-type-face)
     (2 font-lock-type-face))
    ;; Field attributes: @id, @default, @unique, @map, @relation, @updatedAt, @ignore
    (,(rx (group "@" (or "id" "default" "unique" "map" "relation"
                         "updatedAt" "ignore")))
     (1 font-lock-preprocessor-face))
    ;; Block attributes: @@id, @@unique, @@index, @@map, @@ignore, @@fulltext, @@schema
    (,(rx (group "@@" (or "id" "unique" "index" "map" "ignore" "fulltext" "schema")))
     (1 font-lock-preprocessor-face))
    ;; Native database types: @db.VarChar, @db.Timestamp, etc.
    (,(rx (group "@db." (1+ (or word ?_))))
     (1 font-lock-builtin-face)))
  "Level 2 font-lock keywords for Prisma mode (types and attributes).")

(defvar prisma-font-lock-keywords-3
  `(,@prisma-font-lock-keywords-2
    ;; Built-in functions
    (,(rx symbol-start
          (group (or "autoincrement" "now" "uuid" "cuid" "env" "dbgenerated"))
          "(")
     (1 font-lock-function-name-face))
    ;; Constants
    (,(rx symbol-start
          (or "true" "false" "null"
              "NoAction" "Cascade" "SetNull" "SetDefault" "Restrict")
          symbol-end)
     . font-lock-constant-face)
    ;; Relation keywords inside @relation()
    (,(rx symbol-start
          (or "fields" "references" "onDelete" "onUpdate" "name" "map")
          symbol-end ":")
     . font-lock-variable-name-face)
    ;; Field names at start of line (inside blocks)
    (,(rx line-start (1+ space) (group (1+ (or word ?_))) (1+ space))
     (1 font-lock-variable-name-face)))
  "Level 3 font-lock keywords for Prisma mode (full highlighting).")

(defvar prisma-font-lock-keywords prisma-font-lock-keywords-3
  "Default font-lock keywords for Prisma mode.")

;;; Indentation

(defun prisma-indent-line ()
  "Indent the current line according to Prisma syntax.
Prisma files have simple structure: top-level at column 0, inside blocks at `prisma-indent-offset'."
  (interactive)
  (let ((indent 0)
        (pos (- (point-max) (point))))
    (save-excursion
      (beginning-of-line)
      ;; Skip whitespace and check if we're on a closing brace
      (skip-chars-forward " \t")
      (cond
       ;; Closing brace: indent to 0
       ((looking-at "}")
        (setq indent 0))
       ;; Otherwise, check if we're inside a block
       (t
        (condition-case nil
            (save-excursion
              (backward-up-list 1)
              ;; We're inside braces, indent by offset
              (setq indent prisma-indent-offset))
          ;; Not inside any list, top level
          (error (setq indent 0))))))
    ;; Apply indentation
    (indent-line-to indent)
    ;; Move point forward if it was in the indentation
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

;;; Mode Definition

;;;###autoload
(define-derived-mode prisma-mode prog-mode "Prisma"
  "Major mode for editing Prisma schema files.

This mode provides syntax highlighting and indentation for Prisma schema files.
It inherits from `prog-mode' to avoid the parsing overhead of `js-mode'.

\\{prisma-mode-map}"
  :syntax-table prisma-mode-syntax-table
  :group 'prisma

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Font lock
  (setq-local font-lock-defaults
              '((prisma-font-lock-keywords-1
                 prisma-font-lock-keywords-2
                 prisma-font-lock-keywords-3)
                nil nil nil nil))
  (setq-local font-lock-multiline nil)

  ;; Indentation
  (setq-local indent-line-function #'prisma-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width prisma-indent-offset)

  ;; Performance: slight defer for smoother scrolling on large files
  (setq-local jit-lock-defer-time 0.1)

  ;; Electric pairs for braces
  (setq-local electric-indent-chars '(?\n ?\} ?\])))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-mode))

;; LSP integration
(lsp! prisma-mode)

(provide 'prisma)
;;; prisma.el ends here
