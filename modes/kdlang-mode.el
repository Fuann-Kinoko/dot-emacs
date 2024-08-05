(require 'rx)

(defgroup kdlang-mode nil
  "Major mode for editing Simple Declarative Language files."
  :link '(url-link "https://kdl.dev/")
  :group 'languages)

;; (defface kdlang-mode-comment-face
;;   '((t :inherit font-lock-comment-face))
;;   "Font for comments."
;;   :group 'kdlang-mode)

(defface kdlang-mode-tag-face
  '((t :inherit font-lock-keyword-face))
  "Font for tag names."
  :group 'kdlang-mode)

(defface kdlang-mode-namespace-face
  '((t :inherit font-lock-type-face))
  "Font for tag namespaces."
  :group 'kdlang-mode)

(defface kdlang-mode-attribute-face
  '((t :inherit font-lock-variable-name-face))
  "Font for attribute names."
  :group 'kdlang-mode)

(defface kdlang-mode-punctuation-face
  '((t :inherit default))
  "Font for punctuation characters."
  :group 'kdlang-mode)

(defface kdlang-mode-string-face
  '((t :inherit font-lock-string-face))
  "Font for string values."
  :group 'kdlang-mode)

(defface kdlang-mode-number-face
  '((t :inherit default))
  "Font for numeric values."
  :group 'kdlang-mode)

(defface kdlang-mode-boolean-face
  '((t :inherit font-lock-constant-face))
  "Font for boolean/null values."
  :group 'kdlang-mode)

(defface kdlang-mode-date-face
  '((t :inherit kdlang-mode-number-face))
  "Font for numeric values."
  :group 'kdlang-mode)

(defface kdlang-mode-data-face
  '((t :inherit font-lock-string-face))
  "Font for embedded binary data."
  :group 'kdlang-mode)

(defface kdlang-mode-error-face
  '((t :inherit font-lock-warning-face))
  "Font for invalid syntax."
  :group 'kdlang-mode)

(defvar kdlang-mode-font-lock-keywords)
(setq kdlang-mode-font-lock-keywords
  `(
    ;; Binary data
    ("\\["
     (0 'kdlang-mode-punctuation-face)
     ("[a-zA-Z0-9 /+]+"
      (kdlang-mode-end-of-data)
      nil
      (0 'kdlang-mode-data-face))
     ("\\]"
      (kdlang-mode-end-of-data)
      nil
      (0 'kdlang-mode-punctuation-face)
      )
     ("[^]\n +/-9A-[a-z]"
      (kdlang-mode-end-of-data)
      nil
      (0 'kdlang-mode-error-face))
     )

    ;; Comments
    ;; ("\\(?:--\\|//\\|#\\).*$" . 'kdlang-mode-comment-face)

    ;; WYSIWYG strings
    ;; ("`.*`" . 'kdlang-mode-string-face)

    ;; Date/time values
    (,(rx
       bow
       (= 4 (any (?0 . ?9)))
       "/"
       (= 2 (any (?0 . ?9)))
       "/"
       (= 2 (any (?0 . ?9)))
       (zero-or-one
	" "
	(= 2 (any (?0 . ?9)))
	":"
	(= 2 (any (?0 . ?9)))
	(zero-or-one
	 ":"
	 (= 2 (any (?0 . ?9)))
	 (zero-or-one
	  "."
	  (one-or-more (any (?0 . ?9)))))
	(zero-or-one
	 "-"
	 (= 3 (any (?A . ?Z)))))
       eow) . 'kdlang-mode-date-face)

    ;; Time / duration values
    (,(rx
       bow
       (zero-or-one
	(one-or-more (any (?0 . ?9)))
	"d:")
       (= 2 (any (?0 . ?9)))
       ":"
       (= 2 (any (?0 . ?9)))
       (zero-or-one
	":"
	(= 2 (any (?0 . ?9)))
	(zero-or-one
	 "."
	 (one-or-more (any (?0 . ?9)))))
       eow) . 'kdlang-mode-date-face)

    ;; Numeric values
    ("\\<[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][+-]?[0-9]+\\)?L?B?D?f?\\>" . 'kdlang-mode-number-face)

    ;; Boolean values
    ("\\<\\(true\\|false\\|on\\|off\\|null\\)\\>" . 'kdlang-mode-boolean-face)

    ;; Attributes
    (,(rx
       bow
       (submatch (one-or-more (syntax word)))
       (submatch "="))
     (1 'kdlang-mode-attribute-face)
     (2 'kdlang-mode-punctuation-face))

    ;; Tags
    (,(rx
       (or
	(submatch ?\;)
	bol)
       (zero-or-more
	(syntax whitespace))
       bow
       (zero-or-one
	(submatch (one-or-more (syntax word)))
	(submatch ":"))
       (submatch (one-or-more (syntax word)))
       eow)
     (1 'kdlang-mode-punctuation-face nil t)
     (2 'kdlang-mode-namespace-face nil t)
     (3 'kdlang-mode-punctuation-face nil t)
     (4 'kdlang-mode-tag-face))

    ;; Punctuation
    (,(rx (any ?{ ?})) . 'kdlang-mode-punctuation-face)
    (,(rx (any ?\\ ?\;) eol) . 'kdlang-mode-punctuation-face)

    ;; Anything else is an error
    ("[^ \n]" . 'kdlang-mode-error-face)
    ))

(defvar kdlang-mode-syntax-table nil "Syntax table for `kdlang-mode'.")

(setq kdlang-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; (modify-syntax-entry ?\/ ". 14" table) ; /*
        ;; (modify-syntax-entry ?* ". 23" table)  ; */
        (modify-syntax-entry ?\/ ". 124ab" table) ; /* */ and //
        (modify-syntax-entry ?* ". 23" table)  ; /* */
        (modify-syntax-entry ?# "< b" table)   ; #
        (modify-syntax-entry ?- "w 12b" table) ; --
        (modify-syntax-entry ?\n "> b" table)  ; end of // -- #

	(modify-syntax-entry ?` "\"" table)

	;; (modify-syntax-entry ?- "w" table)
	(modify-syntax-entry ?_ "w" table)
	(modify-syntax-entry ?. "w" table)
        table))

(defun kdlang-mode-end-of-data (&rest foo)
  "Return position of the end of data blocks.  FOO is ignored."
  (search-backward "[")
  (save-excursion
    (search-forward "]" nil t)
    (point)))

;;;###autoload
(define-derived-mode kdlang-mode prog-mode "Kdlang"
  "Major mode for editing Simple Declarative Language files."

  :group 'kdlang-mode

  ;; Settings
  (setq-local font-lock-multiline t)

  ;; Comments
  ;; (setq-local comment-start "# ")
  (setq-local comment-start "// ")
  (setq-local comment-end   "")

  ;; Syntax
  (setq-local font-lock-defaults '(kdlang-mode-font-lock-keywords
                                   nil nil nil nil))
  ;; Raw string literals
  (setq-local
   syntax-propertize-function
   (syntax-propertize-rules
    ((rx
      "`"
      (minimal-match
       (zero-or-more
	(not (any "`\\"))))
      (minimal-match
       (one-or-more
	(submatch "\\")
	(minimal-match
	 (zero-or-more
	  (not (any "`\\"))))))
      "`")
     (1 ".")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdlang-mode))

(provide 'kdlang-mode)
;;; kdlang-mode.el ends here
