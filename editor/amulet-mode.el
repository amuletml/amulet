;;; amulet-mode.el --- Editing support for Amulet -*- lexical-binding: t; -*-

;;; Commentary:
;; Just a very quick implementation of syntax highlighting and other editor
;; integration for Amulet.

;;; Code:

(defconst amulet-mode--keywords
  '("forall" "let" "fun" "and" "if" "then" "else" "begin" "end" "in" "external"
    "val" "true" "false" "match" "with" "function" "type" "of" "module" "open"
    "lazy" "as" "class" "instance" "when" "private")
  "All Amulet keywords.  These are just extracted from Lexer.x.")

(defconst amulet-mode--font-lock
  (list
   ;; Keywords
   `(,(regexp-opt amulet-mode--keywords 'words) . font-lock-keyword-face)
   ;; Constructors
   '("\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-type-face)
   ))

(defconst amulet-mode--syntax-table
  (let ((st (make-syntax-table)))
    ;; Underscores and apostrophes are part of identifiers.
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?' "_" st)

    ;; " for strings, \ for deliminators.
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; * is the 2nd character in the start of a 2-char comment sequence, and the
    ;; 1st character in the end of a 2-char comment sequence.  Likewise, ( is
    ;; the first in a 2-char sequence, and ) is the last in a 2-char.
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)

    st)
  "The main syntax table for Amulet.

   Dark magic happens here, and I can't say I understand it.")

(defconst amulet-mode--prettify-symbols
  `(;; Some mathematical symbols
    ("sqrt" . ,(decode-char 'ucs 8730))
    ("&&" . ,(decode-char 'ucs 8743)) ; ∧
    ("||" . ,(decode-char 'ucs 8744)) ; ∨
    ("+." . ,(decode-char 'ucs 8724)) ; ∔
    ("-." . ,(decode-char 'ucs 8760)) ; ∸
    ("*." . ,(decode-char 'ucs 8729)) ; ∙
    ("/." . ,(decode-char 'ucs 247))  ; ÷
    ("<=" . ,(decode-char 'ucs 8804)) ; ≤
    (">=" . ,(decode-char 'ucs 8805)) ; ≥
    ("<>" . ,(decode-char 'ucs 8800)) ; ≠
    ("==" . ,(decode-char 'ucs 8801)) ; ≡
    ("!=" . ,(decode-char 'ucs 8802)) ; ≢
    ("<=>" . ,(decode-char 'ucs 8660)) ; ⇔
    ("infinity" . ,(decode-char 'ucs 8734)) ; ∞
    ("not" . ,(decode-char 'ucs 172)) ; ¬
    ;; Keywords
    ("forall" . ,(decode-char 'ucs 8704)) ; ∀
    ("fun" . ,(decode-char 'ucs 955)) ; λ
    ("<-" . ,(decode-char 'ucs 8592)) ; ←
    ("->" . ,(decode-char 'ucs 8594)) ; →
    ("<=" . ,(decode-char 'ucs 8656)) ; ⇐
    ("=>" . ,(decode-char 'ucs 8658)) ; ⇒
    ("=<<" . ,(decode-char 'ucs 10523)) ; ⤛
    (">>=" . ,(decode-char 'ucs 10524)) ; ⤜
    ;; Some greek letters for type parameters.
    ("'a" . ,(decode-char 'ucs 945))
    ("'b" . ,(decode-char 'ucs 946))
    ("'c" . ,(decode-char 'ucs 947))
    ("'d" . ,(decode-char 'ucs 948))
    ("'e" . ,(decode-char 'ucs 949))
    ("'f" . ,(decode-char 'ucs 966))
    ("'i" . ,(decode-char 'ucs 953))
    ("'k" . ,(decode-char 'ucs 954))
    ("'m" . ,(decode-char 'ucs 956))
    ("'n" . ,(decode-char 'ucs 957))
    ("'o" . ,(decode-char 'ucs 969))
    ("'p" . ,(decode-char 'ucs 960))
    ("'r" . ,(decode-char 'ucs 961))
    ("'s" . ,(decode-char 'ucs 963))
    ("'t" . ,(decode-char 'ucs 964))
    ("'x" . ,(decode-char 'ucs 958)))
  "Prettify various bits of Amulet syntax.

   This isn't on by default, but sure looks cool when it is.")

;;;###autoload
(define-derived-mode amulet-mode prog-mode "Amulet"
  "Major mode for editing Amulet code.

   Currently provides very little actual functionality - just the
   basic syntax highlighting."

  (setq-local paragraph-start (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq indent-tabs-mode nil)

  (setq-local prettify-symbols-alist amulet-mode--prettify-symbols)

  ;; Support for comment/uncomment
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-style 'multi-line)

  ;; Syntax highlighting
  (set-syntax-table amulet-mode--syntax-table)
  (setq-local font-lock-defaults '(amulet-mode--font-lock)))

(with-eval-after-load 'lsp-mode
  ;; If LSP is installed (and loaded), set up editor integration.
  (add-to-list 'lsp-language-id-configuration '(amulet-mode . "amulet"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("/home/squiddev/programming/amulet/.stack-work/install/x86_64-linux-tinfo6/61b4306616bf79a681d016c373f9f630e924d42cf4a06df5cecc4540df6c0e84/8.8.1/bin/amc" "editor"))
    :major-modes '(amulet-mode)
    :server-id 'amc-lsp)))

(provide 'amulet-mode)
;;; amulet-mode.el ends here
