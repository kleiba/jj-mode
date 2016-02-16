(require 'jj-code-generation)
(require 'jj-compile)
(require 'jj-util)

;; until the transition to the new jj-mode has been completed,
;; the following file provides it using legacy code:
(require 'jj-legacy)

(defcustom jj-minor-modes-hook '(subword-mode electric-pair-mode)
    "A list of minor-modes invoked together with `jj-mode'."
    :group 'jj
    :type '(hook))

(defcustom jj-indentation-offset 4
    "The size of the indentation in spaces. (Note that
indentation might be performed with tabs instead of spaces
depending on the variable `indent-tabs-mode'."
    :group 'jj
    :type '(integer))

(define-derived-mode jj-mode nil "JJ"
  "An alternative Java mode"
  :group 'jj
  :syntax-table (make-syntax-table)

  ;; syntax table, syntax highlighting, keymap
  (jj-setup-syntax-table)
  (jj-setup-font-lock-defaults)
  (jj-setup-keymap)

  ;; indentation
  (setq-local indent-line-function 'jj-indent-line)
  (add-hook 'post-self-insert-hook 'jj-post-self-insert-function)

  ;; code formatting
  (add-hook 'completion-at-point-functions 'jj-fixup-whitespace)

  ;; comments
  (setq-local comment-start "//")
  (setq-local comment-end "")

  ;; paragraph filling
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local fill-paragraph-function 'jj-fill-paragraph)

  ;; activate associated minor-modes
  (run-hooks 'jj-minor-modes-hook))

(defun jj-indent-line ()
  (let ((point (point))
        (old-column (current-column))
        (old-indentation (current-indentation))
        syntax-ppss new-indentation)
    ;; syntactic analysis of the first non-whitespace character of the
    ;; current line
    (move-to-column old-indentation)
    (setq syntax-ppss (syntax-ppss))

    (let ((last-open-paren (nth 1 syntax-ppss)))
      (setq new-indentation
            ;; if we're inside function parameters, indent until one
            ;; after the opening parenthesis
            (or (and last-open-paren
                     (with-save-point (goto-char last-open-paren)
                                      (when (looking-at "(")
                                        (1+ (current-column)))))
        
                (* (- (car syntax-ppss)
                      ;; check if we're looking at the end
                      ;; of a sexp
                      (if (= (char-syntax (char-after)) 41) 1 0))
                   jj-indentation-offset))))

    ;; exceptions
    (setq new-indentation
          (cond 
           ;; multi-line comments get an extra character to line up
           ;; the stars TODO: check if comment line starts with a star
           ;; or not
           ((nth 8 syntax-ppss) (1+ new-indentation))
           ;; the cases in 'switch' statements are indented one level
           ;; less
           ((looking-at "\\(?:case\\s-+[^:]+\\|default\\s-*\\):") 
            (- new-indentation jj-indentation-offset))
           (t new-indentation)))

    (indent-line-to new-indentation)
    (when (> old-column old-indentation)
      (goto-char (+ point (- new-indentation old-indentation))))))

(defun jj-fixup-whitespace ()
  (when (looking-at "\\s-")
    '(lambda () 
       (fixup-whitespace)
       (when (looking-at "\\s-\\S-")
         (forward-char 1)))))

(defun jj-fill-paragraph (&optional JUSTIFY)
  ;; only fill when inside comment
  (when (nth 4 (syntax-ppss))
    (fill-comment-paragraph JUSTIFY))
  t)

(defun jj-setup-keymap ()
  (define-key jj-mode-map "\r" 'jj-newline-and-indent)
  (define-key jj-mode-map (kbd "C-j") 'jj-open-line-transpose-indent)
  (define-key jj-mode-map (kbd "C-o") 'jj-open-line-and-indent)
  (define-key jj-mode-map (kbd "C-c C-p") 'jj-insert-system-out-err-println)
  (define-key jj-mode-map (kbd "<f12>") 'jj-build-project))

(defun jj-post-self-insert-function ()
  (when (eq major-mode 'jj-mode)
    (let ((char last-command-event))
      (when (member char '(?: ?}))
        (jj-indent-line)))))

(defun jj-setup-syntax-table ()
  (let ((syntax-table (syntax-table)))
    (modify-syntax-entry ?/ ". 124" syntax-table)
    (modify-syntax-entry ?* ". 23b" syntax-table)
    (modify-syntax-entry ?' "\"" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)))

(defun jj-setup-font-lock-defaults ()
  (let ((keywords           '("abstract" "assert" "break" "case" "catch" "class" "continue"
                              "default" "do" "extends" "else" "enum" "final" "finally" "if" 
                              "implements" "import" "interface" "instanceof" "for" "new" 
                              "package" "private" "protected" "public" "return" "static" 
                              "synchronized" "switch" "this" "throw" "throws" "try" "volatile" 
                              "while"))
        (built-in-types     '("boolean" "byte" "char" "double" "float" "int" "long" "void"))
        (built-in-constants '("true" "false" "null"))
        (package-or-import   "\\<\\(?:package\\|import\\)\\s-+\\([^;]+\\);")
        (package-names       "\\<\\(\\(?:[a-z]+\\.\\)*[a-z]+\\.\\)[A-Z]")
        (class-name-regex    "\\<[A-Z]+[a-z0-9][A-Za-z0-9]+\\>")
        (function-name-regex "\\<\\(\\(?:_+\\|[a-z]\\)[A-Za-z_]+[0-9_]*\\)\\( *(\\)")
        (variable-name-regex "\\<\\(?:_+\\|[a-z]\\)[A-Za-z_]*[0-9_]*\\>")
        (constant-name-regex "\\<[A-Z_]+\\>"))
    (setq font-lock-defaults 
          `((,(cons (regexp-opt keywords 'words) 'font-lock-keyword-face)
             ,(cons (regexp-opt built-in-types 'words) 'font-lock-type-face)
             ,(cons (regexp-opt built-in-constants 'words) 'font-lock-constant-face)
             (,package-or-import 1 font-lock-preprocessor-face)
             (,package-names 1 font-lock-preprocessor-face)
             (,class-name-regex . font-lock-type-face)
             (,function-name-regex 1 font-lock-function-name-face)
             (,variable-name-regex . font-lock-variable-name-face)
             (,constant-name-regex . font-lock-constant-face))
            nil nil 
            ((?/ . ". 124")
             (?* . ". 23b")
             (?' . "\"")
             (?\n . ">"))))))
