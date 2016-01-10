(defun jj-insert-system-out-err-println ()
  (interactive)
  (let* ((insertions '(("System.out.println(" . ");")
                       ("System.err.println(" . ");")))
         next found)
    (dolist (insertion insertions)
      (if found 
          (unless next (setq next insertion))
        (when (and (looking-at (cdr insertion)) (looking-back (car insertion)))
          (kill-region (- (point) (length (car insertion))) (+ (point) (length (cdr insertion))))
          (setq found t))))

    (unless next (setq next (car insertions)))
    (indent-according-to-mode)
    (insert (car next) (cdr next))
    (backward-char (length (cdr next)))))

(defun jj-newline-and-indent ()
  "This function first checks if we're currently neither inside a
string-literal nor a comment, i.e., in actual code. If this is
the case, and the first non-whitespace character (if any) on the
current line is a closing curly parenthesis, we insert not one,
but two newlines, indent the result properly, and put 'point' on
the newly created empty line. In all other cases,
`newline-and-indent' is called."
  (interactive)
  (if (and (looking-at "\\(\\s-*\\)}")
           (let ((syntax-ppss (syntax-ppss)))
             (and (null (nth 2 syntax-ppss)) (null (nth 3 syntax-ppss)))))
      (progn (replace-match "\n\n" nil nil nil 1)
             (jj-indent-line)
             (previous-line)
             (jj-indent-line))
    (newline-and-indent)))

(defun jj-open-line-transpose-indent (n)
  (interactive "*p")
  ;; open line and transpose n-times
  (let ((point (point)))
    (kill-line)
    (beginning-of-line (+ n 2))
    (yank)
    (newline)
    (end-of-line 2)
    (indent-region point (point))
    (goto-char point)))

(defun jj-open-line-and-indent (n)
  "This function works similar to `open-line' but also indents
the new line after point."
  (interactive "*p")
  (let ((point (point)))
    (insert (make-string (min n 256) ?\n))
    (end-of-line)
    (indent-region point (point))
    (goto-char point)))

(provide 'jj-code-generation)
