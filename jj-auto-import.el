(defun jj-assert-imported (package class)
  "Checks if the given `class' in the given `package' are
imported in the current java file and if not, inserts it using
`jj-add-import'."
  (when (and package class)
    ;; first check if the package is the same as this class's package,
    ;; because then we don't have to import it.
    (unless (string= package (jj-current-package))
      (unless (jj-imported-p package class)
        (jj-add-import package class)))))

(defun jj-add-import (package class)
  "Inserts an import statement at the beginning of the file for
package.class"
  
  (save-excursion
    (goto-char (point-min))

    (let ((package-class (concat package "." class))
          (best-position (point))
          (best-match ""))

      (while (re-search-forward "^\\s *import\\s +\\([^;]+\\);" (point-max) t)
        (when (and (string< best-match (match-string 1))
                   (string< (match-string 1) package-class))
          (setq best-match (match-string 1)
                best-position (line-beginning-position 2))))

      (goto-char best-position)
      (when (and (= best-position (point-min))
                 (re-search-forward "^\\s *package\\s +[^;]+;" (point-max) t))
        (forward-line)
        (unless (looking-at "^\\s *$")
          (open-line 1))
        (forward-line))

      (insert "import " package-class ";\n")
      (unless (looking-at "^\\s *\\(import\\b.*\\)?$")
        (newline)))))

(defun jj-imported-p (package class)
  "Checks if the given `class' in the given `package' is imported
in the current java file. If `package' is nil, returns a non-nil
value if there is any import statement that imports a class with
the name `class' (wildcard imports are ignored). If `package'
non-nil returns a non-nil value if either the full package or the
`class' inside the package is imported. If class is nil, returns
nil."
  (when class
    (let ((regexp (concat "^\\s *import\\s +"
                          (if package
                              (concat (regexp-quote package) "\\.\\(\\*\\|" (regexp-quote class)  "\\)")
                            (concat "\\([^;]*\\.\\)?" (regexp-quote class)))
                          "\\s *;")))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward regexp (point-max) t)))))

(provide 'jj-auto-import)
