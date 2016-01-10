(defun jj-guess-project-directory ()
  (let ((package (jj-package)))
    (unless package
      (setq package (jj-guess-package))
)

(provide 'jj-project)

