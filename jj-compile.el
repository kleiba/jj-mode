(require 'jj-util)
(require 'compile)

(defvar jj--parsed-compiler-output
  (make-hash-table :test 'equal)
  "Used internally to highlight compile errors in buffers.")

(defun jj-build-project (&optional clean)
  (interactive "P")
  (let ((ant-file (jj-find-ant-file)))
    (if ant-file
        (compile (concat "ant -e -f " ant-file (if clean " clean" "") " compile"))
      (error "Cannot find build.xml file."))))

(defun jj-find-ant-file ()
  (let ((file (buffer-file-name))
        dir)
    (when file
      (setq dir (file-name-directory file))
      (while (not (or (string-equal "/" dir)
                      (file-exists-p (setq file (concat dir "build.xml")))))
        (setq dir (file-name-directory (directory-file-name dir))))
      (unless (string-equal "/" dir)
        (concat dir "build.xml")))))

(defun jj-idle-compile-buffer ()
  ;; determine package structure of current file

  ;; create temporary directory structure reflecting this file's
  ;; package structure

  ;; copy current buffer to a temp file in the newly created directory

  ;; find the libraries on the class path and the current source
  ;; directory

  ;; invoke javac

  ;; check javac's output messages for warnings and errors pertaining
  ;; to the current buffer

  ;; insert text properties highlighting the 
)

(defun jj-parse-compiler-output ()
  "Parses the output of javac/ant and returns them in a
hashtable."
  ;; (unless (eq major-mode 'compilation-mode)
  ;;   (error "Buffer not in compilation mode: %s" (buffer-name)))
  
  (with-save-point
   (let ((compiler-output (make-hash-table :test 'equal))
         (regexps '(("javac" "^\\([^: \t\r\n]+\\):\\([0-9]+\\): \\(error\\|warning\\): \\(.*\\)\\(?:.\\|\n\\)*?\\(^[^^]*\\)^\\(?:\\(?:.\\|\n\\)+?symbol:.* \\([^ ]+\\)$\\)?\\(?:\\(?:.\\|\n\\)+?location:.* \\([^ \t\r\n]+\\)\\)?" ((file 1) (line 2 string-to-number) (col 5 length) (type 3) (message 4) (rcontext 6) (lcontext 7)))))
         compiler entry)
     ;; TODO: parse buffer to see which compiler was used
     (setq compiler "javac")

     ;; VERDICT: just use own regexp! Should work with:
     ;; - javac
     ;; - ant
     ;; - ant -e

     ;; parse errors in the compiler's error format
     (when (setq entry (assoc compiler regexps))
       (let* ((regexp (nth 1 entry))
              (groups (nth 2 entry)))
         (goto-char (point-min))

         ;; find all error messages
         (while (re-search-forward regexp (point-max) t)
           (message "full match: %s" (match-string 0));;DEBUG
           (let (parse-result)
             ;; parse error message
             (dolist (group-entry groups)
               (let ((value (match-string (cadr group-entry)))
                     (conversion-function (nth 2 group-entry)))
                 ;; in case the expected type of an entry is not
                 ;; "string", i.e., needs to be converted somehow from
                 ;; the match-string, a conversion function can be
                 ;; given as the third argument of a group-entry. This
                 ;; is useful mostly to convert line and column numbers
                 ;; to integers.
                 (when conversion-function
                   (setq value (apply conversion-function value nil)))
                 (setq parse-result (cons (cons (car group-entry) value) parse-result))))

             ;; DEBUG
             (message "line: %s" (assoc 'line parse-result))

             ;; store the parse results of all error messages
             ;; pertaining to the same filename in a list
             (let* ((file-name (cdr (assoc 'file parse-result)))
                    (file-name-outputs (gethash file-name compiler-output)))
               (puthash file-name
                        (if file-name-outputs (cons parse-result file-name-outputs)
                          (list parse-result))
                        compiler-output))))))
     compiler-output)))

(defun jj-underline-region-match (buffer from to regexp color)
  "Underlines a portion of the given `buffer' with `color'
if (and only if) it matches the given regex. The region to be
marked is given by `from' and `to' which both are of the
form (line . column). The column of `from' is understood as
inclusive, while that of `to' as exclusive."
  (when (get-buffer buffer)
    (save-excursion
      (set-buffer buffer)
      (let ((point (point))
            (mark (mark))
            (from-line (car from))
            (from-col (cdr from))
            (to-line (car to))
            (to-col (cdr to))
            region-beginning region-end deactivate-mark)
        ;; find the from position in the buffer
        (goto-char 0)
        (forward-line (1- from-line))
        (forward-char (1- from-col))
        (setq region-beginning (point))
        ;; find the to position relative to from
        (forward-line (- to-line from-line 1))
        (forward-char (- to-col (current-column) 1))
        (setq region-end (point))
        ;; insert highlight if the region matches the regexp
        (when (string-match regexp (buffer-substring region-beginning region-end))
          (overlay-put (make-overlay region-beginning region-end) 'face `(:underline ,color)))
        ;; restore point and mark
        (set-mark mark)
        (goto-char point)))))
  

(provide 'jj-compile)

