(defun jj-new-class()
  (interactive)
  (let ((class (jj-guess-class-name))
	(package (jj-guess-package))
	(class-or-interface (jj-guess-class-or-interface))
	(extends (jj-guess-extends))
	(start-of-signature 0))
    (when package
      (insert (concat "package " package ";\n\n")))

    ;; insert known imports ;;
    (when (and extends 
               (string-match "\\(.+\\.\\)[A-Z]" extends))
      (let ((import (match-string 1 extends)))
        (if (not (string-equal import "java.lang."))
            (insert (concat "import " import "*;\n\n")))))

    (setq start-of-signature (point))

    ;; insert signature ;;
    (insert (concat "public " class-or-interface " " class))

    (when extends
      (insert " extends ")
      (if (string-match "\\.\\([^\\.]+\\)$" extends)
          (insert (match-string 1 extends))
        (insert extends)))

    ;; insert start of class block
    (insert " {\n")

    ;; insert serial version uid ;;
    (when (and extends (string-match extends "java.lang.Exception"))
      (insert "private static final long serialVersionUID = 1;\n\n"))

    ;; insert constructors
    (if extends
	(mapc #'(lambda(x)
		 (let ((key (car x))
		       (code (cdr x)))
		   (when (string-match key extends)
		     (eval code))))
	      '(("java.lang.Exception"
		 . (progn (insert 
			   (concat 
			    (make-string jj-indentation-offset ?\ )
			    "public " class "() {}\n"
			    (make-string jj-indentation-offset ?\ )
			    "public " class "(String message) {\n"
			    (make-string (* 2 jj-indentation-offset) ?\ )
			    "super(message);\n"
			    (make-string jj-indentation-offset ?\ )
			    "}\n\n"
			    (make-string jj-indentation-offset ?\ )
			    "public " class "(Throwable cause) {\n"
			    (make-string (* 2 jj-indentation-offset) ?\ )
			    "super(cause);\n"
			    (make-string jj-indentation-offset ?\ )
			    "}\n"))))
		("java.util.EventObject"
		 . (progn (insert
			   (concat
			    (make-string jj-indentation-offset ?\ )
                            "private static final long serialVersionUID = 1;\n\n"
			    (make-string jj-indentation-offset ?\ )
			    "public " class "(Object source) {\n"
			    (make-string (* 2 jj-indentation-offset) ?\ )
			    "super(source);\n"
			    (make-string jj-indentation-offset ?\ )
			    "}\n"))))))
      (insert "\n"))
	       		
    ;; insert end of class block
    (insert "}")

    (goto-char start-of-signature)
    (jj-insert-doc-template class-or-interface)

    (next-line 1)
    (indent-according-to-mode)))

(defun jj-guess-class-name()
  (let ((buffer-name (buffer-name)))
    (if (string-match "^\\(.+\\)\\.java\\(<.+>\\)?$" buffer-name)
	(match-string 1 buffer-name)
      ""))
  )

;; this class tries to infer from the buffer-name
;; if this file is a "class" or an "interface" and returns
;; the respective string.
(defun jj-guess-class-or-interface()
  (let ((map-or (function (lambda (TEST LIST)
			    (if LIST
				(or (funcall TEST (car LIST) (jj-guess-class-name))
				    (funcall map-or TEST (cdr LIST)))
			      nil)))))
    (if (funcall map-or 'string-match '("Listener$"))
	"interface"
      "class")
    )
  )

;; this method tries to guess the parent class
;; of a newly created class by looking at the
;; last letters of the class name.
(defun jj-guess-extends()
  (assoc-default
   (jj-guess-class-name)

   ;; -->  rules for extends matches ;;
   '(("Exception$" . "java.lang.Exception")
     ("Event$" . "java.util.EventObject")
     ("Panel$" . "javax.swing.JPanel")
     ("Frame$" . "javax.swing.JFrame"))
   ;; <-- end of rules

   'string-match
   )
  )

(defun jj-guess-package()
  (let ((package "")
	(package-length 0))

    ;; Strategy 1:
    ;; look for other .java buffers visiting a file in the same directory
    (let ((buffer-list (jj-buffer-list))
	  (current-buffer-path (substring (buffer-file-name) 0 (- (length (buffer-file-name)) (length (buffer-name)))))
	  (other-buffer)
	  (other-buffer-name)
	  (other-buffer-name-length))

      (while buffer-list
	(progn (setq other-buffer (car buffer-list))
	       (setq other-buffer-name (buffer-name other-buffer))
	       (setq other-buffer-name-length (length other-buffer-name))

	       (if (and 
		    ;; this buffer is not current buffer
		    (not (eq other-buffer (current-buffer)))

		    ;; && this buffer's file is in the same 
		    ;;    directory as the current buffer...
		    (string-equal current-buffer-path 
				  (substring (buffer-file-name other-buffer) 
					     0
					     (- (length (buffer-file-name other-buffer)) other-buffer-name-length)))
		    )

		   ;; .. then read this buffer's "package" statement
		   (save-excursion
		     (set-buffer other-buffer)
		     (setq package (jj-current-package))
		     (setq package-length (length package))
		     ))
	       (setq buffer-list (cdr buffer-list)))))

    ;; Strategy 2:
    ;; look for 'src' directory in this buffer's file's path
    (when (= 0 package-length)
      (let ((buffer-file-name (buffer-file-name)))
        (when (and (string-match (concat "/src/\\(\\(.*\\)/\\)?[^/]+\\.java$") buffer-file-name)
                   (match-string 2 buffer-file-name))
          (setq package (replace-regexp-in-string "/" "." (match-string 2 buffer-file-name))))))

    ;; return package
    (if (string-equal "" package)
	nil
      package)
    )
  )

;; returns all existing live buffers 
;; visiting a *.java file
(defun jj-buffer-list()
  (filter-list (buffer-list)
	       (function 
		(lambda (buffer)
		  (string-match "\\.java$" (buffer-name buffer)))))
  )

;; returns a list consisting of all elements
;; in LIST for which PREDICATE returns not nil.
(defun filter-list (LIST PREDICATE)
  (if LIST
      (let ((list-car (car LIST))
	    (filtered-list-cdr (filter-list (cdr LIST) PREDICATE)))
	(if (funcall PREDICATE list-car)
	    (cons list-car filtered-list-cdr)
	  filtered-list-cdr))
    nil))

;; Reads the package statement of the current
;; .java buffer.
(defun jj-current-package()
  (save-excursion
    (goto-char 0)
    (if (re-search-forward "^\\s *package\\s +\\(.*\\);" (point-max) t)
	(match-string 1)
      "")))

;; Inserts the Javadoc comment above
;; a class or interface declaration.
(defun jj-insert-doc-template(template-name)
  ;; TODO ;;
  (insert (concat "/**\n"
		  " * Describe " (jj-guess-class-or-interface) 
		  " <code>" (jj-guess-class-name) "</code> here.\n"
		  " *\n"
		  " * @author <a href=\"mailto:" (jj-user-email-address) "\">"
		  (user-full-name)
		  "</a>\n"
		  " * @version 1.0\n"
		  " */\n"))
  )

;; You may want to override the body of
;; this function if you'd like a different
;; email address to be inserted into the
;; Javadoc comments.
(defun jj-user-email-address()
  (interactive)
  user-mail-address)

(provide 'jj-legacy)

