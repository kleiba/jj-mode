(defmacro with-save-point (&rest body)
  "Saves the current position of point, executes `forms' and
restores the previous position of point."
  (list 'let `((jj--point84359843653 (point)) (jj--ret2435234 ,(cons 'progn body)))
        '(goto-char jj--point84359843653) 'jj--ret2435234))

; done
(provide 'jj-util)
