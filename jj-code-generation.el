(defvar jj-code-patterns
  '(("DOM parser" . jj-insert-dom-parser-pattern)
    ("SAX parser" . jj-insert-sax-parser-pattern)
    ("Main method" . jj-insert-main-method)
    ("Read file line by line" . jj-insert-read-file-line-by-line)
    ("XPath expression search" . jj-insert-xpath-pattern)
    ("Regex pattern definition" . jj-insert-regex-pattern)))

(defun jj-code-pattern ()
  "Insert a piece of Java code which is bound to a certain name."
  (interactive)
  (let* ((pattern-name (completing-read "Pattern: " jj-code-patterns))
         (pattern-entry (assoc pattern-name jj-code-patterns)))

    (when pattern-entry
      (let ((pattern-function (cdr pattern-entry)))
        (if (fboundp pattern-function)
            (if (commandp pattern-function)
                (call-interactively pattern-function)
              (funcall pattern-function))
          (error "Unknown pattern `%s'." pattern-name))))))

(defun jj-insert-read-file-line-by-line ()
  (let ((point (point)))
    (insert "FileReader fin = new FileReader(file);\n"
            "try {\n"
            "BufferedReader in = new BufferedReader(fin);\n"
            "for (String line; (line = in.readLine()) != null;) {\n"
            "// TODO //\n"
            "}\n"

            "} finally {\n"
            "fin.close();\n"
            "}")

    (indent-region point (point))

    (jj-assert-throws "java.io" "IOException")
    (jj-assert-imported "java.io" "BufferedReader")
    (jj-assert-imported "java.io" "FileReader")

    (re-search-backward "// TODO" (point-min) t)))

(defun jj-insert-sax-parser-pattern ()
  ;(jj-assert-imported package class)
  (let ((point (point)))
    (insert "try {\n"
            "SAXParser saxParser = SAXParserFactory.newInstance().newSAXParser();\n"
            "saxParser.parse(file, new DefaultHandler() {\n"
            "/**\n"
            "* Receive notification of the start of an element.\n"
            "*/\n"
            "public void startElement(String uri, String localName,\n"
            "String qName, Attributes attributes) {\n"
            "// TODO //\n"
            "}\n\n"
            "/**\n"
            "* Receive notification of the end of an element.\n"
            "*/\n"
            "public void endElement(String uri, String localName, String qName) {\n"
            "// TODO //\n"
            "}\n\n"
            "/**\n"
            "* Receive notification of character data inside an element.\n"
            "*/\n"
            "public void characters(char[] ch, int start, int length) {\n"
            "// TODO //\n"
            "}\n"
            "});\n"
            "} catch (ParserConfigurationException e) {\n"
            "throw new RuntimeException(e);\n"
            "} catch (SAXException sax) {\n"
            "throw new IOException(sax);\n"
            "}\n")

    (indent-region point (point))

    (jj-assert-throws "java.io" "IOException")
    (jj-assert-throws "javax.xml.parsers" "ParserConfigurationException")
    (jj-assert-throws "org.xml.sax" "SAXException")

    (jj-assert-imported "javax.xml.parsers" "SAXParser")
    (jj-assert-imported "javax.xml.parsers" "SAXParserFactory")
    (jj-assert-imported "org.xml.sax.helpers" "DefaultHandler")
    (jj-assert-imported "org.xml.sax" "Attributes")
  
    (re-search-backward "// TODO" (point-min) t 3)))

(defun jj-insert-dom-parser-pattern ()
  ;(jj-assert-imported package class)
  (let ((point (point)))
    (insert "DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();\n"
            "DocumentBuilder parser = dbf.newDocumentBuilder();\n"
            "Document doc = parser.parse(file);\n")

    (indent-region point (point))

    (jj-assert-throws "java.io" "IOException")
    (jj-assert-throws "javax.xml.parsers" "ParserConfigurationException")
    (jj-assert-throws "org.xml.sax" "SAXException")

    (jj-assert-imported "javax.xml.parsers" "DocumentBuilder")
    (jj-assert-imported "javax.xml.parsers" "DocumentBuilderFactory")
    (jj-assert-imported "org.w3c.dom" "Document")))

(defun jj-insert-xpath-pattern ()
  ;(jj-assert-imported package class)
  (let ((point (point)))
    (insert "XPath xpath = XPathFactory.newInstance().newXPath();\n"
            "XPathExpression xp = xpath.compile(\"\");\n\n"
            "NodeList nodeList = (NodeList) xp.evaluate(doc, XPathConstants.NODESET);\n"
            "for (int i = 0, n = nodeList.getLength(); i < n; i++) {\n"
            "Element element = (Element) nodeList.item(i);\n\n"
            "// TODO //\n\n"
            "}\n")

    (indent-region point (point))

    (jj-assert-throws "javax.xml.xpath" "XPathExpressionException")

    (jj-assert-imported "javax.xml.xpath" "XPath")
    (jj-assert-imported "javax.xml.xpath" "XPathConstants")
    (jj-assert-imported "javax.xml.xpath" "XPathExpression")
    (jj-assert-imported "javax.xml.xpath" "XPathFactory")
    (jj-assert-imported "org.w3c.dom" "Element")
    (jj-assert-imported "org.w3c.dom" "NodeList")

    (search-backward "\")" point t)))

(defun jj-assert-throws (package class)
  ;; TODO!

  (jj-assert-imported package class))

(defun jj-insert-main-method (args-length)
  (interactive "nNumber of command line parameters [0]): ")

  (let ((point (point))
        (package-name ".")
        (class-name (jj-guess-class-name)))

    (insert "public static void main(String[] args) throws Exception {\n"
            "if (args.length != " (number-to-string args-length) ") {\n"
            "System.err.println(\"USAGE: java " package-name "." class-name)

    (dotimes (i args-length)
      (insert " <arg" (number-to-string i) ">"))

    (insert "\");\n"
            "System.exit(1);\n"
            "}\n}")

    (unless (looking-at "\\s *$")
      (insert "\n"))

    (indent-region point (point))
    (search-backward "}" point t 2)
    (forward-char)
    (newline)
    (newline-and-indent)))

(defun jj-insert-regex-pattern ()
  (let ((name (read-from-minibuffer "Pattern variable name: ")))
    ;; make sure we're on an empty line; otherwise insert one before
    ;; the current line
    (beginning-of-line)
    (unless (looking-at "\\s-*$")
      (newline-and-indent))
    (indent-according-to-mode)

    ;; if we're on the class level and name matches the usual pattern
    ;; for constants, make the declaration private-static-final
    (when (and (string-match "[A-Z]+\\(?:[A-Z0-9_]\\)*" name)
               (= 1 (- (count-matches "{" (point-min) (point))
                       (count-matches "}" (point-min) (point)))))
      (insert "private static final "))

    ;; insert pattern declaration and move point into the empty string
    (insert "Pattern " name " = Pattern.compile(\"\");\n")
    (when (looking-at "\\s-*$")
      (kill-line))
    (backward-char 4)))

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
