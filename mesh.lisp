(defpackage #:mesh.goof
  (:use :cl)
  (:documentation "LISP Web Spider")
  (:export (#:fix-link
	    #:fix-links
	    #:find-links)

(in-package :mesh.goof)

(defun is-full-uri (uri)
  "Check if the URI is actually a URI."
  ;; If the length of the part of a string before : is not equal to the lenth of the string, then it is a URI.
  (if (multiple-value-bind (list len) (split-sequence:split-sequence #\: uri)
	(equal (length (first list)) len)) nil t))

(defun fix-link (site path)
  "Doctor a link to be a full URI."
  ;; Domain is the site without paths.
  (let* ((domain (let ((seq (split-sequence:split-sequence #\/ site)))
		   (concatenate 'string
				(first seq) "//"
				(third seq) "/")))
	 (starts-with-/ (find #\/ path)))
    (cond
      ;; It's already a full URI.
      ((is-full-uri path) path)
      ;; If it doesn't start with /, it's a relative path.
      ((not starts-with-/) (concatenate 'string site path))
      ;; If it does start with /, it's relative to the site root.
      (starts-with-/ (concatenate 'string domain (string-left-trim "/" path))))))

(defun fix-links (site links)
  "Take a list of improper lists containing name and path and fix them per fix-link."
  (mapcar #'(lambda (node)
	      (let* ((name (car node))
		     (path (cdr node)))
		(cons name (fix-link site path)))) links))

(defun find-links (site)
  "Visit a website and obtain a list of websites that are linked to."
  ;; This isn't very functional.
  (let* ((links ())
	 (str (drakma:http-request site))
	 (document (chtml:parse str (cxml-stp:make-builder))))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element)
			  (equal (stp:local-name a) "a"))
	;; LINKS will be a list of improper lists containing the name of the link and the link path.
	(pushnew (cons
		  (stp:string-value a)
		  (fix-link site (stp:attribute-value a "href")))
		 links)))
    links))
