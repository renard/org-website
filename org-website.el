;;; org-website.el --- Generate multi-level website from org

;; Copyright © 2010 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: Emacs, org
;; Last changed: 2011-10-25 20:29:03

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Required modules:
;;   - `string-template': http://elisp-string-template.git.chezwam.org
;;   - `dirtree':
;;     http://www.splode.com/~friedman/software/emacs-lisp/src/dirtree.el
;;
;; This basically setup some hooks to be launched when publishing. Used hooks
;; are: `org-publish-before-export-hook' and `org-publish-after-export-hook'.
;;
;; In addition all variables from `org-website-publish-with-vars' are visible.
;; Refer to `org-website-publish-with-vars' for further information.

;;; Code:

(require 'string-template)
(require 'org-publish)
(require 'org-html)
(require 'dirtree)
(require 'cl)

(defmacro org-website-publish-with-vars (&rest body)
  "Execute BODY with a declaration of following variables after
setting following variables:

  - `filename-rel': Relative relative path of `filename' from the
     project source directory (`base-dir').
  - `file-dir': The `filename' directory relative to `base-dir'.
  - `ppub-dir': The project output directory.
  - `pub-file': The name of the output file beeing published.
  - `pub-file-rel': Relative path of `pub-file' from `ppub-dir'.
  - `pub-file-dir': Directory name of `pub-file-rel'.
  - `path-to-root': Relative path from file beeing published to
    `base-dir'.

As long as these hooks are run by `org-publish-org-to', several
additionnal variables are set by upsteam functions:

Variables visible from `org-publish-before-export-hook':
  * from `org-publish-file':
    - `filename': is the filename of the org file to be published.
    - `project': the name of current project to be published.
    - `project-plist': is the property list for the given project.
    - `ftname': the truename of `filename'.
    - `publishing-function': the function(s) to be used for publication.
    - `base-dir': the source directory of the project to be published.
    - `pub-dir': the output directory of the file beeing be published.
  * from `org-publish-org-to':
    - `plist': alias for `project-plist'
    - `init-buf': the buffer visiting `filename' (current-buffer).
    - `init-buf-string': is the `init-buf' buffer contents.

Variables visible from `org-publish-after-export-hook':
  * All variables visible from `org-publish-before-export-hook'.
  * from `org-publish-org-to'
    - `export-buf-or-file': the buffer containing the export output."
  `(let* ((filename-rel (file-relative-name filename base-dir))
	  (file-dir (file-name-directory filename-rel))
	  (ppub-dir (expand-file-name (plist-get project-plist
						 :publishing-directory)))
	  (pub-file (if (boundp 'export-buf-or-file)
			(if (bufferp export-buf-or-file)
			    (buffer-file-name export-buf-or-file)
			  export-buf-or-file)
		      nil))
	  (pub-file-rel (if pub-file
			    (file-relative-name pub-file ppub-dir)
			  nil))
	  (pub-file-dir (if pub-file-rel
			    (file-name-directory pub-file-rel)
			  nil))
	  (path-to-root (file-relative-name base-dir (file-name-directory
						      filename))))
     ,@body))


(defcustom org-website-publish-admonition-header
  "<div class=\"admonition ${admo}\"><p class=\"header\">${admo-title}</p>"
  "HTML fragment header to be used when publishing an admonition using
`org-website-publish-admonition'.

Expansed items are:

admo

    The lowercase admonition type.

admo-title

    The raw admonition type."
  :type 'string
  :group 'org-publish-website)

(defcustom org-website-publish-admonition-footer
  "</div>"
  "HTML fragment footer to be used when publishing an admonition using
`org-website-publish-admonition'.

Same as `org-website-publish-admonition-header'."
  :type 'string
  :group 'org-publish-website)


(defcustom  org-website-publish-include-header
  "<div class=\"include\"><p><a href=\"${file}\">Download ${file}</a></p>"
  "HTML fragment header to be used when publishing an admonition using
`org-website-publish-include'.

Expensed items are:

file

    The included file."
  :type 'string
  :group 'org-publish-website)

(defcustom  org-website-publish-include-footer
  "</div>"
  "HTML fragment footer to be used when publishing an admonition using
`org-website-publish-include'.

Same as `org-website-publish-include-header'."
  :type 'string
  :group 'org-publish-website)



(defcustom org-website-what-s-new-header
  "<div class=\"what-s-new\"><ul>"
  "HTML fragment header to be used when publishing changes using
`org-website-publish-what-s-new'.")

(defcustom org-website-what-s-new-footer
  "</ul></div>"
  "HTML fragment footer to be used when publishing changes using
`org-website-publish-what-s-new'.")

(defcustom org-website-what-s-new-item
  "<li>
<span class=\"date\">${date}</span>
<a href=\"<lisp>path-to-root</lisp>${file}\">
<span class=\"description\">${desc}</span>
</a>
</li>"
  "HTML fragment to be used when publishing changes using
`org-website-publish-what-s-new'.")

(defvar org-website-alist nil
  "List of all website to publish.

Syntax of each item is:

   (site-name . \"/path/to/site/configuration/file.el\")
")


(defun org-website-publish-admonition ()
  "Publish an admonition in HTML mode.

Admonitions are specially marked topics that can appear
anywhere an ordinary body element can. They contain arbitrary
body elements. Typically, an admonition is rendered as an offset
block in a document, sometimes outlined or shaded, with a title
matching the admonition type. For example:

#+BEGIN_ADMONITION type
Some text inside the admonition
#+END_ADMONITION

This directive might be rendered something like this:

+---------------------------------+
| Type                            |
|                                 |
| Some text inside the admonition |
+---------------------------------+

In an HTML context, previous directive would be expanded as:

#+BEGIN_HTML
<div class=\"admonition type\"><p class=\"header\">Type</p>
#+END_HTML
Some text inside the admonition
#+BEGIN_HTML
<div>
#+END_HTML

HTML fragments can be customized using both
`org-website-publish-admonition-header' and
`org-website-publish-admonition-footer'."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let (params admo substitute)
	(while (re-search-forward "^#\\+BEGIN_ADMONITION:?[ \t]+\\(.*\\)" nil t)
	  (setq params (read (concat "(" (match-string 1) ")"))
		admo (org-symname-or-string (pop params))
		substitute (plist-put substitute :admo (downcase admo))
		substitute (plist-put substitute :admo-title admo))
	  (beginning-of-line)
	  (insert
	   "#+BEGIN_HTML\n"
	   (string-template
	    org-website-publish-admonition-header
	    substitute)
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol))
	  (unless
	      (re-search-forward "^#\\+END_ADMONITION" nil t)
	    (error "#+END_ADMONITION not found in %s@%s." (buffer-file-name)
		   (point)))
	  (beginning-of-line)
	  (insert
	   "\n#+BEGIN_HTML\n"
	   (string-template
	    org-website-publish-admonition-footer
	    substitute)
	   "\n#+END_HTML\n")
	  (delete-region (point) (point-at-eol)))))))


(defun org-website-publish-copy ()
  "Copy files or directories to the publishing directory.

Syntax is:

#+COPY: SRC [DST]

Be aware this function is designed to be run from
`org-publish-before-export-hook'."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let (params src dst)
	(while (re-search-forward "^#\\+COPY:?[ \t]+\\(.*\\)" nil t)
	  (setq params (read (concat "(" (match-string 1) ")"))
		src (expand-file-name
		     (concat base-dir
			      file-dir
			     (org-symname-or-string (pop params))))
		dst (expand-file-name
		     (concat pub-dir
			     (file-name-as-directory
			      (or (org-symname-or-string (pop params))
				  ""))
			     (car (last (remove-if
					 '(lambda (x)
					    (string= "" x))
					 (split-string  src "/")))))))
	  (beginning-of-line)
	  (delete-region (point) (point-at-eol))
	  (if (file-directory-p src)
	      (copy-directory src dst t t)
	    (copy-file src dst t t t)))))))


(defun org-website-publish-include ()
  "Prepare html fragment for file inclusion based on both
`org-website-publish-include-header' and
`org-website-publish-include-footer' string templates.

File mode is automatically set if not defined on the #+INCLUDE
line.

File is also copied into the publishing directory"
  (let (params src-file src-file-fp substitute src mode)
    (save-match-data
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^#\\+INCLUDE:?[ \t]+\\(.*\\)" nil t)
	  (setq params (read (concat "(" (match-string 1) ")"))
		src-file (org-symname-or-string (pop params))
		src-file-fp (concat base-dir file-dir src-file)
		substitute (plist-put substitute :file src-file)
		;; src is the "src" keyword
		src (org-symname-or-string (pop params))
		mode (org-symname-or-string (pop params)))
	  ;; skip #+INCLUDE that are not used for file sourcing
	  (when (string= src "src")
	    (beginning-of-line)
	    (insert
	     (format
	      "#+BEGIN_HTML\n%s\n#+END_HTML\n"
	      (string-template org-website-publish-include-header substitute)))
	    (end-of-line)
	    ;; Add file mode to the include line if not present.
	    (unless mode
	      (insert
	       (format " \"%s\""
		       (substring (symbol-name
				   (with-temp-buffer
				     (insert-file-contents src-file-fp)
				     (set-auto-mode)
				     major-mode))
				  0 -5))))
	    (insert (format
		     "\n#+BEGIN_HTML\n%s\n#+END_HTML\n"
		     (string-template org-website-publish-include-footer
				      substitute)))
	    ;; Copy the file to the publishing-directory.
	    (make-directory pub-dir t)
	    (copy-file src-file-fp pub-dir t t)))))))


(defun org-website-publish-whats-new ()
  "Publish last entries for last change list.

Syntax is:

#+WHATSNEW: <count>"
  (let* ((what-s-new (with-temp-buffer
		       (insert-buffer (plist-get project-plist :rss-buffer))
		       (sort-lines t (point-min) (point-max))
		       (split-string (buffer-string) "\n")))
	 params count substitute)
    (save-match-data
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^#\\+WHATSNEW:?[ \t]+\\(.*\\)" nil t)
	  (setq params (read (concat "(" (match-string 1) ")"))
		count (or
		       (org-symname-or-string (pop params))
		       10))
	  (end-of-line)
	  (insert
	   "\n#+BEGIN_HTML\n"
	   org-website-what-s-new-header)
	  (loop for i upto count
		do (let ((item (split-string (nth i what-s-new) "\t")))
		     (insert
		      (string-template org-website-what-s-new-item
				       `(:date ,(car item)
					       :file ,(cadr item)
					       :title ,(caddr item)
					       :desc ,(cadddr item))))))
	  (insert org-website-what-s-new-footer "\n#+END_HTML\n"))))))


(defun org-website-publish-rss ()
  "Publish RSS file.

Syntax is:

#+RSS: <base_url> <file>"
  (let* ((rss (with-temp-buffer
		(insert-buffer (plist-get project-plist :rss-buffer))
		(sort-lines t (point-min) (point-max))
		(split-string (buffer-string) "\n")))
	 params base-url file)
    (save-match-data
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^#\\+RSS:?[ \t]+\\(.*\\)" nil t)
	  (setq params (read (concat "(" (match-string 1) ")"))
		base-url (org-symname-or-string (pop params))
		file (org-symname-or-string (pop params)))
	  (find-file (concat pub-dir "/" file))
	  (erase-buffer)
	  (insert
	   "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
	   "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">"
	   "<channel>"
	   "<title>"(org-website-get-file-property "title" nil t)"</title>"
	   "<link>" base-url "</link>"
	   "<generator>org-website (https://github.com/renard/org-website)</generator>"
	   "<description>"(org-website-get-file-property "description" nil t)
	   "</description>"
	   "<atom:link href=\"" base-url file
	   "\" rel=\"self\" type=\"application/rss+xml\"/>")
	  (loop for line in rss
		do (let ((item (split-string line "\t")))
		     (when (cadr item)
		       (insert
			"<item>"
			"<title>"(caddr item)"</title>"
			"<link>" base-url (cadr item) "</link>"
			"<description>"
			(replace-regexp-in-string
			 "</?.*?>"
			 ""
			 (cadddr item))
			"</description>"
			"<pubDate>"
			(format-time-string
			 "%a, %d %b %Y %T %z"
			 (apply 'encode-time
				(parse-time-string (car item))))
			"</pubDate>"
			"<guid>" base-url (cadr item) "</guid>"
			"</item>"))))
	  (insert
	   "</channel>"
	   "</rss>")
	  (replace-string "/<lisp>path-to-root</lisp>" "" nil (point-min)
			  (point-max))
	  (save-buffer)
	  (kill-buffer))))))


(defun org-website-get-file-property (prop &optional org-file remove-p-tag)
  "Return PROP property from ORG-FILE.

If ORG-FILE is a directory, then ORG-FILE/index.org would be used.

If REMOVE-P-TAG is set, the paragraph tag (<p>) would be stripped."
  (setq debug-on-error t)
  (unless org-file
    (setq org-file filename))
  (when (file-directory-p org-file)
    (setq page (concat org-file "index.org")))
  (with-temp-buffer
    (insert-file-contents org-file)
    (beginning-of-buffer)
    (save-match-data
      (when (search-forward-regexp (format
				    "^#\\+%s:?[ \t]+\\(.*\\)"
				    (upcase prop)) nil t)
	(let ((ret
	       (replace-regexp-in-string
		"\n" ""
		(substring-no-properties
		 (org-export-string (match-string 1) "html")))))
	  (when remove-p-tag
	    (setq ret (replace-regexp-in-string "</?p>" "" ret))
	    (setq ret (replace-regexp-in-string "</\\(div\\|body\\|html\\)>"
						"" ret)))
	  ret)))))


(defun org-website-publish-date (&optional lang format)
  "Return pushishing page date using FORMAT in LANG."
  (let* ((format (or format "%Y-%m-%dT%T%z"))
	 (system-time-locale (or lang "C")))
    (format-time-string
     format
     (date-to-time (org-website-get-file-property "date" nil t)) t)))

(defun org-website-sitetree-get-file-info (page)
  "Return an HTML formated string containing the PAGE title and description."
  ;; Use default page name
  (when (file-directory-p page)
    (setq page (concat page "index.org")))
  (when (file-exists-p page)
    (let ((file
	   (concat
	    (file-name-sans-extension (file-relative-name page base-dir))
	    ".html"))
	  (title (org-website-get-file-property "title" page t))
	  (desc (org-website-get-file-property "description" page t))
	  (date (org-website-get-file-property "date" page t)))
      ;; Generate list for what-new command
      (set-buffer (get-buffer-create (plist-get
				      project-plist :rss-buffer)))
      (insert (format "%s\t%s\t%s\t%s\n"
	      date file title desc))
      (set-buffer (get-buffer-create (plist-get
				      project-plist :sitetree-buffer)))
      (with-temp-buffer
	(insert desc)
	(save-match-data
	  (replace-string "<" "&lt;" nil (point-min) (point-max))
	  (replace-string ">" "&gt;" nil (point-min) (point-max)))
	(setq desc (buffer-string)))
      (format
       "<a href=\"<lisp>path-to-root</lisp>%s\" class=\"menu-tooltip\" title=\"%s\">%s</a>"
       file desc title))))

(defun org-website-generate-sitetree (dt)
  "Generate the sitetree from given DT directory tree."
  ;; Only lists are handled
  (when (stringp dt)
    (setq dt (list dt)))
  (let* ((dir (car dt))
	 (files
	  (remove-if '(lambda (x) (or (string= "index.org" x)
				      (string= "search.org" x)
				      (file-directory-p (concat dir x))))
		     (directory-files dir nil "\\.org$")))
	(subdir (cdr dt))
	(dirstr (org-website-sitetree-get-file-info dir)))
    (when dirstr
      (if (not (or subdir files))
	  ;; directory has no subdirs nor files.
	  (insert "<li class=\"leaf\">" dirstr "</li>\n")
	(insert "<li class=\"tree\">" dirstr "\n")
	(when subdir
	  (insert "<ul>\n")
	  ;; insert directories first
	  (mapcar '(lambda (x) (org-website-generate-sitetree x)) subdir)
	  (insert "</ul>\n"))
	;; insert files
	(when files
	  (insert "<ul>\n")
	  (mapcar
	   '(lambda (x)
	      (let ((dirstr (org-website-sitetree-get-file-info
			     (concat dir x))))
		(when dirstr
		  (insert
		   (format "<li class=\"leaf\">%s</li>\n" dirstr)))))
	   files)
	  (insert "</ul>\n"))
	;; Close first opened <li>
	(insert "</li>\n")))))

;;;###autoload
(defun org-website-publish-sitetree ()
  "Generate HTML template that could be used later in HTML pages.
This function is intended to replace `org-publish-org-sitetree' for websites."
  ;; Variables defined in `org-publish-projects':
  ;;   - `project-plist'
  ;;
  (let* ((base-dir (file-name-as-directory
		    (plist-get project-plist :base-directory)))
	 (pub-dir (file-name-as-directory
		   (plist-get project-plist :publishing-directory)))
	 (build-buffer (get-buffer-create (plist-get project-plist :sitetree-buffer))))
    (set-buffer build-buffer)
    (erase-buffer)
    (html-mode)
    (insert "<ul>\n")
    (org-website-generate-sitetree (directory-tree base-dir))
    (insert "</ul>\n")
    (indent-region (point-min) (point-max))))


(defun org-website-menu (&optional sitetree-file)
  "Insert the generated menu inside the HTML page."
  (with-temp-buffer
    (insert-buffer-substring-no-properties (plist-get
					    project-plist :sitetree-buffer))
    (beginning-of-buffer)
    (save-match-data
      (while
	  (search-forward-regexp
	   "^[ \t]*\\(<li class=\"\\)\\([^\"]\+\\)\\(\".*</lisp>\\)\\([^\"]\+\\)\\(\".*\\)" nil t)
	(let ((dummy1 (match-string 1))
	      (class (match-string 2))
	      (dummy2 (match-string 3))
	      (dir (match-string 4))
	      (dummy3 (match-string 5)))

	  (unless
	      (string= ".."
		       (car (split-string
			     (file-relative-name
			      pub-file-rel
			      (file-name-directory dir)) "/")))
	    (setq class (concat class " selected")))
	  (when
	      (string= dir pub-file-rel)
	    (setq class (concat class " current")))
	  (delete-region (point-at-bol) (point-at-eol))
	  (insert dummy1 class dummy2 dir dummy3 "\n"))))
    (buffer-string)))


(defun org-website-publish-add-header-and-footer ()
  "Add both header and footer to the page source file.

Both `:website-header' and `:website-footer' properties are used within the
project definition. These are both org files."
  (let* ((header (plist-get project-plist :website-header))
	 (footer (plist-get project-plist :website-footer)))
    (save-excursion
      (when (and header
		 (file-exists-p header))
	(beginning-of-buffer)
	(insert-file-contents header))
      (when (and footer
		 (file-exists-p footer))
	(end-of-buffer)
	(insert "\n")
	(insert-file-contents footer)))))


(defun org-publish-prepare-current-file()
  "Prepare the current published file before processing by:

- adding the footer and header
- processing included files
- processing admonitions

The `org-publish-before-export-hook' is modified."
  (org-website-publish-with-vars
   (org-website-publish-add-header-and-footer)
   (org-website-publish-include)
   (org-website-publish-admonition)
   (org-website-publish-copy)
   (org-website-publish-whats-new)
   (org-website-publish-rss)))
(add-hook 'org-publish-before-export-hook 'org-publish-prepare-current-file)


(defun org-website-eval-lisp()
  "Eval embeded lisp code in published page. This function should be called
using `org-publish-after-export-hook'.

Some variables might be useful in embeded lisp code:

See `org-website-publish-with-vars' for further information about
implicit variables."
  (save-excursion
    (save-restriction
      (save-match-data
	;; needed for thing-at-point
	(html-mode)
	(beginning-of-buffer)
	(let ((open-tag "<lisp>")
	      (close-tag "</lisp>")
	      beg end sexp)
	  (while (search-forward open-tag nil t)
	    (setq beg (- (point) (length open-tag)))
	    (when (search-forward close-tag nil t)
	      (setq end (point))
	      (backward-char (1+ (length close-tag)))
	      (setq sexp (substring-no-properties (thing-at-point 'sexp)))
	      (delete-region beg end)
	      (insert
	       (save-match-data
		 (condition-case err
		     (let ((object (eval (read sexp))))
		       (cond
			;; result is a string
			((stringp object) object)
			;; a list
			((and (listp object)
			      (not (eq object nil)))
			 (let ((string (pp-to-string object)))
			   (substring string 0 (1- (length string)))))
			;; a number
			((numberp object)
			 (number-to-string object))
			;; nil
			((eq object nil) "")
			;; otherwise
			(t (pp-to-string object))))
		   ;; error handler
		   (error
		    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
	      (goto-char beg))))))))


(defun org-website-fix-img-tag ()
  "Fix <img> HTML tag by adding an \"alt\" attribute."
  (save-excursion
    (save-match-data
      (replace-regexp 
       "<img src=\"\\([^\"]+\\)\"/>"
       "<img src=\"\\1\" alt=\"\\1\"/>" nil (point-min) (point-max) ))))


(defun org-publish-finish-current-file()
  "Prepare the current published file after processing by:

- evaluating embeded lisp code

The `org-publish-after-export-hook' is modified."
  (org-website-publish-with-vars
   (org-website-eval-lisp)
   (org-website-fix-img-tag)))

(add-hook 'org-publish-after-export-hook 'org-publish-finish-current-file)


(defun org-website-publish-run-processes-sentinel (proc change)
  "Sentinel in charge of running next process if previous one succeded."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cmd (process-get proc :cmd))
	  (cmd-buf (process-get proc :cmd-buf))
	  (dir (process-get proc :dir))
	  (next (process-get proc :next)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) cmd-buf))
	    (error "Org website ERROR: %s" cmd))
	(message  "Org website OK: %s" cmd))
      (when cmd-buf (kill-buffer cmd-buf))
      (when next
	(org-website-publish-run-processes next dir)))))

(defun org-website-publish-run-processes (processes dir)
  "Sequentially run PROCESSES in DIR."
  (when processes
    (let* ((cmd-line (car processes))
	   (cmd (car cmd-line))
	   (args (cdr cmd-line))
	   (cmd-buf (get-buffer-create (format "ORG Website running %s"
					       cmd-line)))
	   (default-directory dir)
	   (proc (apply 'start-process cmd cmd-buf cmd args)))
      (process-put proc :next (cdr processes))
      (process-put proc :cmd (format "%s" cmd-line))
      (process-put proc :cmd-buf cmd-buf)
      (process-put proc :dir dir)
      (set-process-sentinel proc 'org-website-publish-run-processes-sentinel))))

(defun org-website-publish-run-all-processes ()
  (org-website-publish-run-processes
   (plist-get project-plist :project-shell-final-commands)
   (plist-get project-plist :project-root))
  (kill-buffer (plist-get project-plist :sitetree-buffer))
  (kill-buffer (plist-get project-plist :rss-buffer)))


(defadvice org-html-make-link (after ows:org-html-make-link activate)
  "Add title element to the html <a> tag."
  (with-temp-buffer
    (insert ad-return-value)
    (goto-char (point-min))
    (save-match-data
      (when (search-forward-regexp "href=\"\\([^\"]+\\)\"" nil t)
	(insert (format " title=\"%s\" class=\"link-tooltip\""
			(match-string-no-properties 1)))))
    (setq ad-return-value (buffer-string))))

;;;###autoload
(defun org-website-publish-async (project &optional force)
  "Publish website in async mode.

A `:website-async-opts' property could be set for a projects in
`org-publish-project-alist'. For example:

  :website-async-opts (\"-l\"
		       ,(concat (file-name-as-directory
				 user-emacs-directory)
				\"init.el\")
		       \"--eval\" \"(require (quote color-theme))\"
		       \"--eval\" \"(require (quote color-theme-tango))\")

"
  (interactive
   (list
    (car (assoc (org-icompleting-read
		 "Publish project: "
		 org-website-alist nil t)
		org-website-alist))
    current-prefix-arg))
  (let* ((force (if force "t" "nil"))
	 (cmd-line
	  (append command-line-args
		  `("-batch"
		    ,@org-website-async-options
		    "--eval"
		    ,(format "(org-publish-website \"%s\" %s)" project force))))
	 (cmd-buf (get-buffer-create (format "ORG Website async build %s" project)))
	 (proc (apply 'start-process (car cmd-line)
		      cmd-buf (car cmd-line) (cdr cmd-line))))
    (message "Run: %S" cmd-line)
    (process-put proc :cmd (format "Build %s" project))
    (process-put proc :cmd-buf cmd-buf)
    (set-process-sentinel proc 'org-website-publish-run-processes-sentinel)))

;;;###autoload
(defun org-publish-website (website &optional force)
  "Generate `org-publish-project-alist' according WEBSITE."

  (interactive
   (list
    (car (assoc (org-icompleting-read
		 "Publish project: "
		 org-website-alist nil t)
		org-website-alist))
    current-prefix-arg))

  (let* ((filename (cadr (assoc website org-website-alist)))
	 (project
	  (with-temp-buffer
	    (insert-file-contents-literally filename)
	    (read (current-buffer))))
	 org-publish-project-alist)

    ;; :base-directory
    (mapcar '(lambda (x)
	       (let ((f (concat
			 (file-name-directory filename)
			 (plist-get project x))))
		 (setq project (plist-put project x f))))
	    '(:base-directory :website-header :website-footer))

    ;; projet-root
    (setq project
	  (plist-put project :project-root
		     (file-name-directory
		      (plist-get project :base-directory))))

    ;; :publishing-directory
    (let ((v (plist-get project :publishing-directory)))
      (unless (and v (file-name-absolute-p v))
	(setq project
	      (plist-put project :publishing-directory
			 (file-name-as-directory
			  (concat (file-name-directory filename) v))))))

    (unless (plist-get project :rss-buffer)
      (setq project (plist-put project :rss-buffer "*Org website rss*")))

    (unless (plist-get project :sitetree-buffer)
      (setq project (plist-put project :sitetree-buffer "*Org website sitetree*")))


    (mapcar '(lambda(x)
	       (let* ((v (plist-get project x)))

		 (when (and v (not (file-name-absolute-p v)))
		   (setq v (concat (file-name-directory filename) v)))

		 (when (file-readable-p v)
		   (setq project
			 (plist-put
			  project x
			  (with-temp-buffer
			    (insert-file-contents v)
			    (buffer-string)))))))
	    '(:html-preamble :html-postamble))

  (setq org-publish-project-alist
	(list (cons website project)))
  (org-publish website force)))

(provide 'org-website)
