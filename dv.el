;;; dv.el --- Emacs package dependency visualizer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Théo Bori

;; Author: Théo Bori <theobori@disroot.org>
;; Maintainer: Théo Bori <theobori@disroot.org>
;; Keywords: tools
;; URL: https://github.com/theobori/dv
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; dv is an Emacs package for visualizing dependent ELisp files and required packages.
;;
;; This package is able to produce an image representing a graph
;; of dependencies using Graphviz.

;;; Code

(require 'package)

(defgroup dv ()
  "Emacs package dependency visualizer"
  :group 'tools
  :group 'external
  :link "https://github.com/theobori/dv")

;;;; User options

(defcustom dv-dot-executable (executable-find "dot")
  "Dot executable."
  :type 'string
  :group 'dv)

;;;; Constants

(defconst dv-type-package 'package
  "Graph node type for package")

(defconst dv-type-path 'path
  "Graph node type for path")

(defconst dv-require-regexp "(\\s-*require\\s-*'\\(?1:[a-z]*\\)\\s-*"
  "Regular expression for capturing `require' package name")

(defconst dv-use-package-regexp "(use-package\\s-*\\(?1:[a-z-]+\\)"
  "Regular expression for capturing `use-package' package name")

(defconst dv-load-regexp "(load\\s-*\"\\(?1:.*\\)\""
  "Regular expression for capturing `load' file path")

(defconst dv-load-file-regexp "(load-file\\s-*\"\\(?1:.*\\)\"\\s-*)"
  "Regular expression for capturing `load-file' file path")

(defconst dv-package-match-pairs (list
				  (cons dv-require-regexp 1)
				  (cons dv-use-package-regexp 1))
  "List of cons, regular expressions that capture package names associated
with the amount of regexp groups for each")

(defconst dv-path-match-pairs (list
			       (cons dv-load-file-regexp 1)
			       (cons dv-load-regexp 1))
  "List of cons, regular expressions that capture package names associated
with the amount of regexp groups for each")

;;;; Structures

(cl-defstruct dv-graph-key "Representing a graph key"
	      (type dv-type-package
		    :read-only t
		    :type symbol
		    :documentation "Indicating the node type")
	      (label nil
		     :read-only t
		     :type string
		     :documentation "Representing the node label that will be used for producing the dot code"))

(cl-defstruct dv-graph-value "Representing a graph value"
	      (metadata nil
			:read-only t
			:type any
			:documentation "Representing the node metadata")
	      (children nil
			:read-only nil
			:type list
			:documentation "Containing the node children"))

;;;; Functions and Emacs user commands

(defun dv--get-string-from-file (path)
  "Return file PATH content as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun dv--match-strings-all-groups (regexp string groups-amount)
  "Returns a list of string where each string correspond to a REGEXP group
that matched text in STRING. GROUPS-AMOUNT is the amount of group within
REGEXP, each of them should be go from 1 to GROUPS-AMOUNT incrementing
by 1 each time."
  (save-match-data
    (let ((index 0)
	  (cycle-table (make-hash-table :test #'equal))
	  (matches))
      (while (and
	      (string-match regexp string index)
	      (null (gethash index cycle-table)))
	(puthash index t cycle-table)
	(setq index (match-end 0))
	(let ((amount groups-amount))
	  (while (>= amount 1)
	    (let ((match (match-string-no-properties amount string)))
	      (when match
		(setq matches (cons match matches))))
	    (setq amount (- amount 1)))))
      matches)))

(defun dv--match-pairs (pairs string)
  "Returns a list of every pair, representing element of PAIRS, that
matched string."
  (let ((matches))
    (dolist (pair pairs)
      (let* ((regexp (car pair))
	     (groups-amount (cdr pair))
	     (other-matches (dv--match-strings-all-groups regexp string groups-amount)))
	(setq matches (append matches other-matches))))
    matches))

(defun dv--match-package-pairs (string)
  "Return packages name matched in STRING."
  (dv--match-pairs dv-package-match-pairs string))

(defun dv--match-path-pairs (string)
  "Return paths name matched in STRING."
  (dv--match-pairs dv-path-match-pairs string))

(defun dv--unique (seq)
  "Returns a copy of SEQ without duplicates. Wrapping
`cl-delete-duplicates'."
  (cl-remove-duplicates seq :test #'equal))

(defun dv-run-dot (dot-code sentinel &rest args)
  "It will create a dot subprocess by using `make-process', passing the
DOT-CODE code as standard input and using ARGS as CLI arguments. The
SENTINEL argument is supposed to be a function passed to the `:sentinel'
`make-process' property."
  (let ((process (make-process :name dv-dot-executable
                               :buffer nil
                               :command (append (list dv-dot-executable) args)
                               :connection-type 'pipe
			       :sentinel sentinel)))
    (process-send-string process dot-code)
    (process-send-eof process)))

(defmacro dv--make-create-graph-func (name process-element-func)
  `(defun ,name (&rest seq)
     (let ((graph (make-hash-table :test #'equal))
	   (seen (make-hash-table :test #'equal)))
       (dolist (element seq)
	 (,process-element-func graph seen element))
       graph)))

(defun dv--get-package-desc (pkg)
  "Returns `package-desc' object is PKG is available. Otherwise it returns
nil."
  (or
   (if (package-desc-p pkg) pkg)
   (cadr (assq pkg package-alist))
   (let ((built-in (assq pkg package--builtins)))
     (if built-in
         (package--from-builtin built-in)
       (cadr (assq pkg package-archive-contents))))))

(defun dv--get-package-graph-key (pkg)
  "Returns a `dv-graph-key' with PKG which can be a string or a symbol."
  (make-dv-graph-key
   :type dv-type-package
   :label (cond ((symbolp pkg) (symbol-name pkg))
		((stringp pkg) pkg)
		(t (error "Invalid label type %s" (type-of pkg))))))

(defun dv--package-update-graph (graph seen pkg)
  "Fill GRAPH, which is a hash table, with a PKG metadata and its
dependencies. The packages metadatas are retrieved thanks to the
`dv--get-package-desc' function. It returns GRAPH."
  (when pkg
    (let ((key (dv--get-package-graph-key pkg)))
      (unless (gethash key seen)
	(puthash key t seen)
	;; Get PKG metadata
	(let ((desc (dv--get-package-desc pkg))
	      (children))
	  ;; Process PKG reqs
	  (dolist (req (package-desc-reqs desc))
	    (let* ((child (car req))
		   (child-key (dv--get-package-graph-key child)))
	      (setq children (cons child-key children))
	      (dv--package-update-graph graph seen child)))
	  ;; Update GRAPH with the computed PKG metadata
	  (puthash key (make-dv-graph-value
			:metadata desc
			:children children) graph)))
      graph)))

(dv--make-create-graph-func dv-package-create-graph
			    dv--package-update-graph)

(defun dv--get-package-desc-node-label (desc)
  "Returns a node label for a `package-desc' object."
  (let ((name (symbol-name (package-desc-name desc)))
	(version (string-join
		  (mapcar (lambda (x) (format "%s" x)) (package-desc-version desc))
		  ".")))
    (concat name "-" version)))

(defun dv--path-update-graph (graph seen path &optional base-dir)
  "Fill GRAPH, which is a hash table depending of the text found in the
content read from PATH. It returns GRAPH.

So, PATH will be read, then parsed to extract dependencies. Basically,
we are looking for ELisp expression with keywords like `require',
`use-package', `load-path', etc.."
  ;; If there is no BASE-DIR, we use the current directory as BASE-DIR
  (unless base-dir
    (setq base-dir (file-name-directory (file-truename path))))
  (let* ((absolute-path (if (file-name-absolute-p path)
			    path
			  ;; Concat BASE-DIR and PATH if PATH is relative
			  (file-truename (file-name-concat base-dir path))))
	 (key (make-dv-graph-key
	       :type dv-type-path
	       :label absolute-path)))
    (unless (gethash key seen)
      (puthash key t seen)
      (let* ((text (dv--get-string-from-file absolute-path))
	     (child-packages (dv--unique (dv--match-package-pairs text)))
	     (child-paths (dv--unique (dv--match-path-pairs text)))
	     (children))
	;; Process package dependencies
	(dolist (child-package child-packages)
	  (let* ((package-symbol (intern child-package))
		 (package-key (dv--get-package-graph-key package-symbol)))
	    (setq children (cons package-key children))
	    ;; Visit child package
	    (dv--package-update-graph graph seen package-symbol)))
	;; Process path dependencies
	(dolist (child-path child-paths)
	  (let* ((child-absolute-path (if (file-name-absolute-p child-path)
					  child-path
					(file-truename (file-name-concat base-dir child-path))))
		 (path-key (make-dv-graph-key
			    :type 'path
			    :label child-absolute-path)))
	    (setq children (cons path-key children))
	    ;; Visit child path
	    (dv--path-update-graph
	     graph seen child-absolute-path
	     (file-name-directory child-absolute-path))))
	;; Update the graph
	(puthash key (make-dv-graph-value
		      :metadata absolute-path
		      :children children) graph))))
  graph)

(dv--make-create-graph-func dv-path-create-graph
			    dv--path-update-graph)

(defun dv--get-path-node-label (path)
  "Returns a node label for a PATH."
  path)

(defun dv--get-node-label (metadata)
  "Returns a node label depending of the METADATA object type."
  (cond ((stringp metadata) (dv--get-path-node-label metadata))
	((package-desc-p metadata) (dv--get-package-desc-node-label metadata))
	(t (error "Invalid node metadata type %s" (type-of metadata)))))

(defun dv--create-dot-code-links (graph seen key)
  "Returns a string representing dot code dependencies relations for a
directed GRAPH. CELL represents a GRAPH value containing the needed
metadatas to traverse the GRAPH."
  (unless (gethash key seen)
    (puthash key t seen)
    (let* ((dot-code)
	   (value (gethash key graph))
	   (children (dv-graph-value-children value))
	   (label (dv-graph-key-label key)))
      (dolist (child children)
	(let* ((child-label (dv-graph-key-label child))
	       (dot-code-links (concat "\"" label "\" -> \"" child-label "\""))
	       ;; Process CHILD node links
	       (dot-code-links-child (dv--create-dot-code-links graph seen child)))
	  ;; Append the current node link with CHILD node
	  (setq dot-code (append dot-code (list dot-code-links)))
	  ;; Only append CHILD node links if it has children
	  (when (> (length dot-code-links-child) 0)
	    (setq dot-code (append dot-code (list dot-code-links-child))))))
      (string-join dot-code "\n"))))

(defun dv--create-dot-code-links-from-keys (graph seen &rest keys)
  "Helper function that produce dot code relations with GRAPH KEYS as input."
  (string-join
   (mapcar (lambda (key) (dv--create-dot-code-links graph seen key)) keys)
   "\n"))

(defun dv--create-dot-code-var-declarations (graph)
  "Helper function that produce node declarations for every node of the
GRAPH. It's mainly for prenventing orphan nodes."
  (let ((keys (hash-table-keys graph))
	(f (lambda (key)
	     (concat "\"" (dv-graph-key-label key)  "\""))))
    (string-join (mapcar f keys) "\n")))

(defun dv-create-dot-code (graph &optional dest-file &rest keys)
  "It returns a string representing a directed GRAPH in the dot language
with. KEYS are the root nodes that will be traversed in depth. DEST-FILE
is an optional parameter, if non-nil it should specify a path with
the produced dot code will be written."
  (let ((dot-code (string-join
		   (list
		    "digraph {"
		    ;; Preventing a scenario where there are only orphans nodes,
		    ;; resulting into a blank graph
		    ;;
		    ;; So we first declare every dependency as a header
		    (dv--create-dot-code-var-declarations graph)
		    ;; Then we compute the nodes links
		    (apply
		     #'dv--create-dot-code-links-from-keys
		     graph (make-hash-table :test #'equal) keys)
		    "}")
		   "\n")))
    (when dest-file
      (with-temp-file dest-file
	(insert dot-code)))
    dot-code))

(defun dv--process (graph keys dest-file &optional open-file)
  "It produces dot code representing KEYS dependencies. This dot
code is passed to the dot program that will interpret it, then create an
image written at the DEST-FILE path. If OPEN-FILE is non-nil, the
produced image will be open by Emacs when possible."
  (let* ((dot-code (apply #'dv-create-dot-code graph nil keys))
	 (file-format (file-name-extension dest-file))
	 (absolute-path (file-truename dest-file))
	 ;; Sentinel function evaluated after the dot process execution.
	 ;; It has access to OPEN-FILE and DEST-FILE.
	 (sentinel (lambda (proc _)
		     (let ((status (process-exit-status proc)))
		       (unless (equal status 0)
			 (error "%s failed with status code %s" proc status)))
		     (when (and (display-graphic-p) open-file)
		       (find-file absolute-path)))))
    (dv-run-dot dot-code sentinel "-T" file-format "-o" dest-file)
    (message "The file %s has been created" absolute-path)))

(defmacro dv--make-entry-point-func (name create-graph-func entity-to-key-func)
  `(defun ,name (dest-file &optional open-file &rest seq)
     "It generates an image file representing dependencies. See
`dv--process' for more details."
     (dv--process (apply ,create-graph-func seq)
		  (mapcar ,entity-to-key-func seq)
		  dest-file open-file)))

(dv--make-entry-point-func dv-package
			   #'dv-package-create-graph
			   (lambda (pkg) (dv--get-package-graph-key pkg)))

(dv--make-entry-point-func dv-path
			   #'dv-path-create-graph
			   (lambda (path)
			     (make-dv-graph-key :type dv-type-path :label (file-truename path))))

(provide 'dv)

;;; dv.el ends here
