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

;; dv is an Emacs package for visualizing ELisp files and required packages.
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

;;;; Functions and Emacs user commands

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

(defun dv--package-update-graph (graph seen pkg)
  "Fill GRAPH, which is a hash table, with a PKG metadata and its dependencies.
The packages metadatas are retrieved thanks to the
`dv--get-package-desc' function.

Each GRAPH entry will have the following composition below.  In this
context, the keys will always be symbols identifying packages, like
`'yaml'.  And the values will be a cons with a `package-desc' object as
CAR and a list of symbol as CDR.  We could represent like this,
(key: \='pkg-id, value: (metadata . \='(c1 c2 ...))).

It returns GRAPH."
  (when pkg
    (unless (gethash pkg seen)
      (puthash pkg t seen)
      (let* ((desc (dv--get-package-desc pkg))
	     (children nil))
	;; Process PKG reqs
	(dolist (req (package-desc-reqs desc))
	  (let ((child (car req)))
	    (setq children (append children (list child)))
	    (dv--package-update-graph graph seen child)))
	;; Update GRAPH with the computed PKG metadatas
	(puthash pkg (cons desc children) graph)))
    graph))

(defun dv-package-create-graph (&rest pkgs)
  "It creates and returns a new hash table representing PKGS dependencies
graph.

See `dv--package-update-graph' for more details."
  (let ((graph (make-hash-table :test 'equal))
	(seen (make-hash-table :test 'equal)))
    (dolist (pkg pkgs)
      (dv--package-update-graph graph seen pkg))
    graph))

(defun dv--get-package-desc-node-label (desc)
  "Returns a node label for a `package-desc' object."
  (let ((name (symbol-name (package-desc-name desc)))
	(version (string-join
		  (mapcar (lambda (x) (format "%s" x)) (package-desc-version desc))
		  ".")))
    (concat name "-" version)))

(defun dv--get-filepath-node-label (filepath)
  "Returns a node label for a FILEPATH."
  filepath)

(defun dv--get-node-label (metadata)
  "Returns a node label depending of the METADATA object type."
  (cond ((stringp metadata) (dv--get-filepath-node-label metadata))
	((package-desc-p metadata) (dv--get-package-desc-node-label metadata))
	(t (error "Invalid node metadata type %s" (type-of metadata)))))

(defun dv--create-dot-code-links (graph seen pkg)
  "Returns a string representing dot code dependencies relations for a
directed GRAPH. CELL represents a GRAPH value containing the needed
metadatas to traverse the GRAPH."
  (unless (gethash pkg seen)
    (puthash pkg t seen)
    (let* ((dot-code nil)
	   (cell (gethash pkg graph))
	   (metadata (car cell))
	   (children (cdr cell))
	   (label (dv--get-node-label metadata)))
      (dolist (child children)
	(let* ((child-cell (gethash child graph))
	       (child-metadata (car child-cell))
	       (child-label (dv--get-node-label child-metadata))
	       (dot-code-links (concat "\"" label "\" -> \"" child-label "\""))
	       (dot-code-links-child (dv--create-dot-code-links graph seen child)))
	  (setq dot-code (append dot-code (list dot-code-links)))
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
  (let ((values (hash-table-values graph))
	(f (lambda (value)
	     (concat "\"" (dv--get-node-label (car value)) "\""))))
    (string-join (mapcar f values) "\n")))

(defun dv-create-dot-code (graph &optional dest-file &rest keys)
  "It returns a string representing a directed GRAPH in the dot language
with. KEYS are the root nodes that will be traversed in depth. DEST-FILE
is an optional parameter, if non-nil it should specify a filepath with
the produced dot code will be written."
  (let ((dot-code (string-join
		   (list
		    "digraph {"
		    ;; Preventing a scenario where there are only orphans nodes,
		    ;; resulting into a blank graph
		    ;;
		    ;; So we first declare every dependency as a header.
		    (dv--create-dot-code-var-declarations graph)
		    ;; Then we compute the nodes links
		    (apply
		     #'dv--create-dot-code-links-from-keys
		     graph (make-hash-table :test 'equal) keys)
		    "}")
		   "\n")))
    (when dest-file
      (with-temp-file dest-file
	(insert dot-code)))
    dot-code))

(defun dv-package (dest-file &optional open-file &rest pkgs)
  "This function produces dot code representing PKG dependencies. This dot
code is passed to the dot program that will interpret it, then create an
image written at the DEST-FILE filepath. If OPEN-FILE is non-nil, the
produced image will be open by Emacs when possible."
  (let* ((graph (apply #'dv-package-create-graph pkgs))
	 (dot-code (apply #'dv-create-dot-code graph nil pkgs))
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

(provide 'dv)

;;; dv.el ends here
