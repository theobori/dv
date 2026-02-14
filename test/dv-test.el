;;; dv-test.el --- Units tests for dv  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Théo Bori

;; Author: Théo Bori <theobori@disroot.org>
;; Maintainer: Théo Bori <theobori@disroot.org>
;; Keywords: tools
;; URL: https://github.com/theobori/dv

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

;; Units tests for dv

;;; Code

(require 'dv)
(require 'ert)
(require 'package)

;;;; Constants

(defconst dv-test-text "(require 'package)
(use-package magit)
(use-package magit)
(use-package magit)

(require 'peg)
(require 'peg)
(require 'peg)

(load \"./example-files/a.el\")
(load-file \"./example-files/a.el\")"
  "Test text used only for tests")

;;;; Units tests for dv

(ert-deftest test-dv--match-strings-all-groups ()
  "Test that `dv--match-strings-all-groups' works correctly."
  (should (equal
	   (dv--match-strings-all-groups "hel\\(?1:lo\\)" "abc hello" 1)
	   '("lo")))
  (should (equal
	   (dv--match-strings-all-groups "a\\(?2:bc\\) hel\\(?1:lo\\)" "abc hello abc hellolo" 2)
	   '("lo" "bc" "lo" "bc"))))

(ert-deftest test-dv--match-pairs ()
  "Test that the functions built on top of `dv--match-pairs' works
correctly."
  (should (equal
	   (dv--match-package-pairs dv-test-text)
	   '("peg" "peg" "peg" "package" "magit" "magit" "magit")))
  (should (equal
	   (dv--match-filepath-pairs dv-test-text)
	   '("./example-files/a.el" "./example-files/a.el"))))

(ert-deftest test-dv--unique ()
  "Test that the functions `dv--unique' works correctly."
  (should (equal
	   (dv--unique '(1 1 2 3 3 3 3 4))
	   '(1 2 3 4)))
  (should (equal
	   (dv--unique '(1 2 3 4))
	   '(1 2 3 4))))

(ert-deftest test-dv-get-package-desc ()
  "Test that the functions `dv-get-package-desc' works correctly."
  (should (package-desc-p (dv-get-package-desc 'yaml)))
  (should-error (dv-get-package-desc 'unknown-package)))

(ert-deftest test-dv--get-absolute-filepath ()
  "Test that the functions `dv--get-absolute-filepath' works correctly."
  (should (equal
	   (dv--get-absolute-filepath "/tmp/test.el")
	   "/tmp/test.el"))
  (should (equal
	   (dv--get-absolute-filepath "./tmp/test.el")
	   (file-name-concat (file-truename ".") "tmp/test.el")))
  (should (equal
	   (dv--get-absolute-filepath "./tmp/test.el" "basedir")
	   (file-name-concat (file-truename ".") "basedir/tmp/test.el"))))

(ert-deftest test-dv-run-dot ()
  "Test that the functions `dv-run-dot' works correctly."
  (should (dv-run-dot "" (lambda (proc _)
			   (unless (equal (process-exit-status proc) 0)
			     (error "Fail")))
		      "--help")))

(ert-deftest test-dv-package-create-graph ()
  "Test that the functions `dv-package-create-graph' works correctly. This
test should be run via the GNU Makefile"

  (let ((expected '(("magit" . ("with-editor" "transient" "seq"
				"magit-section" "llama" "cond-let"
				"compat" "emacs"))
		    ;; ("transient" . ("seq" "compat" "emacs"))
		    ("transient" . nil)
		    ("with-editor" . ("compat" "emacs"))
		    ("llama" . ("compat" "emacs"))
		    ("magit-section" . ("seq" "llama" "cond-let" "compat" "emacs"))
		    ("cond-let" . ("emacs"))
		    ("emacs" . nil)
		    ;; ("compat" . ("seq" "emacs"))
		    ("compat" . nil)
		    ("seq" . nil)))
	(graph (dv-package-create-graph 'magit)))
    (dolist (pair expected)
      (let* ((key (make-dv-graph-key :type dv-type-package :label (car pair)))
	     (expected-children (mapcar
				 (lambda (name) (make-dv-graph-key :type dv-type-package :label name))
				 (cdr pair)))
	     (graph-children (dv-graph-value-children (gethash key graph))))
	(should (equal expected-children graph-children))))))

;; TODO: write a test for `dv-package-create-graph'

(ert-deftest test-dv-package ()
  "Test that the functions `dv-package' works correctly. Verifying there
are no errors during the function execution."
  (let ((inputs '(("test-magit" . (magit))
		  ("test-magit-js-teco" . (magit js teco))
		  ("test-yaml" . (yaml))))
	(extensions '("png" "svg" "jpg" "jpeg")))
    (dolist (input inputs)
      (let ((dest-file-base (car input))
	    (pkgs (cdr input)))
	(dolist (extension extensions)
	  (let ((dest-file (concat dest-file-base "." extension)))
	    (should (apply #'dv-package dest-file nil pkgs))))))))

(ert-deftest test-dv-filepath ()
  "Test that the functions `dv-filepath' works correctly. Verifying there
are no errors during the function execution. This test should be run via
the GNU Makefile"
  (should (dv-filepath "unique-test.png" nil "./test/example-files/a.el")))

(provide 'dv-test)

;;; dv-test.el ends here
