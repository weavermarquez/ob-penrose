;;; ob-penrose.el --- org-babel functions for penrose evaluation

;; Copyright (C) 2023 Valerie Kim

;; Author: Valerie Kim
;; Keywords: declarative diagram, literate programming, reproducible research
;; Homepage: https://github.com/weavermarquez/ob-penrose
;; Version: 0.01

;;; License: see LICENSE.txt

;;; Commentary:

;; Org-Babel support for evaluating penrose diagrams.

;; Finally you can use `edebug' to instrumentalize
;; `org-babel-expand-body:penrose' and continue to evaluate the code block. You
;; try to add header keywords and change the body of the code block and
;; reevaluate the code block to observe how things get handled.

;;; Requirements:

;; roger cli | https://penrose.cs.cmu.edu/docs/ref/api
;; penrose syntax highlights | https://github.com/weavermarquez/penrose-mode
;; penrose syntax highlights | https://github.com/penrose/penrose

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint) ;; Session based evaluation
(require 'ob-eval) ;; Temp File based evaluation
;; (require 'penrose-modes)

(defvar org-babel-default-header-args:penrose
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a penrose source block.")

(defcustom ob-penrose-cli-path nil
  "Path to penrose.cli executable."
  :group 'org-babel
  :type 'string)

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:penrose' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.

(defun org-babel-expand-body:penrose (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-penrose nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-penrose-var-to-penrose (cdr pair))))
      vars "\n")
     "\n" body "\n")))

(defun org-babel-execute:penrose (body params)
  "Execute a block of Penrose code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Penrose source code block")
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "penrose requires a \":file\" header argument")))
         (source-style-file (cdr (assoc :style params)))
         (source-domain-file (cdr (assoc :domain params)))
         (variation (cdr (assoc :variation params)))
         (in-style-file (org-babel-temp-file "penrose-style-" ".style"))
         (in-domain-file (org-babel-temp-file "penrose-domain-" ".domain"))
         (in-substance-file (org-babel-temp-file "penrose-substance-" ".substance"))
         (roger (or ob-penrose-cli-path
                   (executable-find "roger")
                   (error "`ob-penrose-cli-path' is not set and roger is not in `exec-path'")))
         (cmd (concat (shell-quote-argument (expand-file-name roger))
                      " trio --trio " (org-babel-process-file-name in-substance-file)
                      " " (org-babel-process-file-name in-style-file)
                      " " (org-babel-process-file-name in-domain-file)
                      " -o " (org-babel-process-file-name out-file)
                      " -v " variation)))
    (unless (file-executable-p roger)
      ;; cannot happen with `executable-find', so we complain about
      ;; `ob-penrose-cli-path'
      (error "Cannot find or execute %s, please check `ob-penrose-cli-path'" roger))
    (with-temp-file in-style-file (insert (org-babel-eval-read-file source-style-file)))
    (with-temp-file in-domain-file (insert (org-babel-eval-read-file source-domain-file)))
    (with-temp-file in-substance-file (insert body))
    (message "%s" cmd)
    (org-babel-eval cmd "")
    nil))

(provide 'ob-penrose)
;;; ob-penrose.el ends here
