;;; company-issue.el --- company-mode completion back-end for issues -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/company-issue
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (magit "2.8.0") (company "0.9.2"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; company-mode completion back-end for issues.
;;
;; Setup:
;;
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends 'company-issue))
;;
;; You need to execute `company-issue-setup' inside the repository to
;; configure the issues source and cache them.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'company)
(require 'magit-git)
(require 'url-parse)

(defgroup company-issue nil
  "company-mode completion back-end for issues"
  :prefix "company-issue-"
  :group 'applications)

(defvar company-issue-project (make-hash-table :test #'equal))

(defvar-local company-issue-root nil)
(defvar-local company-issue-host nil)
(defvar-local company-issue-slug nil)

(defun company-issue-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol (intern string-or-symbol)))

(defun company-issue-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise convert it to a string and return that."
  (cl-etypecase string-or-symbol
    (stringp string-or-symbol)
    (numberp (number-to-string string-or-symbol))
    (symbolp (symbol-name string-or-symbol))))

;; TODO: maybe do something similar to `magithub--repo-parse-url':
;; https://github.com/vermiculus/magithub/blob/7051c1b/magithub-core.el#L533
(defun company-issue-git-parse-url (url)
  "Parse an git remote URL, and return an url struct."
  (url-generic-parse-url (if (and (not (string-match-p "^[a-zA-Z_-]+://" url))
                                  (string-match-p ":" url)
                                  (not (string-match-p "\\\\\\\\" url))) ;; Two literal backlashes
                             (concat "ssh://" (subst-char-in-string ?: ?/ url))
                           url)))

(defun company-issue-parse-remote-url (url)
  "Return a list (hostname slug) from an git URL.

Where slug is stripped from its \".git\" suffix and \"/\"
prefix."
  (let ((urlobj (company-issue-git-parse-url url)))
    (list (url-host urlobj) (string-remove-prefix "/" (string-remove-suffix ".git" (url-filename urlobj))))))

(defun company-issue-load-object (filename)
  "Read obj from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(cl-defun company-issue-issues (&optional (host company-issue-host) (slug company-issue-slug))
  "Read company issues from cache from HOST and SLUG."
  (and host slug
       (or (gethash company-issue-root company-issue-project)
           (let* ((cache-file (expand-file-name "company-issue" (magit-git-dir)))
                  (from-cache (file-exists-p cache-file))
                  (issues (if from-cache
                              (company-issue-load-object cache-file)
                            (company-issue-fetch (company-issue-as-symbol host) slug))))
             (or from-cache (with-temp-file cache-file (pp issues (current-buffer))))
             (puthash company-issue-root issues company-issue-project)))))

;;;###autoload
(defun company-issue-setup (&optional force)
  "Read config from git, when FORCE is non-nil will thy for ask again."
  (interactive "P")
  (and (or force (not (magit-get "company-issue" "host")))
       (cl-multiple-value-bind (host slug)
           (company-issue-parse-remote-url (magit-get "remote" (magit-read-remote "Remote" nil 'use-only) "url"))
         (magit-set host "company-issue" "host")
         (magit-set slug "company-issue" "slug"))))

(cl-defstruct (company-issue-item (:constructor company-issue-item-new))
  "A structure holding the information of an issue."
  id url desc title)

(cl-defgeneric company-issue-fetch (host slug)
  (:documentation "Fetch `company-issues' from backend."))

(defun company-issue-prefix ()
  "Get a prefix from current position."
  (cons (company-grab "#\\([[:digit:]]*\\)" 1) t))

(defun company-issue-candidates (prefix)
  "Return issues candidates for current project with PREFIX."
  (all-completions prefix (company-issue-issues)))

(defun company-issue-meta (candidate)
  "Return company meta string for a CANDIDATE."
  (company-issue-item-title (gethash candidate (company-issue-issues))))

(defun company-issue-annotation (candidate)
  "Return company annotation string for a CANDIDATE."
  (company-issue-item-title (gethash candidate (company-issue-issues))))

(defun company-issue-doc-buffer (candidate)
  "Return a company documentation buffer from a CANDIDATE."
  (company-doc-buffer (company-issue-item-desc (gethash candidate (company-issue-issues)))))

;;;###autoload
(defun company-issue (command &optional arg &rest ignored)
  "`company-mode' completion back-end for issues.

Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-issue))
    (init (and (setq company-issue-root (magit-toplevel))
               (or (and company-issue-host company-issue-slug)
                   (cl-multiple-value-setq (company-issue-host company-issue-slug)
                     (list (magit-get "company-issue" "host") (magit-get "company-issue" "slug"))))))
    (prefix (or (company-issue-prefix) 'stop))
    (candidates (company-issue-candidates arg))
    (meta (company-issue-meta arg))
    (annotation (company-issue-annotation arg))
    (doc-buffer (company-issue-doc-buffer arg))))

(provide 'company-issue)
;;; company-issue.el ends here
