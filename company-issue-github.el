;;; company-issue-github.el --- company-mode completion for GitHub issues -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/company-issue.el
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.4") (ghub "1.1.0"))

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

;; company-mode completion for GitHub issues using `ghub'.

;;; Code:
(eval-when-compile
  (require 'let-alist))
(require 'ghub)
(require 'company-issue)

(defgroup company-issue-github nil
  "company-mode completion back-end for GitHub issues."
  :prefix "company-issue-github-"
  :group 'company-issue)

(defcustom company-issue-github-unpaginate t
  "Whether to make an un-paginated call to fetch the GitHub issues."
  :type 'boolean
  :safe 'booleanp
  :group 'company-issue-github)

(defvar company-issue-github-per-page 100
  "Number of issues to list per page, by default 100 which is the max allowed.")

(cl-defmethod company-issue-fetch ((_host (eql github.com)) slug)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (issue (ghub-get (format "/repos/%s/issues" slug) nil :query `((per_page . ,(number-to-string company-issue-github-per-page))) :unpaginate company-issue-github-unpaginate))
      (let-alist issue
        (puthash (company-issue-as-string .number)
                 (company-issue-item-new :id    .number
                                         :url   .html_url
                                         :title .title
                                         :desc  .body)
                 table)))
    table))

(provide 'company-issue-github)
;;; company-issue-github.el ends here
