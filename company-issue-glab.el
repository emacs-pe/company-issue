;;; company-issue-glab.el --- company-mode completion for GitLab issues -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/company-issue
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.4") (glab "1.0"))

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

;; company-mode completion for GitLab issues using `glab'.

;;; Code:
(eval-when-compile
  (require 'let-alist))
(require 'glab)
(require 'company-issue)

(defgroup company-issue-glab nil
  "company-mode completion back-end for GitLab issues."
  :prefix "company-issue-glab"
  :group 'company-issue)

(defcustom company-issue-glab-unpaginate t
  "Whether to make an un-paginated call to fetch the GitLab issues."
  :type 'boolean
  :safe 'booleanp
  :group 'company-issue-glab)

(cl-defmethod company-issue-fetch ((_host (eql gitlab.com)) slug)
  ;; NB: URL encode slug "user/repo" to avoid to know first hand the
  ;;     project_id.
  ;;     https://docs.gitlab.com/ee/api/projects.html#get-single-project
  (let ((table (make-hash-table :test 'equal))
        (glab-unpaginate company-issue-glab-unpaginate))
    (dolist (issue (glab-get (format "/projects/%s/issues" (url-hexify-string slug))))
      (let-alist issue
        (puthash (company-issue-as-string .iid)
                 (company-issue-item-new :id    .iid
                                         :url   .web_url
                                         :title .title
                                         :desc  .description)
                 table)))
    table))

(provide 'company-issue-glab)
;;; company-issue-glab.el ends here
