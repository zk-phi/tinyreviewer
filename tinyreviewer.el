;;; tinyreviewer.el --- A simple tool to review a git branch.

;; Copyright (C) 2017- zk-phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk-phi
;; URL: https://github.com/zk-phi/tinyreviewer
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:

;;; Change Log:

;;; Code:

(defconst tinyreviewer-version "1.0.0")

(defgroup tinyreviewer nil
  "A simple tool to review a git branch."
  :group 'emacs)

;; * Analyze diffs

(defun git-review--get-patch (revision1 &optional revision2)
  (let ((revision1 (shell-quote-argument revision1))
        (revision2 (shell-quote-argument (or revision2 (concat revision1 "^"))))
        res)
    (with-temp-buffer
      (save-excursion
        (insert (shell-command-to-string (format "git diff -u %s %s" revision2 revision1))))
      (search-forward-regexp "^diff --git a/\\(.+\\) b/\\(.+\\)$")
      (let (file1 file2 beg)
        (while (progn
                 (setq file1 (match-string 1)
                       file2 (match-string 2)
                       beg (match-beginning 0))
                 (search-forward-regexp "^diff --git a/\\(.+\\) b/\\(.+\\)$" nil t))
          (push (list file1 file2 (buffer-substring beg (match-beginning 0))) res))
        (push (list file1 file2 (buffer-substring beg (point-max))) res))
      res)))

;; * Manage commit list

(provide 'tinyreviewer)

;;; tinyreviewer.el ends here
