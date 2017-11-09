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

;; * Utility

(defun git-review--split-string (string regexp)
  "Split string with regexp. Strings in the resulting list may
contain REGEXP at the beginning."
  (with-temp-buffer
    (save-excursion (insert string))
    (let ((beg (point)) res)
      (while (search-forward-regexp regexp nil t)
        (unless (= beg (match-beginning 0))
          (push (buffer-substring beg (match-beginning 0)) res))
        (setq beg (match-beginning 0)))
      (unless (= beg (point-max))
        (push (buffer-substring beg (point-max)) res))
      (nreverse res))))

;; * Parse `git show` output

(defun git-review--get-patches (revision)
  "Get `git show REVISION` parsed."
  (let* ((command (format "git show --patch %s" (shell-quote-argument revision)))
         (res (shell-command-to-string command))
         (splitted (git-review--split-string res "^diff --git a/\\(.+\\) b/\\(.+\\)$")))
    (list :header (car splitted) :files (mapcar 'git-review--parse-file-patch (cdr splitted)))))

(defun git-review--parse-file-patch (file-patch)
  "Internal function for `git-review--get-patches'."
  (cl-destructuring-bind (header . hunks)
      (git-review--split-string file-patch "@@[^\n]*@@$")
    (let ((file (or (and (string-match ; create, delete, modify, move (with modification)
                          "^--- \\(?:a/\\)?\\(.*\\)\n\\+\\+\\+ \\(?:b/\\)?\\(.*\\)$" header)
                         (cons (match-string 1 header) (match-string 2 header)))
                    (and (string-match        ; move (without modification)
                          "^diff --git a/\\(.*\\) b/\\(.*\\)$" header)
                         (cons (match-string 1 header) (match-string 2 header)))))
          (hunks (git-review--parse-patch-hunks hunks)))
      (unless (string= (car file) (cdr file)) (push (list 'move (cdr file)) hunks))
      (list :file (car file) :diff hunks))))

(defun git-review--parse-patch-hunks (hunks)
  "Internal function for `git-review--parse-file-patch'."
  (let ((offset 0) res)
    (dolist (hunk hunks)
      (cl-destructuring-bind (header . lines) (split-string hunk "\n" t)
        (let ((lineno (progn
                        (string-match "^@@ -\\([0-9]*\\)," header)
                        (+ (read (match-string 1 header)) offset))))
          (dolist (line lines)
            (cl-case (aref line 0)
              ((?-)
               (push (list 'delete lineno) res)
               (cl-incf offset -1))
              ((?+)
               (push (list 'insert lineno (substring line 1)) res)
               (cl-incf lineno)
               (cl-incf offset))
              (t
               (cl-incf lineno)))))))
    (nreverse res)))

;; (git-review--get-patches "d22872")
;; (:header "commit d22872a372cab28f81376fed8b4878c1a5c0b0db
;; Author: p <_>
;; Date:   Sat Oct 28 00:35:43 2017 +0900
;;
;;     hoge
;;
;; " :files
;; ((:file "/dev/null" :diff
;;         ((move "hoge")
;;          (insert 0 "hoge")
;;          (insert 1 "hoge")
;;          (insert 2 "hoge")
;;          (insert 3 "")))))

;; (git-review--get-patches "90e8ba")
;; (:header "commit 90e8bafac82af2ba8ef98ab167b9f922f6f6beff
;; Author: p <_>
;; Date:   Wed Nov 1 09:14:25 2017 +0900
;;
;;     hoge
;;
;; " :files
;; ((:file "hoge" :diff
;;         ((delete 18)
;;          (insert 18 " 18: hage")
;;          (delete 30)
;;          (delete 37)
;;          (insert 37 " 38: piyo")
;;          (delete 39)
;;          (insert 39 " 40: huge")
;;          (insert 40 " 401:aaa")))))

;; * Make "combined diff" from parsed patches

(defvar-local git-review--original-file-name nil)
(defvar-local git-review--file-name-changes nil)

(defun git-review--open-file-for-combined-diff (file initial-commit commit-count)
  "Internal function for `git-review--make-combined-diff'. Open
file for combined diff, and returns the buffer."
  (let ((buf (generate-new-buffer (concat "*review " (file-name-nondirectory file) "*"))))
    (unless (string= file "/dev/null")
      (with-current-buffer buf
        (save-excursion
          (let ((cmd (format "git show %s:%s"
                             (shell-quote-argument (concat initial-commit "^"))
                             (shell-quote-argument file))))
            (insert (shell-command-to-string cmd))))
        (replace-regexp "^" (make-string commit-count ?\s))
        (setq git-review--original-file-name file)
        (goto-char (point-min))))
    buf))

(defun git-review--apply-file-patch (file-patch commit-index commit-count)
  "Internal function for `git-review--make-combined-diff'. Apply
hunks to the current buffer. This function uses overlays to
record deleted lines, which should be replaced with real strings
later (by `git-review--finalize-combined-diff')."
  (when (eq (caar file-patch) 'move)
    (push (cons (cadr (pop file-patch)) commit-index) git-review--file-name-changes))
  (save-excursion
    (dolist (hunk file-patch)
      (goto-line (cadr hunk))
      (cl-case (car hunk)
        ((delete)
         (save-excursion
           (forward-char commit-index)
           (delete-char 1) (insert "-"))
         (let ((str (concat (buffer-substring (point-at-bol) (point-at-eol)) "\n"))
               ov newstr)
           (cond ((setq ov (car (overlays-at (point))))
                  (setq newstr (concat (overlay-get 'before-string ov) str)))
                 ((setq ov (car (overlays-at (point-at-bol +1))))
                  (setq newstr (concat str (overlay-get 'before-string ov))))
                 ((setq ov (make-overlay (point) (point)))
                  (setq newstr str)))
           (overlay-put ov 'before-string newstr))
         (kill-whole-line))
        ((insert)
         (insert (make-string commit-index ?\s)
                 "+"
                 (make-string (- commit-count commit-index 1) ?\s)
                 (cl-caddr hunk) "\n"))))))

(defun git-review--finalize-combined-diff (commit-count)
  "Internal function for
`git-review--make-combined-diff'. Replace all intermediate
overlays with real strings, and adds the file name changes."
  (save-excursion
    (insert "Combied Diff: " git-review--original-file-name "\n"
            "\n==============================\n")
    (when (print git-review--file-name-changes)
      (insert "File Name Changes:\n\n"
              (make-string commit-count ?\s) git-review--original-file-name "\n")
      (dolist (change (nreverse git-review--file-name-changes))
        (save-excursion
          (forward-line -1)
          (forward-char (cdr change))
          (delete-char 1)
          (insert "-"))
        (insert (make-string (cdr change) ?\s)
                "+"
                (make-string (- commit-count (cdr change) 1) ?\s)
                (car change) "\n"))
      (insert "\n==============================\n"))
    (insert "File Content Changes:\n\n"))
  (mapc (lambda (ov)
          (goto-char (overlay-start ov))
          (insert (overlay-get ov 'before-string))
          (delete-overlay ov))
        (overlays-in (point-min) (point-max))))

;; (let ((buffer (git-review--open-file-for-combined-diff "./hoge" "90e8ba" 3)))
;;   (with-current-buffer buffer
;;     (git-review--apply-file-patch '((delete 18)
;;                                     (insert 18 " 18: hage")
;;                                     (delete 30)
;;                                     (delete 37)
;;                                     (insert 37 " 38: piyo")
;;                                     (delete 39)
;;                                     (insert 39 " 40: huge")
;;                                     (insert 40 " 401:aaa"))
;;                                   0 3)
;;     (git-review--apply-file-patch '((move "./fuga")
;;                                     (delete 18)
;;                                     (insert 18 " 18: brabrabra"))
;;                                   1 3)
;;     (git-review--apply-file-patch '((move "./piyo")
;;                                     (delete 18)
;;                                     (insert 18 " 18: piyopiyopiyo")
;;                                     (delete 52)
;;                                     (insert 52 " 52: piyopiyopiyo"))
;;                                   2 3)
;;     (git-review--finalize-combined-diff 3))
;;   (display-buffer buffer))

(provide 'tinyreviewer)

;;; tinyreviewer.el ends here
