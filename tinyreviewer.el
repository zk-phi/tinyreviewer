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

;; * ----- Utility

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

;; * ----- Parse `git show` output

;; Commit:     (HEADER . (FILE-PATCH ...))
;; File patch: ((FROM-FILENAME . TO-FILENAME) . (HUNK ...))
;; Hunk:       List of commands -- either (delete LINENO) or (insert LINENO STRING)

(defun git-review--get-commit (revision)
  "Get `git show REVISION` parsed."
  (let* ((command (format "git show --patch %s" (shell-quote-argument revision)))
         (res (shell-command-to-string command))
         (splitted (git-review--split-string res "^diff --git a/\\(.+\\) b/\\(.+\\)$")))
    (cons (car splitted) (mapcar 'git-review--parse-file-patch (cdr splitted)))))

(defun git-review--parse-file-patch (file-patch)
  "Internal function for `git-review--get-commit'."
  (cl-destructuring-bind (header . hunks)
      (git-review--split-string file-patch "@@[^\n]*@@")
    (let ((file (or (and (string-match ; create, delete, modify, move (with modification)
                          "^--- \\(?:a/\\)?\\(.*\\)\n\\+\\+\\+ \\(?:b/\\)?\\(.*\\)$" header)
                         (cons (match-string 1 header) (match-string 2 header)))
                    (and (string-match        ; move (without modification)
                          "^diff --git a/\\(.*\\) b/\\(.*\\)$" header)
                         (cons (match-string 1 header) (match-string 2 header))))))
      (cons file (git-review--parse-patch-hunks hunks)))))

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

;; (git-review--get-commit "d22872")
;; ("commit d22872a372cab28f81376fed8b4878c1a5c0b0db
;; Author: p <_>
;; Date:   Sat Oct 28 00:35:43 2017 +0900
;;
;;     hoge
;;
;; "
;;  (("/dev/null" . "hoge")
;;   (insert 0 "hoge")
;;   (insert 1 "hoge")
;;   (insert 2 "hoge")
;;   (insert 3 "")))

;; (git-review--get-commit "90e8ba")
;; ("commit 90e8bafac82af2ba8ef98ab167b9f922f6f6beff
;; Author: p <_>
;; Date:   Wed Nov 1 09:14:25 2017 +0900
;;
;;     hoge
;;
;; "
;;  (("hoge" . "hoge")
;;   (delete 18)
;;   (insert 18 " 18: hage")
;;   (delete 30)
;;   (delete 37)
;;   (insert 37 " 38: piyo")
;;   (delete 39)
;;   (insert 39 " 40: huge")
;;   (insert 40 " 401:aaa")))

;; * ----- Make "combined diff" from commits

(defvar-local git-review--original-file-name nil
  "Initial file name for the buffer")
(defvar-local git-review--file-name-changes nil
  "List of new file names of the form (FILENAME . COMMIT-INDEX) for the buffer, if moved.")
(defvar-local git-review--display-file-name nil
  "User-friendly file name for the buffer.")

(defun git-review--open-file-for-combined-diff (file first-revision commit-count)
  "Internal function for `git-review--make-combined-diffs'. Open
file for combined diff, and returns the buffer."
  (let ((buf (generate-new-buffer (concat "*review " (file-name-nondirectory file) "*"))))
    (with-current-buffer buf
      (unless (string= file "/dev/null")
        (save-excursion
          (let ((cmd (format "git show %s:%s"
                             (shell-quote-argument (concat first-revision "^"))
                             (shell-quote-argument file))))
            (insert (shell-command-to-string cmd))))
        (replace-regexp "^" (make-string commit-count ?\s))
        (goto-char (point-min)))
      (setq git-review--original-file-name file
            git-review--display-file-name  file))
    buf))

(defun git-review--apply-filename-change (to-filename commit-index)
  "Internal function for `git-review--make-combined-diffs'. Record
filename changes to the current buffer's local variables, which
can be formatted later (by
`git-review--finalize-combined-diff')."
  (push (cons to-filename commit-index) git-review--file-name-changes)
  (when (string= git-review--display-file-name "/dev/null")
    (setq git-review--display-file-name to-filename)))

(defun git-review--apply-hunk (hunk commit-index commit-count)
  "Internal function for `git-review--make-combined-diffs'. Apply
a hunk to the current buffer. This function uses overlays to
record deleted lines, which should be replaced with real strings
later (by `git-review--finalize-combined-diff')."
  (save-excursion
    (dolist (command hunk)
      (goto-line (cadr command))
      (cl-case (car command)
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
                 (cl-caddr command) "\n"))))))

(defun git-review--finalize-combined-diff (commit-count)
  "Internal function for
`git-review--make-combined-diffs'. Replace all intermediate
overlays with real strings, and adds the file name changes."
  (save-excursion
    (mapc (lambda (ov)
            (goto-char (overlay-start ov))
            (insert (overlay-get ov 'before-string))
            (delete-overlay ov))
          (overlays-in (point-min) (point-max))))
  (save-excursion
    (insert "Combied Diff: " git-review--display-file-name "\n"
            "\n==============================\n")
    (when git-review--file-name-changes
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
    (insert "File Content Changes:\n\n")))

;; (let ((buffer (git-review--open-file-for-combined-diff "./hoge" "90e8ba" 3)))
;;   (with-current-buffer buffer
;;     (git-review--apply-hunk '((delete 18)
;;                                (insert 18 " 18: hage")
;;                                (delete 30)
;;                                (delete 37)
;;                                (insert 37 " 38: piyo")
;;                                (delete 39)
;;                                (insert 39 " 40: huge")
;;                                (insert 40 " 401:aaa"))
;;                              0 3)
;;     (git-review--apply-filename-change "./fuga" 1)
;;     (git-review--apply-hunk '((delete 18)
;;                                (insert 18 " 18: brabrabra"))
;;                              1 3)
;;     (git-review--apply-filename-change "./piyo" 2)
;;     (git-review--apply-hunk '((delete 18)
;;                                (insert 18 " 18: piyopiyopiyo")
;;                                (delete 52)
;;                                (insert 52 " 52: piyopiyopiyo"))
;;                              2 3)
;;     (git-review--finalize-combined-diff 3))
;;   (display-buffer buffer))

;; (let ((buffer (git-review--open-file-for-combined-diff "/dev/null" nil 1)))
;;   (with-current-buffer buffer
;;     (git-review--apply-filename-change "./dada" 0)
;;     (git-review--apply-hunk '((insert 1 "hoge")
;;                                (insert 2 "fuga")) 0 1)
;;     (git-review--finalize-combined-diff 1))
;;   (display-buffer buffer))

;; * ----- Make combined diff of all files from commits

(defun git-review--make-combined-diffs (commit-file-patches first-revision)
  "Make and return combined diff buffers for files deleted,
created or modified in COMMITS."
  (let ((commit-count (length commit-file-patches))
        (headers nil)
        (deleted-files nil)                             ; List[Buffer]
        (modified-files (make-hash-table :test 'equal)) ; Map[Filename][Buffer]
        (commit-index 0))
    (dolist (flie-patches commit-file-patches)
      (dolist (file-patch flie-patches)
        (cl-destructuring-bind ((from-file . to-file) . hunks) file-patch
          (let ((buf (gethash from-file modified-files)))
            (unless buf
              (setq buf (git-review--open-file-for-combined-diff from-file first-revision commit-count))
              (puthash from-file buf modified-files))
            (unless (string= from-file to-file)
              (git-review--apply-filename-change to-file commit-index)
              (remhash from-file modified-files)
              (if (string= to-file "/dev/null")
                  (push buf deleted-files)
                (puthash to-file buf modified-files)))
            (with-current-buffer buf
              (git-review--apply-hunk hunks commit-index commit-count)))))
      (cl-incf commit-index))
    (maphash
     (lambda (k v) (with-current-buffer v (git-review--finalize-combined-diff commit-count)))
     modified-files)
    (mapc
     (lambda (v) (with-current-buffer v (git-review--finalize-combined-diff commit-count)))
     deleted-files)
    ;; return value
    (let ((res deleted-files))
      (maphash (lambda (_ v) (push v res)) modified-files)
      res)))

;; (git-review--make-combined-diffs
;;  (list (cdr (git-review--get-commit "b3a980319f9cbb6c284a2475c68231a641ff3a15"))
;;        (cdr (git-review--get-commit "90e8bafac82af2ba8ef98ab167b9f922f6f6beff")))
;;  "b3a980319f9cbb6c284a2475c68231a641ff3a15")

(provide 'tinyreviewer)

;;; tinyreviewer.el ends here
