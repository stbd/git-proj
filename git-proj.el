;; git-proj - Small extension for Emacs to help working with Git projects
;;
;; Author: Olli Huurinainen, huurinainen.olli@gmail.com
;; Version: 0.1
;;
;; Copyright (C) 2015  Olli Huurinainen
;;
;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun git-proj-open-file-with-optional-line-number (description)
  (let ((values (split-string description ":"))) ; Try to split on ":"
     (if (> (safe-length values) 0)
         (find-file (nth 0 values)))
     (if (> (safe-length values) 1)
         (forward-line (- (string-to-number (nth 1 values)) 1)))
 ))

(defun git-proj-get-buffer-and-prepare-window ()
  (let ((buffer (get-buffer-create "git-proj-buffer")))
    (if (eq (get-buffer-window buffer 'visible) nil) ; If buffer is not visible
        (progn
         (split-window-sensibly (selected-window))   ; Split and show
         (switch-to-buffer buffer)
         )
      (set-buffer buffer)
      )
    (erase-buffer)                                   ; Always clean buffer and
    buffer                                           ; return it
))

(defun git-proj-show-search-result-with-absolute-paths
  (description path-prefix list-of-lines-starting-with-file)
  (let ((result "")
        (buffer (git-proj-get-buffer-and-prepare-window))
        )
    (dolist (var list-of-lines-starting-with-file result)
      (setq result (concat result path-prefix var "\n"))    ; For each line, add abolute path
      )
    (princ description buffer)
    (princ "\n\n" buffer)
    (princ result buffer)
))

(defun git-proj-get-filename ()
  (let ((filename (replace-regexp-in-string "\'" "" ; Cleanup filename
          (replace-regexp-in-string "\"" ""
          (thing-at-point 'filename))))
          )
    filename
))

(defun git-proj-goto-file-impl (search-root filename)
  (let ((default-directory search-root))                      ; Operate in search-root folder
    (with-temp-buffer
      (call-process "git" nil t t "ls-files" (concat "*" filename))
      (cond
       ((not (equal (string-prefix-p "fatal" (buffer-string)) nil))
        (message (buffer-string))
        )
       ((string-match (buffer-string) "")
        (message "git-proj-goto-file: file \"%s\" not found" filename)
        )
       ((equal (count-lines (point-min) (point-max)) 1)
        (git-proj-open-file-with-optional-line-number
         (concat search-root (replace-regexp-in-string "\n$" "" (buffer-string))))
        )
       ((> (count-lines (point-min) (point-max)) 1)
        (git-proj-show-search-result-with-absolute-paths
         "Multiple matching files (use git-proj-goto-file to goto file): "
         search-root
         (split-string (buffer-string) "\n" t))                ; Split to lines
        )
       (t
        (message "git-proj-goto-file: %s" (buffer-string))
)))))

(defun git-proj-goto-file ()
  "Tries to open file referenced by string under cursor"
  (interactive)
  (let ((filename (git-proj-get-filename)))

    (if (not (equal filename nil))       ; If cursor points to something
        (cond
         ((string-prefix-p "/" filename) ; If it is absolute path, just open it
          (git-proj-open-file-with-optional-line-number filename))
         (t                              ; Else, search for it
          (git-proj-goto-file-impl git-proj-root filename)
          )
         )
      (message "git-proj-goto-file: empty filename")
)))

(defun git-proj-grep-impl (search-root symbol)
  (let ((default-directory search-root))                            ; Operate in search-root folder
    (with-temp-buffer
      (call-process "git" nil t t "grep" "-I" "-n" "-w" symbol)
      (cond
       ((not (equal (string-prefix-p "fatal" (buffer-string)) nil))
        (message (buffer-string))
        )
       ((string-match (buffer-string) "")
        (message "git-proj-goto-def: symbol \"%s\" not found" symbol)
        )
       (t
        (git-proj-show-search-result-with-absolute-paths
         "Search result (use git-proj-goto-file to goto location): "
         search-root
         (split-string (buffer-string) "\n" t)        ; Split to lines
))))))

(defun git-proj-grep ()
  "Calls git grep for symbol under cursor"
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if (not (equal symbol nil))
        (git-proj-grep-impl git-proj-root symbol)
      (message "git-proj-grep-def: empty symbol")
)))

(add-hook 'after-init-hook
          (lambda ()
            (if (equal (boundp 'git-proj-root) nil)
                (setq git-proj-root default-directory)
              )))

(provide 'git-proj)
