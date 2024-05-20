;;; project-butler.el --- Lay out a project's buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Stefan Thesing

;; Author: Stefan Thesing <software@webdings.de>
;; Keywords: convenience, projects
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://codeberg.org/jabbo/project-butler

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Add-on for `project.el`. It lets you declare buffer/window arrangements
;; in your config and lays them out for you. It also cleans up after you.

;;; Code:

;;;; Requirement
(require 'project)

;;;; Customizations
(defgroup project-butler '()
  "Customization for `project-butler'."
  :tag "Project Butler"
  :group 'project)

(defcustom project-butler-projects-list '()
  "A list that maps project directories to parameters.
These parameters are a window pattern and a lists of
files to be automatically opened by `project-butler-open'."
  :type 'list
  :group 'project-butler)

(defcustom project-butler-confirm-cleanup t
  "Ask the user to confirm when using `project-butler-cleanup'."
  :type 'boolean
  :group 'project-butler)

(defcustom project-butler-add-open-command t
  "Add `project-butler-open' to `project-switch-commands'.
These are offered to the user when switching projects."
  :type 'boolean
  :group 'project-butler)


;;;; Commands
(defun project-butler-open (&optional proj-dir)
  "Lookup PROJ-DIR in the `project-butler-projects-list' variable.
Read the defined window pattern and path list and finally open the buffers.
If the path-list is empty, open the project directory in dired."
  (interactive)
  (unless proj-dir
    (setq proj-dir (car (last (project-current t))))) ; project picked by user

  (let* ((proj (alist-get proj-dir project-butler-projects-list
                          nil nil 'string-equal))
         (window-pattern (car proj))
         (path-list (car (cdr proj))))
    (if path-list
        (project-butler--place-buffers proj-dir path-list window-pattern)
      (dired proj-dir))))

(defun project-butler-cleanup ()
  "Clean up the project.
Close all buffers that are in its path-list and (optionally) of files in
the project directory. Revert all window splits in the current frame."
  (interactive)
  (let ((root (project-root (project-current nil))))
    (when (or (not project-butler-confirm-cleanup)
              (yes-or-no-p (format "Clean up project %s?" root)))
      (project-kill-buffers :no-confirm)
      (delete-other-windows))))


;;;; Functions
(defun project-butler--ensure-pattern (path-list window-pattern)
  "Check if WINDOW-PATTERN is within bounds of PATH-LIST.
Throw an error if any number in window-pattern exceeds the length
of path-list."
  ;; check for invalid characters
  (if (string-match "^[0-9|_<^>v]*$" window-pattern)
      ;; Convert window-pattern to a list of single-digit strings
      (let* ((pattern-list (string-to-list window-pattern))
             (pattern-numbers
              (delq nil
                    (mapcar (lambda (x)
                              (when (string-match-p "[0-9]" (string x))
                                ;; Don't forget to correct for 0-index
                                (1- (string-to-number (string x)))))
                            pattern-list))))
        ;; Check if any number in pattern-numbers exceeds the length of
        ;; path-list
        (when (cl-some (lambda (n) (> n (length path-list))) pattern-numbers)
          (error "Number in window-pattern exceeds length of path-list!")))
    (error "Invalid window-pattern")))

(defun project-butler--normalize-path (path base-directory)
  "Normalize PATH.
If it's not absolute, expand it relative to BASE-DIRECTORY."
  (if (file-name-absolute-p path)
      path
    (expand-file-name path base-directory)))


(defun project-butler--place-buffers
    (proj-dir path-list &optional window-pattern)
  "Open paths and arrange them in windows.
Paths are specified by PATH-LIST, relative to PROJ-DIR.
WINDOW-PATTERN is a command sequence. The following commands are valid:
- Digits -- In the current window, open a path in the path-list. 1 means the
            first path in the list, 2 means the second path and so on.
- `_' -- Split the current window below and move the point to the new window.
- `|' -- Split the current window to the right and move the point to the new
         window.
- `<' -- Move the point to the window to the left of the current window.
- `>' -- Move the point to the window to the right of the current window.
- `v' -- Move the point to the window below the current window.
- `^' -- Move the point to the window above the current window.

Paths not explicitly mentioned in the WINDOW-PATTERN are opened
in the background. See the documentation for details and examples."
  ;; Let's start by opening all the buffers for path-list
  (dolist (path path-list)
    ;; path list can contain absolute or relative paths, we normalize
    ;; them here
    (find-file (project-butler--normalize-path path proj-dir)))
  ;; If we have a window-pattern or if it is an empty string, we're done.
  ;; Otherwise, let's place the buffers in their respective windows:
  (when (and window-pattern (not (string= window-pattern "")))
    (progn
      ;; First, check the window-pattern for errors
      (project-butler--ensure-pattern path-list window-pattern)
      ;; If we're still good to go, let's go through the pattern
      (dotimes (i (length window-pattern))
        (let ((char (substring window-pattern i (1+ i))))
          ;; Depending on the current character in window-pattern, perform the
          ;; corresponding action
          (condition-case nil
              (cond ((string-match-p "[0-9]" char)
                     (let ((path (nth (1- (string-to-number char))
                                      path-list)))
                       ;; we normalize paths here, as well
                       (find-file
                        (project-butler--normalize-path path proj-dir))))
                    ((string= "_" char)
                     (split-window-below)
                     (other-window 1))
                    ((string= "|" char)
                     (split-window-right)
                     (other-window 1))
                    ((string= "<" char)
                     (windmove-left))
                    ((string= ">" char)
                     (windmove-right))
                    ((string= "v" char)
                     (windmove-down))
                    ((string= "^" char)
                     (windmove-up))
                    (t (message "Unknown character: %s" char)))
            (error (message "An error occurred, trying to continue..."))))))))


;;Add `project-butler-open' to the commands of project.el
(when project-butler-add-open-command
  (add-to-list 'project-switch-commands
               '(project-butler-open "Open buffers" "o") :append ))

(provide 'project-butler)
;;; project-butler.el ends here
