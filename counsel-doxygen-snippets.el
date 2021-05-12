;;; counsel-doxygen-snippets.el --- Counsel frontend to search and insert references to doxygen code snippets. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; Homepage:
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1")  (dash "2.17.0") (counsel "0.13.0"))
;; Keywords: convenience, languages, tools

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;;; Commentary:

;;; Code:

(require 'dash)
(require 'ivy)
(require 'counsel)

(defgroup counsel-doxygen-snippets nil
  "`counsel' for doxygen code snippets"
  :group 'counsel)

(defcustom counsel-doxygen-snippets-prefix " @snippet"
  "Prefix to be inserted in front of the snippet reference."
  :safe #'stringp
  :type 'string)

(defcustom counsel-doxygen-snippets-root ""
  "Root directory to search for snippets in. Intended to be set in your .dir-locals.el files."
  :type 'string
  :safe (lambda (v) (and (stringp v) (file-directory-p v))))

(defvar counsel-doxygen-snippets--history nil)

(defun counsel-doxygen-snippets--project-root ()
  ""
  (let ((project-root nil))
    (when (member 'projectile-mode minor-mode-list)
      (setq project-root (projectile-project-root)))
    (unless project-root
      (setq project-root (locate-dominating-file (buffer-file-name) ".dir-locals.el")))
    (unless project-root
      (setq project-root (vc-root-dir)))
    project-root))

(defun counsel-doxygen-snippets--root ()
  ""
  (if (file-name-absolute-p counsel-doxygen-snippets-root)
      counsel-doxygen-snippets-root
    (concat (counsel-doxygen-snippets--project-root) counsel-doxygen-snippets-root)))

(defun counsel-doxygen-snippets--path (file)
  ""
  (let ((file (file-relative-name file (counsel-doxygen-snippets--root))))
    (if (string-prefix-p "./" file)
        file
      (format "./%s" file))))

(defun counsel-doxygen-snippets--insert-snippet (str)
  "Insert the formatted snippet reference based on STR."
  (when (string-match "\\(.*\\):[\t\s]+//![\t\s]?\\[\\(.*\\)\\]" str)
    (let ((file (match-string 1 str))
          (name (match-string 2 str)))
      (insert (format "%s %s %s" counsel-doxygen-snippets-prefix (counsel-doxygen-snippets--path file) name)))))

(defun counsel-doxygen-snippets--function (str)
  "Search for STR."
  (-distinct (split-string (shell-command-to-string (format "rg --no-heading --no-line-number -M 120 --color never -e '//!\\s\\[.*?%s.*?\\]'" (counsel--elisp-to-pcre (ivy--regex-plus str)))) "[\n]" t)))

;;;###autoload
(defun counsel-doxygen-snippets (&optional initial-input)
  ""
  (interactive)
  (let ((default-directory (or (counsel-doxygen-snippets--root) default-directory)))
    (ivy-read "name: "
              #'counsel-doxygen-snippets--function
              :require-match t
              :sort t
              :multi-action nil
              :history 'counsel-doxygen-snippets--history
              :dynamic-collection t
              :initial-input initial-input
              :unwind #'counsel-delete-process
              :caller 'counsel-doxygen-snippets
              :action #'counsel-doxygen-snippets--insert-snippet)))

(provide 'counsel-doxygen-snippets)

;;; counsel-doxygen-snippets.el ends here
