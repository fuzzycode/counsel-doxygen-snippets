;;; counsel-doxygen-snippets.el --- Counsel frontend to search and insert references to doxygen code snippets. -*- lexical-binding: t; -*-

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
  :type 'string)

(defvar counsel-doxygen-snippets-root ""
  "Root directory to search for snippets in. Intended to be set in your .dir-locals.el files.")

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
    (concat (counsel-doxygen-snippet--project-root) counsel-doxygen-snippets-root)))

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
      (insert (format "%s %s %s" counsel-doxygen-snippets-prefix (counsel-doxygen-snippet--path file) name)))))

(defun counsel-doxygen-snippets--function (str)
  "Search for STR."
  (-distinct (split-string (shell-command-to-string (format "rg --no-heading --no-line-number -M 120 --color never -e '//!\\s\\[.*?%s.*?\\]'" (counsel--elisp-to-pcre (ivy--regex-plus str)))) "[\n]" t)))

;;;###autoload
(defun counsel-doxygen-snippets (&optional initial-input)
  ""
  (interactive)
  (let ((default-directory (or (counsel-doxygen-snippet--root) default-directory)))
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
