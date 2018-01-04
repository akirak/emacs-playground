;;; emacs-pg.el --- Run Emacs in a sandboxed home directory -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: emacs
;; URL: https://github.com/akirak/emacs-pg

;; This file is not part of GNU Emacs.

;;; License:

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

;;; Commentary:

;; 

;;; Code:

(require 'cl)
(require 'subr-x)

(defconst emacs-pg-original-home-directory (concat "~" user-login-name))

(defcustom emacs-pg-script-directory
  (expand-file-name ".local/bin" emacs-pg-original-home-directory)
  "The directory where the wrapper script is saved."
  :group 'pg)

(defcustom emacs-pg-directory
  (expand-file-name ".emacs-pg" emacs-pg-original-home-directory)
  "The directory where home directories of emacs-pg are stored."
  :group 'pg)

(defcustom emacs-pg-inherited-contents '(".gnupg")
  "Files and directories in the home directory that should be added to virtual home directories."
  :group 'pg)

(defun emacs-pg--emacs-executable ()
  (executable-find (car command-line-args)))

(defun emacs-pg--script-paths ()
  (let ((dir emacs-pg-script-directory)
        (original-name (file-name-nondirectory (emacs-pg--emacs-executable))))
    (mapcar (lambda (filename) (expand-file-name filename dir))
            (list original-name (concat original-name "-nopg")))))

(defun emacs-pg--read-url (prompt)
  (read-from-minibuffer prompt))

(defun emacs-pg--update-symlinks (dest)
  (let ((origin emacs-pg-original-home-directory))
    (cl-loop for relpath in emacs-pg-inherited-contents
             do (let ((src (expand-file-name relpath origin))
                      (new (expand-file-name relpath dest)))
                  (when (and (not (file-exists-p new))
                             (file-exists-p src))
                    (make-directory (file-name-directory new) t)
                    (make-symbolic-link src new))
                  ))))

(defun emacs-pg--parse-github-url (url)
  (pcase url
    ((pred (string-match "^git@github\.com:\\(.+\\)\\(?:\.git\\)$")) (match-string 1 url))
    ((pred (string-match "^https://github\.com/\\(.+?\\)\\(\.git\\)?$")) (match-string 1 url))
    ))

(defun emacs-pg--build-name-from-url (url)
  (pcase (emacs-pg--parse-github-url url)
    ('nil "")
    (rpath (car (split-string rpath "/")))))

;;;###autoload
(defun emacs-pg-add (url &optional name)
  (interactive "P")
  (unless url
    (setq url (emacs-pg--read-url  "Source repository (Git) for ~/.emacs.d: ")))
  (unless name
    (setq name (if (called-interactively-p 'any)
                   (read-from-minibuffer "Name for the config: "
                                         (emacs-pg--build-name-from-url url))
                 (or (emacs-pg--build-name-from-url url)
                     (error "Failed to get a name from the URL")))))
  (let ((dpath (expand-file-name name emacs-pg-directory)))
    (if (file-exists-p dpath)
        (progn (message (format "%s already exists" dpath))
               nil)
      (condition-case nil
          (progn
            (make-directory dpath t)
            (process-lines "git" "clone" "--recursive" "--depth=1"
                           url
                           (expand-file-name ".emacs.d" dpath))
            (emacs-pg--update-symlinks dpath)
            name)
        (error (progn (delete-directory dpath t)
                      (error "failed")))))))

;;;###autoload
(defun emacs-pg-update-symlinks ()
  (interactive)
  (mapc #'emacs-pg--update-symlinks
        (directory-files emacs-pg-directory t "^\[^.\]")))

(defvar emacs-pg-last-config-home)

;;;###autoload
(defun emacs-pg-try ()
  (interactive)
  (let* ((candidates (directory-files emacs-pg-directory
                                      nil
                                      "^\[^.\]"))
         (name-or-url (completing-read "Choose a configuration: " candidates))
         (name (if (member name-or-url candidates)
                   name-or-url
                 (emacs-pg-add name-or-url)))
         (home (expand-file-name name emacs-pg-directory))
         (process-environment (cons (concat "HOME=" home)
                                    process-environment)))
    (start-process "emacs-pg"
                   (format "*emacs-pg %s*" name)
                   (emacs-pg--emacs-executable))
    (setq emacs-pg-last-config-home home)))

;;;###autoload
(defun emacs-pg-adopt ()
  (interactive)

  (unless (boundp 'emacs-pg-last-config-home)
    (error "No emacs-pg instance has been run yet"))

  (let ((home emacs-pg-last-config-home)
        (emacs-bin (emacs-pg--emacs-executable)))
    (destructuring-bind
        (wrapper unwrapper) (emacs-pg--script-paths)
      (with-temp-file wrapper
        (insert (string-join `("#!/bin/sh"
                               ,(format "HOME=%s exec %s \"$@\""
                                        home
                                        emacs-bin))
                             "\n")))
      (with-temp-file unwrapper
        (insert (format "#!/bin/sh\nexec %s \"$@\"" emacs-bin)))
      (set-file-modes wrapper #o744)
      (set-file-modes unwrapper #o744))))

;;;###autoload
(defun emacs-pg-dismiss ()
  (interactive)
  (mapc 'delete-file (emacs-pg--script-paths)))

(provide 'emacs-pg)

;;; emacs-pg.el ends here
