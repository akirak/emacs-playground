;;; play.el --- Run Emacs in a sandboxed home directory -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: emacs
;; URL: https://github.com/akirak/play.el

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

(defconst play-original-home-directory (concat "~" user-login-name))

(defcustom play-script-directory
  (expand-file-name ".local/bin" play-original-home-directory)
  "The directory where the wrapper script is saved."
  :group 'play)

(defcustom play-directory
  (expand-file-name ".emacs-play" play-original-home-directory)
  "The directory where home directories of play are stored."
  :group 'play)

(defcustom play-inherited-contents '(".gnupg")
  "Files and directories in the home directory that should be added to virtual home directories."
  :group 'play)

(defcustom play-dotemacs-list
      '(
        (:repo "https://github.com/bbatsov/prelude.git" :name "prelude")
        (:repo "https://github.com/seagle0128/.emacs.d.git")
        (:repo "https://github.com/purcell/emacs.d.git")
        (:repo "https://github.com/syl20bnr/spacemacs.git" :name "spacemacs")
        (:repo "https://github.com/eschulte/emacs24-starter-kit.git" :name "emacs24-starter-kit")
        )
      "List of configuration repositories suggested in play-checkout."
      )

(defun play--emacs-executable ()
  (executable-find (car command-line-args)))

(defun play--script-paths ()
  (let ((dir play-script-directory)
        (original-name (file-name-nondirectory (play--emacs-executable))))
    (mapcar (lambda (filename) (expand-file-name filename dir))
            (list original-name (concat original-name "-noplay")))))

(defun play--read-url (prompt)
  (read-from-minibuffer prompt))

(defun play--update-symlinks (dest)
  (let ((origin play-original-home-directory))
    (cl-loop for relpath in play-inherited-contents
             do (let ((src (expand-file-name relpath origin))
                      (new (expand-file-name relpath dest)))
                  (when (and (not (file-exists-p new))
                             (file-exists-p src))
                    (make-directory (file-name-directory new) t)
                    (make-symbolic-link src new))
                  ))))

(defconst play--github-repo-path-pattern
  "\\(?:[0-9a-z][-0-9a-z]+/[-a-z0-9_.]+?[0-9a-z]\\)")

(defconst play--github-repo-url-patterns
  (list (concat "^git@github\.com:\\("
                play--github-repo-path-pattern
                "\\)\\(?:\.git\\)$")
        (concat "^https://github\.com/\\("
                play--github-repo-path-pattern
                "\\)\\(\.git\\)?$")))

(defun play--github-repo-path-p (path)
  (let ((case-fold-search t))
    (string-match-p (concat "^" play--github-repo-path-pattern "$") path)))

(defun play--parse-github-url (url)
  (cl-loop for pattern in play--github-repo-url-patterns
           when (string-match pattern url)
           return (match-string 1 url)))

(defun play--github-repo-path-to-https-url (path)
  (concat "https://github.com/" path ".git"))

(defun play--build-name-from-url (url)
  (pcase (play--parse-github-url url)
    ('nil "")
    (rpath (car (split-string rpath "/")))))

;;;###autoload
(defun play-add (url &optional name)
  (interactive "P")
  (unless url
    (setq url (play--read-url  "Source repository (Git) for ~/.emacs.d: ")))
  (unless name
    (setq name (if (called-interactively-p 'any)
                   (read-from-minibuffer "Name for the config: "
                                         (play--build-name-from-url url))
                 (or (play--build-name-from-url url)
                     (error "Failed to get a name from the URL")))))
  (let ((dpath (expand-file-name name play-directory)))
    (if (file-exists-p dpath)
        (progn (message (format "%s already exists" dpath))
               nil)
      (condition-case nil
          (progn
            (make-directory dpath t)
            (process-lines "git" "clone" "--recursive" "--depth=1"
                           url
                           (expand-file-name ".emacs.d" dpath))
            (play--update-symlinks dpath)
            name)
        (error (progn (delete-directory dpath t)
                      (error "failed")))))))

;;;###autoload
(defun play-update-symlinks ()
  (interactive)
  (mapc #'play--update-symlinks
        (directory-files play-directory t "^\[^.\]")))

(defvar play-last-config-home)

;;;###autoload
(defun play-try ()
  (interactive)
  (let* ((candidates (directory-files play-directory
                                      nil
                                      "^\[^.\]"))
         (name-or-url (completing-read "Choose a configuration: " candidates))
         (name (if (member name-or-url candidates)
                   name-or-url
                 (play-add name-or-url)))
         (home (expand-file-name name play-directory))
         (process-environment (cons (concat "HOME=" home)
                                    process-environment)))
    (start-process "play"
                   (format "*play %s*" name)
                   (play--emacs-executable))
    (setq play-last-config-home home)))

;;;###autoload
(defun play-adopt ()
  (interactive)

  (unless (boundp 'play-last-config-home)
    (error "No play instance has been run yet"))

  (let ((home play-last-config-home)
        (emacs-bin (play--emacs-executable)))
    (destructuring-bind
        (wrapper unwrapper) (play--script-paths)
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
(defun play-dismiss ()
  (interactive)
  (mapc 'delete-file (play--script-paths)))

(provide 'play)

;;; play.el ends here
