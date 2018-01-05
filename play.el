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
      "List of configuration repositories suggested in play-checkout.")

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

(defun play--directory (name)
  "Get the path of a sandbox named NAME."
  (expand-file-name name play-directory))

;;;###autoload
(defun play-update-symlinks ()
  (interactive)
  (mapc #'play--update-symlinks
        (directory-files play-directory t "^\[^.\]")))

(defvar play-last-config-home)

(defun play--process-buffer-name (name)
  (format "*play %s*" name))

(defun play--start (name home)
  (let ((process-environment (cons (concat "HOME=" home)
                                   process-environment)))
    (start-process "play"
                   (play--process-buffer-name name)
                   (play--emacs-executable))
    (setq play-last-config-home home)))

;; TODO: Add support for Helm
(defun play--config-selector (installed available-alist)
  (completing-read "Choose a configuration: "
                   (remove-duplicates (append installed
                                              (mapcar 'car available-alist))
                                      :test 'equal)))

(defun play--select-config (available-alist)
  (let* ((installed-list (directory-files play-directory nil "^\[^.\]"))
         (inp (play--config-selector installed-list available-alist))
         (installed (member inp installed-list))
         (available (assoc inp available-alist)))
    (cond
     (installed (let ((name inp))
                  (play--start name (play--directory name))))
     (available (apply 'play--start-with-dotemacs available))
     ((play--git-url-p inp) (let ((name (read-from-minibuffer "Name for the config: "
                                                              (play--build-name-from-url inp))))
                              (play--start-with-dotemacs name :repo inp)))
     (t (error (format "Doesn't look like a repository URL: %s" inp))))))

(defun play--git-url-p (s)
  t ; FIXME
  )

(cl-defun play--initialize-sandbox (name url
                                         &key
                                         (recursive t)
                                         (depth 1))
  (condition-case nil
      (progn
        (setq dpath (play--directory name))
        (make-directory dpath t)
        (apply 'process-lines
               (remove nil (list "git" "clone"
                                 (when recursive "--recursive")
                                 (when depth
                                   (concat "--depth="
                                           (cond ((stringp depth) depth)
                                                 ((numberp depth) (int-to-string depth)))))
                                 url
                                 (expand-file-name ".emacs.d" dpath)))
               )
        (play--update-symlinks dpath)
        dpath)
      (error (progn (messsage (format "Cleaning up %s..." dpath))
                    (delete-directory dpath t)
                    (error (error-message-string err))))))

(cl-defun play--start-with-dotemacs (name
                                     &rest other-props
                                     &key repo &allow-other-keys)
  (when (null repo)
    (error "play--start-with-dotemacs: You must path :repo to this function"))
  (let ((url (if (play--github-repo-path-p repo)
                 (play--github-repo-path-to-https-url repo)
               repo)))
    (play--start name
                 (apply 'play--initialize-sandbox
                        name url
                        (cl-remprop 'repo other-props)))))

;;;###autoload
(defun play-checkout (name)
  (interactive "P")

  (make-directory play-directory t)

  (pcase (and name (play--directory name))

    ;; NAME refers to an existing sandbox
    (`(and (pred file-directory-p)
           ,dpath)
     (play--start name dpath))

    ;; Otherwise
    ('nil
     ;; Build an alist from play-dotemacs-list
     (let ((alist (cl-loop for plist in play-dotemacs-list
                           collect (cons (or (plist-get plist :name)
                                             (play--build-name-from-url (plist-get plist :repo)))
                                         plist))))
       (if (null name)
           (play--select-config alist)
         (pcase (assoc name alist)
           ('nil (error (format "Config named %s does not exist in play-dotemacs-list"
                                name)))
           (pair (apply 'play--start-with-dotemacs pair))))))))

;;;###autoload
(defun play-start-last ()
  (interactive)
  (pcase (and (boundp 'play-last-config-home)
              play-last-config-home)
    ('nil (error "Play has not been run yet. Run 'play-checkout'"))
    (home (let* ((name (file-name-nondirectory home))
                 (proc (get-buffer-process (play--process-buffer-name name))))
            (if (and proc (process-live-p proc))
                (when (yes-or-no-p (format "%s is still running. Kill it? " name))
                  (lexical-let ((sentinel (lambda (process event)
                                            (play--start name home))))
                    (set-process-sentinel proc sentinel)
                    (kill-process proc)))
              (play--start name home))))))

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
