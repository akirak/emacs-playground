;; Install straight.el
;; Just pasted from https://github.com/raxod502/straight.el#getting-started
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; You have to install use-package in order to use use-package with straight
(straight-use-package 'use-package)

(use-package play
 :straight (play :type git :host github :repo "akirak/play.el")
 :commands (play-checkout
            play-start-last
            play-adopt
            play-dismiss
            play-update-symlinks)
 )

;; Select a configuration at startup
(play-checkout)

;; Display a tutorial in the window
(setq inhibit-startup-screen t)
(let ((buf (get-buffer-create  "*play-start*")))
  (with-current-buffer buf
      (insert "Run 'play-checkout' to start Emacs with another config, and 'play-adopt' to replace Emacs"))
  (switch-to-buffer buf))
