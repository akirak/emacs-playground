;;; test-helper.el --- Helpers for emacs-pg-test.el

(unless (require 'emacs-pg nil 'noerror)
  (message "attempting to load emacs-pg.el")
  (load-file "./emacs-pg.el")
  )

;;; test-helper.el ends here
