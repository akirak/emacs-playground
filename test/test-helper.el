;;; test-helper.el --- Helpers for emacs-pg-test.el

(unless (require 'play nil 'noerror)
  (message "attempting to load play.el")
  (load-file "./play.el")
  )

;;; test-helper.el ends here
