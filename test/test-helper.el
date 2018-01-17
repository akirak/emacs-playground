;;; test-helper.el --- Helpers for emacs-pg-test.el

(unless (require 'playground nil 'noerror)
  (message "attempting to load playground.el")
  (load-file "./playground.el")
  )

;;; test-helper.el ends here
