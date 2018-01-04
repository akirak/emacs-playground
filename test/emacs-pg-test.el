;;; emacs-pg-test.el --- Tests for emacs-pg

(require 'emacs-pg)

(ert-deftest emacs-pg--parse-github-url-test ()
  ;; Parse a HTTPS URL
  (should (equal (emacs-pg--parse-github-url "https://github.com/akirak/.spacemacs.d.git")
                 "akirak/.spacemacs.d"))
  ;; Parse a HTTPS URL without '.git' suffix
  (should (equal (emacs-pg--parse-github-url "https://github.com/akirak/.spacemacs.d")
                 "akirak/.spacemacs.d"))
  ;; Parse a SSH URL
  (should (equal (emacs-pg--parse-github-url "git@github.com:akirak/spacemacs-config.git")
                 "akirak/spacemacs-config"))
  ;; Parse a URL including upper-case letters and numbers
  (should (equal (emacs-pg--parse-github-url "https://github.com/Capitalise/abc123")
                 "Capitalise/abc123"))
  ;; Return nil for other cases
  (should (null (emacs-pg--parse-github-url "https://bitbucket.org/myaccount/repo.git")))
  (should (null (emacs-pg--parse-github-url "local/relative/path.git")))
  (should (null (emacs-pg--parse-github-url "/absolute/path-to-file")))
          )

;;; emacs-pg-test.el ends here
