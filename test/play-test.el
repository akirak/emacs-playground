;;; emacs-pg-test.el --- Tests for emacs-pg

(require 'play)

(ert-deftest parse-github-url-test ()
  ;; Parse a HTTPS URL
  (should (equal (play--parse-github-url "https://github.com/akirak/.spacemacs.d.git")
                 "akirak/.spacemacs.d"))
  ;; Parse a HTTPS URL without '.git' suffix
  (should (equal (play--parse-github-url "https://github.com/akirak/.spacemacs.d")
                 "akirak/.spacemacs.d"))
  ;; Parse a SSH URL
  (should (equal (play--parse-github-url "git@github.com:akirak/spacemacs-config.git")
                 "akirak/spacemacs-config"))
  ;; Parse a URL including upper-case letters and numbers
  (should (equal (play--parse-github-url "https://github.com/Capitalise/abc123")
                 "Capitalise/abc123"))
  ;; Return nil for other cases
  (should (null (play--parse-github-url "https://bitbucket.org/myaccount/repo.git")))
  (should (null (play--parse-github-url "local/relative/path.git")))
  (should (null (play--parse-github-url "/absolute/path-to-file")))
          )

(ert-deftest github-repo-path-p-test ()
  (should (play--github-repo-path-p "User0123/Repo-Name.el"))
  (should (play--github-repo-path-p "organization-name/org_Repo-Name"))
  (should (not (play--github-repo-path-p "~/.emacs.d/name")))
  (should (not (play--github-repo-path-p "/usr/bin")))
  (should (not (play--github-repo-path-p "/tmp")))
  (should (not (play--github-repo-path-p "relative/path/to/somewhere/deep")))
  (should (not (play--github-repo-path-p "git@github.com:user/repo.git")))
  (should (not (play--github-repo-path-p "https://github.com/user/repo")))
  )

(ert-deftest github-repo-path-to-https-url-test ()
  (should (equal (play--github-repo-path-to-https-url "my/repo")
  "https://github.com/my/repo.git")))

(ert-deftest git-url-p-test ()
  "Test the function to check if a string is a URL recognized by Git"
  (should (play--git-url-p "git@github.com:user/repo.git"))
  (should (play--git-url-p "https://github.com/user/repo.git"))
  (should (play--git-url-p "https://github.com/user-123/Repo123"))
  (should (not (play--git-url-p "apparently this is not a URL"))))

;;; emacs-pg-test.el ends here
