# emacs-pg (Emacs Playground)

emacs-pg runs Emacs with a sandboxed home directory from within Emacs. It creates a new directory, initialize `.emacs.d` in the sandbox from a remote Git repository, and starts Emacs with the sandbox as `$HOME` directory. This allows you to easily experiment with various Emacs configuration repositories available on GitHub, while keeping your current configuration with (almost) no modification. It can also simplify your workflow in Emacs, as it allows you to create a sandboxed Emacs environment where only selected items are visible under `~`. 

## Installation

You can use `(emacs-pg :fetcher github :repo "akirak/emacs-pg")` recipe to install the package.

If you use use-package:

```emacs-lisp
(use-package emacs-pg)
```

If you use [straight.el](https://github.com/raxod502/straight.el) with use-package:

```emacs-lisp
(use-package emacs-pg
 :straight (emacs-pg :type git :host github :repo "akirak/emacs-pg")
 :defer t
 :commands (emacs-pg-try
            emacs-pg-add
            emacs-pg-adopt
            emacs-pg-dismiss
            emacs-pg-update-symlinks)
 )
```

This repository also bundles an example Emacs configuration file which allows you to try pre-configured repositories:

```shell
curl https://raw.githubusercontent.com/akirak/emacs-pg/master/init.el > ~/.emacs
```

## Configuration

By default, emacs-pg installs executable scripts to `~/.local/bin`. If you haven't add this directory to `$PATH`, you can change the installation target by setting `emacs-pg-script-directory` variable:

```emacs-lisp
;;; This should be a directory in your $PATH
(defcustom emacs-pg-script-directory (expand-file-name "~/bin"))
```

## Usage

To try a configuration repository, follow these steps:

1. Run `emacs-pg-try` command and enter a URL to the Git repository.
2. Emacs is started with a sandbox home directory with the repository as `~/.emacs.d`. Try it.
3. If you like it, run `emacs-pg-adopt` to set the home directory of Emacs to the sandbox. 

After you run `emacs-pg-adopt`, Emacs always start with the sandbox directory as `$HOME`. This is done by creating a wrapper script which has the same name as Emacs. If you want to run Emacs with your original/real home directory and `~/.emacs.d`, you can use `emacs-nopg` script. 

You can uninstall these wrapper scripts by running `emacs-pg-dismiss` command in Emacs where emacs-pg is available. 

### Access files in your home directory

emacs-pg starts Emacs with a sandboxed home directory. `~` in Emacs points to the sandbox, and not the original home directory, so your data files cannot be accessed from `~` in Emacs. You can still visit the original home directory from `~USER` (`USER` is your login name), but this is inconvenient. 

To address this issue, emacs-pg has a feature of automatically creating symbolic links from created sandboxes to contents in the original home directory. To produce symbolic links when you create a new sandbox, set `emacs-pg-inherited-contents` variable:

```emacs-lisp
(setq emacs-pg-inherited-contents
      '("Dropbox" ".gpg" "git"))
```

Each item of the variable should be a relative path to the target from the home directory. In the above example, emacs-pg creates the following three symbolic links when it creates a new sandbox:

- `SANDBOX/Dropbox` to `~USER/Dropbox` 
- `SANDBOX/.gpg` to `~USER/.gpg` 
- `SANDBOX/git` to `~USER/git` 

You can also use `add-to-list`:

```emacs-lisp
(add-to-list 'emacs-pg-inherited-contents "Dropbox" 'append)
```

After you update the configuration, you can use `emacs-pg-update-symlinks` to create missing symbolic links in existing sandboxes. 

## How it works

- `emacs-pg-try` command starts Emacs with a sandbox home directory. You can choose from previously created sandboxes or enter a new repository URL for `~/.emacs.d`. 
- `emacs-pg-add` command creates a sandbox directory with a given repository as `~/.emacs.d`. It takes the following steps:
  1. Create the directory for a new sandbox
  2. Clone the given remote repository into `.emacs.d` directory in the sandbox
  3. Create symbolic links to contents in the original home directory from the sandbox, as configured in `emacs-pg-inherited-contents` variable
- `emacs-pg-adopt` command creates a wrapper script running Emacs with the last sandbox directory (selected by `emacs-pg-try`) as `$HOME`. The name of the created script has the same name as Emacs (usually `emacs`), so it virtually replaces Emacs with the sandbox version. It also creates another script named `emacs-nopg`, which runs Emacs without modifying `HOME` variable. 
- `emacs-pg-dismiss` command deletes the wrapper scripts. 

## License

[GPL](LICENSE.txt)
