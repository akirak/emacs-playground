# Playground for Emacs

[![Build Status](https://travis-ci.org/akirak/emacs-playground.svg?branch=master)](https://travis-ci.org/akirak/emacs-playground)

The basic idea behind Playground is to create an isolated directory called a sandbox and make it `$HOME` of Emacs. Playground allows you to easily experiment with various Emacs configuration repositories available on GitHub, while keeping your current configuration untouched (almost, except for a stuff for Playground). It can also simplify your workflow in Emacs by hiding irrelevant files and directories existing in your home directory. 

For details on the mechanism of Playground, see [How it works](#how-it-works) section.

## Features

- Two ways to checkout a sandbox: declarative configuration and interactively specifying a Git repository
- Pre-configured with some popular and notable Emacs configuration repositories. Also including my configuration, which is very minimal at present
- Support producing symbolic links for selectively mapping a directory structure between sandboxes and the home directory
- Support producing a wrapper script to replace Emacs for regular use of a sandbox

## Prerequisites

- Emacs (>= 24.4 && < 26.0)
- Git
- UNIX-like operating system with a graphical environment (not on terminal)
  - On Windows, Playground cannot generate a convenient shell script, but it still allows you to try a configuration

## Installation

Playground is available on MELPA as `playground` package.

An alternative way is to clone this repository to somewhere and start Emacs using this command: 

    emacs -Q -l playground.el

## Quick start

    M-x playground-checkout

Then select a configuration in the minibuffer.

## Configuration

These settings are mostly for users who might want to use a sandbox regularly. If you just want to try Emacs configurations for a short period of time, you can skip this section and proceed to [Usage](#usage).

### Configuration repositories

To add configuration repositories suggested in `playground-checkout`, set `playground-dotemacs-list` variable. 

### Location of wrapper scripts

By default, Playground installs wrapper scripts to `~/.local/bin` when you run `playground-persist`. If this directory is not included in your `$PATH`, you can change the destination by setting `playground-script-directory`:

```emacs-lisp
(setq playground-script-directory (expand-file-name "~/bin")) ; Install scripts to ~/bin
```

### Symbolic links

By default, Playground creates a symbolic link in sandboxes only for `~/.gnupg`. You can produce more symbolic links by setting `playground-inherited-contents`:

```emacs-lisp
(setq playground-inherited-contents '("Dropbox" ".gpg" "git"))
```

Each item in the list should be a relative path from the home directory. In the above example, the following three symbolic links are created (`~/` is the sandbox context):

- `~/Dropbox` to `~USER/Dropbox` 
- `~/.gpg` to `~USER/.gpg` 
- `~/git` to `~USER/git` 

These symbolic links are produced when a new sandbox is created. To update the mappings in previously created sandboxes, run `playground-update-symlinks` command. This command creates missing symbolic links in existing sandboxes. 

## Usage

The main entry point to using Playground is `playground-checkout` command. It allows you to check out a sandbox from the following sources:

- Existing sandboxes that you have checked out on the machine (via selection)
- A predefined list of sandbox configurations (via selection)
- Any Git repository that should be used as `~/.emacs.d` in the new sandbox context (by typing a URL in the mini buffer)

If you enter a URL, Playground asks you a name for the created sandbox. It can be any file name. 

After you specify an existing sandbox or a sandbox configuration in `playground-checkout`, Playground initializes the sandbox if it is not created yet and starts Emacs with the sandbox as its home. You can try it. You can also restart the last selected sandbox by `playground-start-last` command. 

### Replacing your Emacs

If you come to like a particular configuration and want to use it regularly, you can make it the default by running `playground-persist` command in the parent Emacs session. It creates a wrapper script that effectively replaces your current Emacs configuration. The following is how it works: The script will have the same name as Emacs (normally `emacs`), and it starts Emacs in the sandbox environment. Granted that this script is installed into a directory that have a higher precedence in  `$PATH`, `emacs` command will always run Emacs on the sandbox. 

In case you occasionally run Emacs in your original environment, Playground also creates `emacs-noplay` wrapper script. It starts Emacs on the original directory. 

If you don't like the configuration you have set as the default, you can roll back this change by running `playground-return` command. The wrapper scripts will be deleted, and Emacs will run on the original home directory in all of its succeeding sessions. 

## How it works

The fundamental idea for Playground is to start Emacs with `$HOME` environment variable set to a different location:

    HOME=/foo/bar emacs

This command line starts Emacs with `/foo/bar` as its home directory (`~`). Emacs started in this way loads an initialization file from `/foo/bar/.emacs.d/init.el` (or `/foo/bar/.emacs[.el]`, etc.) rather than from `~USER/.emacs.d` (`~USER` means the home directory of USER, which is usually `/home/USER`). You can use this technique to try a configuration repository created by other people. 

This technique causes another effect of 'hiding' a bunch of files and directories located in the original home directory. If you use Emacs alongside other desktop applications, your home directory is likely to contain files that will never be edited or browsed in Emacs. This can be a mess when you use Emacs. However, in an Emacs session in the modified home environment variable, `~` points to `/foo/bar`. This applies to every situation inside the modified Emacs session, including caching downloaded packages, locating recent files and backup files, and interactive interfaces like `find-file`. The modified home environment virtually serves as a sandbox for Emacs. 

Meanwhile, you can still visit files and directories in the original home directory . In `find-file`, you can browse files in your original home directory by typing `~USER/` (`USER` should be your login name) even on the modified environment. The same applies to other commands. You can save typing efforts for specific directories by creating symbolic links from inside the sandbox to corresponding locations in the original home directory, e.g. `~/path` in the sandbox to `~USER/path`.

Playground helps you manage sandboxes of this kind. It creates a sandbox in the following steps:

1. Create a new directory inside a fixed location (`~USER/.emacs-play` by default). This will become your sandbox, a virtual `~` for Emacs. 
2. Clone a given Git repository to `~/.emacs.d` in the sandbox context. You can specify any Git repository as your Emacs configuration for the sandbox. 
3. Create symbolic links from inside the sandbox to directories in the original home directory. This configuration is done globally, so you can enforce the same structure in all of your sandboxes. 

## Thanks

- [alphapapa](https://github.com/alphapapa) for an advice on the project name
- [purcell](https://github.com/purcell) for contribution

## License

[GPL v3](LICENSE.txt)
