# git-proj

## Introduction

This is a (really) small package for Emacs to help working with Git projects. Currently it contains two functions: git-proj-grep, and git-proj-goto-file.

### Usage

#### git-proj-goto-file

Function tries to open file which is referenced by string under cursor. If the string starts with "/" it is assumed to be absolute path and the file is just opened. Otherwise "git ls-files" is called. If perfect match is found that file is opened, otherwise matching files are shown to user in a new buffer with absolute paths (user can then call function again and since now path is absolute file is opened without questions)

#### git-proj-grep

Function calls "git grep" with string under cursor and shows the result in a new buffer. User can then call git-proj-goto-file to open match.

### How to use

Clone the repo and add it to git library search path and require git-proj.

The root of the project can be given with variable git-proj-root, if that is undefined the folder in which emacs was started is assumed to be git repo.

Example .emacs configuration with project root set to "/home/user/projects/test-proj/" and functions bind to keys:

    (add-to-list 'load-path "/home/user/.emacs.d/")
    (require 'git-proj)
    (setq git-proj-root "/home/user/projects/test-proj/")
    (global-set-key (kbd "M-g M-g") 'git-proj-grep)
    (global-set-key (kbd "M-g M-f") 'git-proj-goto-file)

Currently tested only on Emacs 24.3.