# git-proj

## Introduction

Git-proj is a (really) small package for Emacs to help working with Git projects. No configuration is required, but maybe provided. Currently package contains three functions: git-proj-grep, git-proj-goto-file, and git-proj-goto-other

### Functions

#### git-proj-goto-file

Function tries to open file which is referenced by string under cursor. If the string starts with "/" it is assumed to be absolute path and the file is just opened. Otherwise "git ls-files" is called. If perfect match is found that file is opened, otherwise matching files are shown to user in a new buffer with absolute paths (so user can then call this function again and since now the path is absolute file is opened without questions)

#### git-proj-grep

Function calls "git grep" with string under cursor and shows the result in a new buffer. User can then call git-proj-goto-file to open match.

#### git-proj-goto-other

Function tries to open the "other" file, e.g. if current file is "name.h" function tries to open "name.c". Matching files are searched with git ls-files. If only single match is found file is opened without questions, if multiple matches are found those are listed in a new buffer in which user can then use git-proj-goto-file to open wanted file. Default rules how to form filename for the other are listed in git-proj-default-goto-other-rules variable, user can customize those by setting value for git-proj-goto-other-rules in .emacs.

### How to use

Clone the repo, add its location to Emacs library search path, add require git-proj to .emacs.

The root of the project can be given with variable git-proj-root, if that is undefined the folder in which Emacs was started is assumed to be git repo.

Example .emacs configuration with project root set to "/home/user/projects/test-proj/" and functions bind to keys:

    (require 'git-proj)
    (setq git-proj-root "/home/user/projects/test-proj/")
    (global-set-key (kbd "M-g M-g") 'git-proj-grep)
    (global-set-key (kbd "M-g M-f") 'git-proj-goto-file)
    (global-set-key (kbd "M-g M-o") 'git-proj-goto-other)

Currently tested on Emacs 24.3.