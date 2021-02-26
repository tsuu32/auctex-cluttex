# auctex-cluttex.el

[![MELPA](https://melpa.org/packages/auctex-cluttex-badge.svg)](https://melpa.org/#/auctex-cluttex)
[![Build Status](https://travis-ci.org/tsuu32/auctex-cluttex.svg?branch=master)](https://travis-ci.org/tsuu32/auctex-cluttex)

This package provides [ClutTeX](https://www.ctan.org/pkg/cluttex) support for
[AUCTeX](https://www.gnu.org/software/auctex/) package.

## Incompatible change
* 2021/02/26: `auctex-cluttex-mode` is now buffer-local (not global) minor mode. Check setup below and modify your settings.

## Features
- Run `cluttex` command with `C-c C-c ClutTeX` (`M-x TeX-command-master ClutTeX`)
  - with suitable command-line options (e.g. `-e pdflatex`, `-e uplatex --bibtex=upbibtex`, ...)
  - with `--synctex=1` option if `TeX-source-correlate-mode` is enabled.
- Colorize `cluttex` log buffer (which can be seen with `C-c C-l` (`M-x TeX-recenter-output-buffer`))

## Requirements
Make sure that you install these requirements.

* Emacs 24.4 or higher
* AUCTeX 12.2 or higher
* ClutTeX 0.4 or higher

## Installation
### From MELPA
`M-x package-install auctex-cluttex` to install auctex-cluttex from [MELPA](https://melpa.org).

### Manually
Add the following to your init file:

```elisp
(add-to-list 'load-path "/path/to/auctex-cluttex/") ; add auctex-cluttex.el's directory to the load-path
(require 'auctex-cluttex)                           ; load auctex-cluttex
```

## Setup
To use this package, add following code to your init file:

```elisp
(add-hook 'plain-TeX-mode-hook
          #'auctex-cluttex-mode)
(add-hook 'LaTeX-mode-hook
          #'auctex-cluttex-mode)
```

You can use ClutTeX as default command of `C-c C-c` (`M-x TeX-command-master`).

## Sample Configuration
```elisp
(require 'tex-site)
(with-eval-after-load 'tex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (TeX-source-correlate-mode))
(add-hook 'plain-TeX-mode-hook
          #'auctex-cluttex-mode)
(add-hook 'LaTeX-mode-hook
          #'auctex-cluttex-mode)
```


Enjoy `C-c C-c ClutTeX`!
