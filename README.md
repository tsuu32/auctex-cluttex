# auctex-cluttex.el

[![MELPA](https://melpa.org/packages/auctex-cluttex-badge.svg)](https://melpa.org/#/auctex-cluttex)

This package provides [ClutTeX](https://www.ctan.org/pkg/cluttex) support for
[AUCTeX](https://www.gnu.org/software/auctex/) package.

## Features
- Run `cluttex` command with `C-c C-c ClutTeX` (`M-x TeX-command-master ClutTeX`)
  - with suitable command-line options (e.g. `-e pdflatex`, `-e uplatex --bibtex=upbibtex`, ...)
  - with `--synctex=1` option if `TeX-source-correlate-mode` is enabled.
- Colorized `cluttex` compile log (which can see with `C-c C-l` (`M-x TeX-recenter-output-buffer`))

## Requirements
Make sure that you install these requirements.

* Emacs 24.4 or higher
* AUCTeX 12.2 or higher
* ClutTeX 0.4 or higher

## Setup
To use this package, add following code to your init file.

```elisp
(with-eval-after-load 'tex
  (auctex-cluttex-mode))
```

If you want to use ClutTeX as default command of `C-c C-c` (`M-x TeX-command-master`), 
add following code to your init file.

```elisp
(add-hook 'plain-TeX-mode-hook
          #'auctex-cluttex-set-command-default)
(add-hook 'LaTeX-mode-hook
          #'auctex-cluttex-set-command-default)
```

## Sample Configuration
```elisp
(require 'tex-site)
(with-eval-after-load 'tex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (TeX-source-correlate-mode)
  (auctex-cluttex-mode))
(add-hook 'plain-TeX-mode-hook
          #'auctex-cluttex-set-command-default)
(add-hook 'LaTeX-mode-hook
          #'auctex-cluttex-set-command-default)
```


Enjoy `C-c C-c ClutTeX`!
