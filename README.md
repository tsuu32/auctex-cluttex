# auctex-cluttex.el

This package provides [ClutTeX](https://www.ctan.org/pkg/cluttex) support for
[AUCTeX](https://www.gnu.org/software/auctex/) package.

## Features
#### `M-x TeX-command-master ClutTeX` (`C-c C-c ClutTeX`)
Run `cluttex` with suitable arguments.

#### Colorized output buffer
ClutTeX output buffer seen by `M-x TeX-recenter-output-buffer` (`C-c C-l`) is colorized.

#### `--synctex=1` option
Run `cluttex` with `--synctex=1` option if `TeX-source-correlate-mode` is enabled.

## Requirements
Make sure that you install these requirements.

* Emacs 24.4 or higher
* AUCTeX 12.2 or higher
* ClutTeX 0.4 or higher

## Setup
To use this package, add following code to your init file.

```elisp
(with-eval-after-load 'tex
  (require 'auctex-cluttex))
```

If you want to use ClutTeX as default command of `M-x TeX-command-master` (`C-c C-c`), 
add following code to your init file.

```elisp
(add-hook 'plain-TeX-mode-hook
          #'auctex-cluttex-set-command-default)
(add-hook 'LaTeX-mode-hook
          #'auctex-cluttex-set-command-default)
```

## Sample Configuration
```
(require 'tex-site)
(with-eval-after-load 'tex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (TeX-source-correlate-mode)
  (require 'auctex-cluttex))
(add-hook 'plain-TeX-mode-hook
          #'auctex-cluttex-set-command-default)
(add-hook 'LaTeX-mode-hook
          #'auctex-cluttex-set-command-default)
```


Enjoy `C-c C-c ClutTeX`!
