# A Emacs config for personal use
This is my emacs configuration for personal use. It is based on [Purcell's emacs.d](https://github.com/purcell/emacs.d) and [Prelude](https://github.com/bbatsov/prelude).

1. Mainly for C++ and Python development.
2. Support for markdown and org-mode.
3. If you have Github Copilot account, you can use it in Emacs, Copilot is enabled for c++-mode, python-mode, and gfm-mode(markdown).
4. Wslg support, it will use [emacs-rime](https://github.com/DogLooksGood/emacs-rime).
5. Magit and Projectile are enabled by default.
6. Font ligatures is enabled default.
7. Use tuna mirror for melpa.

## Installation
1. `git clone https://github.com/luozikuan/emacs.d.git ~/.emacs.d`
2. Github Copilot
   - Ensure `node` is installed on your system.
   - `git clone https://github.com/zerolfx/copilot.el ~/.emacs.d/site-lisp/copilot.el`
3. If you want to use rime in wslg, make sure `fcitx5` is configured correctly.
