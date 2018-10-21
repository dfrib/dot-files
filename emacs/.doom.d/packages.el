;;; ~/.doom.d/packages.el -*- no-byte-compile: t; -*-
(disable-packages! cmake-mode
                   company-irony
                   company-irony-c-headers
                   flycheck-irony
                   irony
                   irony-eldoc
                   ivy-rtags
                   rtags)

;; lsp and ccls
(package! ccls)
(package! company-lsp)
(package! lsp-mode)
(package! lsp-ui)
(package! lsp-python)

;; misc
(package! ag)
(package! resize-window)
