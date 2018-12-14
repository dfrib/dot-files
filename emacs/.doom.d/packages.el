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
(package! lsp-ui)
(package! spinner)                      ; required by lsp-mode
;(package! lsp-python)

;; misc
(package! ag)
(package! resize-window)
(package! clang-format)
