;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; This config is heavily influenced by the non-evil DOOM Emacs config of
;; UndeadKernel: https://github.com/UndeadKernel/emacs_doom_private
;; -------------------------------------------------------------------------- ;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "David Friberg"
      user-mail-address "david.n.friberg@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic behaviour and appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show trailing spaces
(setq-default show-trailing-whitespace t)

;; Disable trailing whitespaces in the minibuffer
(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; Set tabs to indent as white spaces and set def. tab width to 4 white spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Minimalistic Emacs at startup
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode nil)

;; Maximize first frame
(set-frame-parameter nil 'fullscreen 'maximized)

;; File names relative to project (not root)
(setq +doom-modeline-buffer-file-name-style 'relative-from-project)

;; Don't ask when killing emacs
(setq confirm-kill-emacs nil)

;; Resize windows interactively.
;(def-package! resize-window
;  :commands (resize-window))

;; Reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)

;; Smooth mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; scroll two lines at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      mouse-wheel-follow-mouse t                    ; scroll window under mouse
      scroll-step 1)

;; Do not automatically copy selected text.
(setq select-enable-primary nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overall theme & visual behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Font setup
(setq doom-font (font-spec :family "Meslo LG M DZ for Powerline" :size 14)
      doom-variable-pitch-font (font-spec :family "Meslo LG M DZ for Powerline")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Meslo LG M DZ for Powerline" :size 16))

;; (setq doom-font (font-spec :family "Meslo LG M DZ for Powerline" :size 20)
;;       doom-variable-pitch-font (font-spec :family "Meslo LG M DZ for Powerline")
;;       doom-unicode-font (font-spec :family "DejaVu Sans Mono")
;;       doom-big-font (font-spec :family "Meslo LG M DZ for Powerline" :size 24))



;; All themes are safe to load
;(setq custom-safe-themes t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-vibrant)
;;(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-nova)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-sourcerer)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;(setq display-line-numbers-type t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! smartparens
  (smartparens-global-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persp / workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default +workspaces-switch-project-function #'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq projectile-enable-caching nil)
;; (setq projectile-project-compilation-cmd "./run.py")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PlantUML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package plantuml-mode
;;   :mode "\\.plantuml\\'"
;;   :config (setq plantuml-jar-path
;;                 (expand-file-name "~/opensrc/plantuml/plantuml.jar"))
;; )
(after! plantuml-mode
  (setq plantuml-jar-path
                (expand-file-name "~/opensrc/plantuml/plantuml.jar"))
  )

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; (add-to-list 'auto-mode-alist '("Dockerfile.*\\" . plantuml-mode))

;; Jinja2
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))

;; Matlab
;; (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;; (add-to-list 'auto-mode-alist '("\\.m\\" . matlab-mode))
 ;; (setq matlab-indent-function t)
 ;; (setq matlab-shell-command "matlab")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode and org-capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; org config and capture templates
;; (after! org
;;   (setq org-startup-folded nil ; do not start folded
;;         org-tags-column -100 ; the column to the right to align tags
;;         org-log-done 'time ; record the time when an element was marked done/checked
;;         org-fontify-done-headline nil ; do not change the font of DONE items
;;         org-ellipsis " ↴ "
;;         org-bullets-mode 1
;;         org-directory "~/org"
;;         org-latex-caption-above nil)


;; automatically redisplay images generated by babel
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; So that ox-hugo export options are available in the Org Export Dispatcher
;; menu
;; (with-eval-after-load 'ox
;;   (require 'ox-hugo))
(after! ox
  (require 'ox-hugo))

;; Add a an option to use lines to frame the exported code block.
;; (setq org-latex-minted-options '(("frame" "lines")))

;;
(setq org-babel-python-command "python3")

;; ;; Org theme customization
;; (add-hook! org-mode
;;   ;; Document title font
;;   (set-face-attribute 'org-document-title nil :height 2.0)
;;   ;; Face of keyword DONE (Green like strings)
;;   (set-face-attribute 'org-done nil :foreground "#98be65")
;;   ;; Face of keyword TODO or [ ] (Purple like keywords)
;;   (set-face-attribute 'org-todo nil :foreground "#c678dd")
;;   ;; Face of ellipsis symbol (Purple like keywords)
;;   (set-face-attribute 'org-ellipsis nil :foreground "#c678dd")
;;   ;; Face of the entire headline of a DONE line
;;   (set-face-attribute 'org-headline-done nil :foreground nil)
;;   (cond
;;    ;; If doom-one theme is enabled
;;    ((custom-theme-enabled-p 'doom-one)
;;       ;; Change the style of the BEGIN_SRC and RESULT blocks
;;       (set-face-attribute 'org-block-begin-line nil
;;                           :background "#4c516d"
;;                           :foreground "#979aaa"
;;                           :weight 'normal
;;                           :height 0.9
;;                           :box '(:line-width 2 :color "#4c516d")))
;;    ;; For other themes, disable the changes
;;    (t
;;       (set-face-attribute 'org-block-begin-line nil
;;                           :background unspecified
;;                           :foreground unspecified
;;                           :weight unspecified
;;                           :height unspecified
;;                           :box unspecified
;;                           :inherit org-meta-line)))

(after! org
  ;; Point to plantuml jar.
  (setq org-plantuml-jar-path
        (expand-file-name "~/opensrc/plantuml/plantuml.jar"))
  ;; Display images inline in org document.
  ;(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal variables?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO(dfrib): consider adding a +private.el file which will not be under
;; public version control.
;; (load! "+private" nil t) ; do not complain if file does not exist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select popup buffers by default
;(setq +popup-defaults
;  (list :side   'bottom
;        :height 0.16
;        :width  40
;        :quit   t
;        :select t
;        :ttl    5))

;; TODO(dfrib): See if I really feel that I need this customization?
;; Select the IList buffer when it is shown
;(after! imenu-list
;  (set-popup-rule! "^\\*Ilist"
;    :side 'right :size 35 :quit nil :select t :ttl 0))

;; Larger undo tree window
;(after! undo-tree
;  (set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'right :size 40 :modeline nil :select t :quit t))

;; Larger org src edit
;(after! org
;  (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet file templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom file templates
;; Just place them in ~/.doom.d/snippets - these will take precedence over the
;; default ones.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and caching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq make-backup-files t
;      backup-by-copying t
;      delete-old-versions t
;      kept-new-versions 6
;      kept-old-versions 2
;      version-control t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(after! flycheck
;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;  )

;; Let flycheck search for required files in the `load-path' and the current folder.
;(setq flycheck-emacs-lisp-load-path '("./"))

;; disable using hooks
;; (add-hook 'text-mode-hook (lambda ()
;;                             (flycheck-mode -1)))
;; (add-hook 'org-mode-hook (lambda ()
;; (flycheck-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        ))

(use-package! company-lsp
  :after lsp-mode
  :config
  (setq company-transformers nil company-lsp-cache-candidates nil)
  (set-company-backend! 'lsp-mode 'company-lsp)
  )

(after! flycheck
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (global-flycheck-mode -1)
  )

;; LSP-UI
;;https://github.com/MaskRay/Config
(use-package! lsp-ui
  ;:load-path "~/Dev/Emacs/lsp-ui"
  :commands lsp-ui-mode
  :config
  (setq
   ;; Disable sideline hints
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t
   ;; Disable imenu
   lsp-ui-imenu-enable nil
   ;; Disable ui-doc (already present in minibuffer)
   lsp-ui-doc-enable nil
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature nil
   lsp-ui-doc-background (doom-color 'base4)
   lsp-ui-doc-border (doom-color 'fg)
   ;; Enable ui-peek
   lsp-ui-peek-enable t
   ;lsp-ui-peek-fontify t
   lsp-ui-peek-always-show t
   lsp-ui-peek-force-fontify nil
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs))
   ;; Flycheck
   lsp-ui-flycheck-enable t
   )

  (custom-set-faces
   '(ccls-sem-global-variable-face ((t (:underline t :weight extra-bold))))
   '(lsp-face-highlight-read ((t (:background "sea green"))))
   '(lsp-face-highlight-write ((t (:background "brown4"))))
   ;; '(lsp-ui-peek-peek ((t (:background "sea green"))))
   ;; '(lsp-ui-peek-list ((t (:background "deep sky blue"))))
   '(lsp-ui-peek-highlight ((t (:background "deep sky blue"))))
   '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
   '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil)))))

   ;; (map! :after lsp-ui-peek
   ;;       :map lsp-ui-peek-mode-map
   ;;       "h" #'lsp-ui-peek--select-prev-file
   ;;       "j" #'lsp-ui-peek--select-next
   ;;       "k" #'lsp-ui-peek--select-prev
   ;;       "l" #'lsp-ui-peek--select-next-file
   ;;       )

  ;; Slightly modified hydra version of original evil version from:
  ;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/config.el
  (defhydra +mr/lsp-traverse-hydra (:hint nil)
  "Traverse references"
  ("d" lsp-ui-peek-find-definitions "next" :bind nil)
  ("n" (-let [(i . n) (lsp-ui-find-next-reference)]
         (if (> n 0) (message "%d/%d" (+ i 1) n))) "next")
  ("p" (-let [(i . n) (lsp-ui-find-prev-reference)]
         (if (> n 0) (message "%d/%d" (+ i 1) n))) "prev")
  ("R" (-let [(i . n) (lsp-ui-find-prev-reference '(:role 8))]
         (if (> n 0) (message "read %d/%d" (+ i 1) n))) "prev read" :bind nil)
  ("r" (-let [(i . n) (lsp-ui-find-next-reference '(:role 8))]
         (if (> n 0) (message "read %d/%d" (+ i 1) n))) "next read" :bind nil)
  ("W" (-let [(i . n) (lsp-ui-find-prev-reference '(:role 16))]
         (if (> n 0) (message "write %d/%d" (+ i 1) n))) "prev write" :bind nil)
  ("w" (-let [(i . n) (lsp-ui-find-next-reference '(:role 16))]
         (if (> n 0) (message "write %d/%d" (+ i 1) n))) "next write" :bind nil)
  ("q" nil "stop")
  )
)

;; LSP-Mode
(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-auto-guess-root t lsp-eldoc-prefer-signature-help nil)
  (setq lsp-enable-links nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (add-to-list 'lsp-file-watch-ignored "build")
  ;; (setq lsp-project-blacklist '("/CC/"))
  )

;; LSP-Company
(use-package! company-lsp
  ;:load-path "~/Dev/Emacs/company-lsp"
  :after lsp-mode
  :config
  (setq company-transformers nil company-lsp-cache-candidates nil)
  (set-company-backend! 'lsp-mode 'company-lsp)
  )

;(set-company-backend! '(c-mode c++-mode)
;  '(company-lsp company-files company-yasnippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CCLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar +ccls-path-mappings [])

;;;###autoload
(defvar +ccls-initial-blacklist [])

;;;###autoload
(defvar +lsp-blacklist nil)

;;;###autoload
(defvar +my-use-eglot nil)

;;;###autoload
(defun +ccls|enable ()
  (when (and buffer-file-name (--all? (not (string-match-p it buffer-file-name)) +lsp-blacklist))
    (require 'ccls)
    (setq-local lsp-ui-sideline-show-symbol nil)
    (when (string-match-p "/llvm" buffer-file-name)
      (setq-local lsp-enable-file-watchers nil))
    (if +my-use-eglot (call-interactively #'eglot) (lsp))))

(defun +my|toggle-eglot ()
  (interactive)
  (setq +my-use-eglot (not +my-use-eglot))
  (message "use: %s" (if +my-use-eglot "eglot" "lsp-mode")))

(defun ccls/callee ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller ()
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind)
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind)
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 128)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

(use-package! ccls
  ;:load-path "~/Dev/Emacs/emacs-ccls"
  :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +ccls|enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
  (ccls-use-default-rainbow-sem-highlight)
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq
   ccls-initialization-options
   `(:clang
     (:excludeArgs
      ;; Linux's gcc options. See ccls/wiki
      ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants" "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
       "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2" "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4" "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387" "-mskip-rax-setup"
       "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s" "-Wa -"]
      :extraArgs []
      :pathMappings ,+ccls-path-mappings)
     :completion
     (:include
      (:blacklist
       ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
        "^/usr/(local/)?include/c\\+\\+/v1/"
        ]))
     :index (:initialBlacklist ,+ccls-initial-blacklist :parametersInDeclarations :json-false :trackDependency 1)))

  (after! projectile
   (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! c++-mode
  (set-formatter! 'c++-mode 'clang-format)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open debugging window style
;(setq gdb-many-windows t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalized bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! "+bindings")
