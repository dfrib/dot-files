;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;;
;; This config is heavily influenced by the non-evil DOOM Emacs config of
;; UndeadKernel: https://github.com/UndeadKernel/emacs_doom_private
;; -------------------------------------------------------------------------- ;;

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
(def-package! resize-window
  :commands (resize-window))

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

;; Doom theme
;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-vibrant)
;;(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-nova)
;;(setq doom-theme 'doom-dracula)
;;(setq doom-theme 'doom-solarized-light)
;;(setq doom-theme 'doom-sourcerer)

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

;; (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; (add-to-list 'auto-mode-alist '("Dockerfile.*\\" . plantuml-mode))

;; Jinja2
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode and org-capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq +org-dir "~/org/")

;; Org-capture snippets functions from:
;; http://www.howardism.org/Technical/Emacs/capturing-content.html
(require 'which-func)

(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil)))

(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
   %s

   #+BEGIN_%s %s
%s
   #+END_%s" initial-txt type headers code-snippet type)))

;; org config and capture templates
(after! org
  (setq org-startup-folded nil ; do not start folded
        org-tags-column -100 ; the column to the right to align tags
        org-log-done 'time ; record the time when an element was marked done/checked
        org-fontify-done-headline nil ; do not change the font of DONE items
        org-ellipsis " ↴ "
        org-bullets-mode 1
        org-directory "~/org"
        org-latex-caption-above nil)

  ;; Clear the org-capture-templates before adding new teampltes
  ;; (Use only custom ones)
  (setq org-capture-templates 'nil)

  ;; Templates adapted from: https://github.com/niklascarlsson/doom-private

  ;; --- Personal snippets
  (add-to-list 'org-capture-templates
               '("p" "Personal entries"))
  ;; Code snippet
  (add-to-list 'org-capture-templates
               '("ps" "Code snippet (personal)"  entry
                 (file "~/org/personal/code/snippets.org")
                 "* %?\n%(ha/org-capture-code-snippet \"%F\")"))

  ;; Example block snippet
  (add-to-list 'org-capture-templates
               '("pe" "Example snippet (personal)"  entry
                 (file "~/org/personal/example/snippets.org")
                 "* %?\n%(ha/org-capture-clip-snippet \"%F\")"))

  ;; Work todos on real-life post-its! :) for now...

  ;; Journal
  (add-to-list 'org-capture-templates
               '("pj" "Journal (personal)" entry
                 (file+olp+datetree "~/org/personal/journal.org")
                 "* %?" :append t))

  ;; --- Work snippets
  (add-to-list 'org-capture-templates
               '("w" "Work entries"))
  ;; Code snippet
  (add-to-list 'org-capture-templates
               '("ws" "Code snippet (work)"  entry
                 (file "~/org/work/code/snippets.org")
                 "* %?\n%(my/org-capture-code-snippet \"%F\")"))

  ;; Example block snippet
  (add-to-list 'org-capture-templates
               '("we" "Example snippet (work)"  entry
                 (file "~/org/work/example/snippets.org")
                 "* %?\n%(my/org-capture-clip-snippet \"%F\")"))

  ;; Todo list
  (add-to-list 'org-capture-templates
               '("wt" "Todo (work)" entry
                 (file+headline "~/org/work/todo.org" "Inbox")
                 "* [ ] %?\n%i" :prepend t :kill-buffer t))

  ;; Journal entry
  (add-to-list 'org-capture-templates
               '("wj" "Journal (work)" entry
                 (file+olp+datetree "~/org/work/journal.org")
                 "* %?\nEntered on %U\n %i\n %a")))

;; automatically redisplay images generated by babel
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; place latex-captions below figures and tables
(setq org-latex-caption-above nil)

;; Custom build command for org to latex pdf export.
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode %f"
        "xelatex -shell-escape -interaction nonstopmode %f"
        "xelatex -shell-escape -interaction nonstopmode %f"))

;; Configure Org to use minted.
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

;; Custom org latex class.
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("zarticle"
                 "\\documentclass[11pt,Wordstyle]{Zarticle}
                      \\usepackage[utf8]{inputenc}
                      \\usepackage{graphicx}
                          [NO-DEFAULT-PACKAGES]
                          [PACKAGES]
                          [EXTRA] "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))))

;; Add a an option to use lines to frame the exported code block.
(setq org-latex-minted-options '(("frame" "lines")))



(setq org-babel-python-command "python3")

;; TODO(dfrib): Look into org-noter to annotate e.g. PDF document in an external
;; synched org document.
;; https://github.com/weirdNox/org-noter

;; TODO(dfrib): Look into org-brain for mapping different concepts written in
;; in org documents with multiple parents and so on (mind-map style).
;; https://github.com/Kungsgeten/org-brain

;; Enable eshell in babel blocks
(load! "+ob-eshell")

;; Org theme customization
(add-hook! org-mode
  ;; Document title font
  (set-face-attribute 'org-document-title nil :height 2.0)
  ;; Face of keyword DONE (Green like strings)
  (set-face-attribute 'org-done nil :foreground "#98be65")
  ;; Face of keyword TODO or [ ] (Purple like keywords)
  (set-face-attribute 'org-todo nil :foreground "#c678dd")
  ;; Face of ellipsis symbol (Purple like keywords)
  (set-face-attribute 'org-ellipsis nil :foreground "#c678dd")
  ;; Face of the entire headline of a DONE line
  (set-face-attribute 'org-headline-done nil :foreground nil)
  (cond
   ;; If doom-one theme is enabled
   ((custom-theme-enabled-p 'doom-one)
      ;; Change the style of the BEGIN_SRC and RESULT blocks
      (set-face-attribute 'org-block-begin-line nil
                          :background "#4c516d"
                          :foreground "#979aaa"
                          :weight 'normal
                          :height 0.9
                          :box '(:line-width 2 :color "#4c516d")))
   ;; For other themes, disable the changes
   (t
      (set-face-attribute 'org-block-begin-line nil
                          :background unspecified
                          :foreground unspecified
                          :weight unspecified
                          :height unspecified
                          :box unspecified
                          :inherit org-meta-line)))

  ;; Hydra from UndeadKernel's config.
  ;; https://github.com/UndeadKernel/emacs_doom_private/blob/master/autoload/org.el
  (defhydra +boy/org-babel-hydra (:hint nil)
    "
Org-Babel:
_n_:next      _c_:clear results  _i_:show all
_p_:previous  _h_:show/hide      _I_:hide all
_e_:edit      _RET_:execute      _l_:center screen
_g_:goto      _s_:split          _q_:cancel
"
    ("c" org-babel-remove-result)
    ("RET" org-babel-execute-src-block)
    ("e" org-edit-src-code)
    ("h" org-hide-block-toggle-maybe)
    ("s" org-babel-demarcate-block)
    ("g" org-babel-goto-named-src-block)
    ("i" org-show-block-all)
    ("I" org-hide-block-all)
    ("n" org-babel-next-src-block)
    ("p" org-babel-previous-src-block)
    ("l" recenter-top-bottom)
    ("q" nil :color blue))
  )

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
(setq +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select t
        :ttl    5))

;; TODO(dfrib): See if I really feel that I need this customization?
;; Select the IList buffer when it is shown
(after! imenu-list
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select t :ttl 0))

;; Larger undo tree window
(after! undo-tree
  (set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'right :size 40 :modeline nil :select t :quit t))

;; Larger org src edit
(after! org
  (set-popup-rule! "^\\*Org Src" :side 'bottom :slot -2 :height 0.6 :width 0.5 :select t :autosave t :ttl nil :quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet file templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom file templates
;; Just place them in ~/.doom.d/snippets - these will take precedence over the
;; default ones.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and caching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prettify lambdas in elisp
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
;; Let the scratch buffer have elisp major mode by default
;; if set to t it has the same mode as previous buffer
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! magit
  ;; Show differences at the word level when a hunk is selected.
  (setq magit-diff-refine-hunk t))
(add-hook! magit-mode (visual-line-mode +1))

;; Automatic spellchecking in commit messages
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;; Submodules hook in status buffer.
(with-eval-after-load 'magit
(magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! ibuffer
  ;; nearly all of this is the default layout
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 50 50 :left :elide) ; change: 30s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ag - The Silver Searcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-package! ag
  :defer t
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! ivy
  ;; Add a kill action to Ivy's buffer switching
  (ivy-set-actions 'ivy-switch-buffer '(("k" kill-buffer "kill")))
  ;; Add a kill action to DOOM's buffer switching
  (ivy-set-actions '+ivy/switch-workspace-buffer '(("k" kill-buffer "kill"))))

(after! counsel
  ;; Override the default ignore regexp to not ignore dot-files.
  (setq counsel-find-file-ignore-regexp
        "\(?:^[#]\)\|\(?:[#~]$\)\|\(?:^Icon?\)"
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell
;; this should be set to nil for performance
;; https://www.emacswiki.org/emacs/FlySpell
(setq flyspell-issue-message-flag nil)

;; Choose hunspell as our spell checker
(setq ispell-program-name "hunspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  )

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
;; LSP-Mode
;; (def-package! lsp-mode
;;   :commands (lsp-mode))

(def-package! lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  :config
  (add-to-list 'lsp-file-watch-ignored "build")
  ;; (setq lsp-project-blacklist '("/CC/"))
  )

;; LSP-UI
;;https://github.com/MaskRay/Config
(def-package! lsp-ui
  :demand t
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

;; (require 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; LSP-Company
(def-package! company-lsp
  :after company lsp-mode
  :init
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil
        company-lsp-enable-snippet t)
  )
(set-company-backend! '(c-mode c++-mode)
  '(company-lsp company-files company-yasnippet))

;; LSP-python
;; (def-package! lsp-python
;;   :init (add-hook! python-mode #'lsp-python-enable)
;;   :config
;;   (set-company-backend! 'python-mode 'company-lsp)
;; )

;; LSP-Flycheck
;; (require 'lsp-ui-flycheck)
;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))
;; (add-hook 'c-mode-common-hook 'flycheck-mode) ;; Turn on flycheck for C++ buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CCLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO(dfrib): move the ccls path to private.
(defun +ccls//enable ()
  (require 'ccls)
  (lsp))

(def-package! ccls
  :commands (lsp-ccls-enable)
  ;; run ccls by default in C++ files
  ;:init (add-hook! (c-mode c++-mode cuda-mode objc-mode) #'+ccls//enable)
  :init (add-hook! 'c-mode-common-hook #'+ccls//enable)
  :config
  (setq ;ccls-executable (expand-file-name "~/opensource/ccls/Release/ccls")
        ccls-cache-dir (concat doom-cache-dir ".ccls_cached_index")
        ccls-sem-highlight-method 'font-lock)
  (setq ccls-extra-args '("--log-file=/tmp/cc.log"))
  (setq ccls-initialization-options
        '(:completion (:detailedLabel t) :xref (:container t)
                      :diagnostics (:frequencyMs 5000)))
  (set-company-backend! '(c-mode c++-mode) '(company-lsp))
  )

;; Recommended CCLS helpers from
;; https://github.com/MaskRay/ccls/wiki/Emacs
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook for `clang-format-buffer'
(defun my-after-save-actions ()
  "Used in `after-save-hook'."
  (when (memq this-command '(save-buffer))
    'clang-format-buffer
    ))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'my-after-save-actions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open debugging window style
(setq gdb-many-windows t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalized bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! "+bindings")
