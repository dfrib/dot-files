;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
;;
;; This +bindings.el is heavily influenced and initially based upon the
;; +bindings.el of UndeadKernel.
;; https://github.com/UndeadKernel/emacs_doom_private
;; -------------------------------------------------------------------------- ;;

;; Change the default key of persp-mode to avoid conflicts with projectile.
(setq persp-keymap-prefix (kbd "C-c e")
      projectile-keymap-prefix (kbd "C-c p"))

(map!
 "M-x"           #'execute-extended-command
 "C-x C-b"       #'ibuffer-list-buffers
 ;; Lookup
 "<home>"        #'+lookup/definition
 "<prior>"       #'pop-tag-mark
 "<end>"         #'+lookup/references
 ;"<end>"         #'lsp-ui-peek-find-references
 ;; Quick access to magit-status (in addition to C-c v menu)
 "C-x g"         #'magit-status
 ;; Editor related bindings
 "C-a"           #'doom/backward-to-bol-or-indent
 [remap newline] #'newline-and-indent
 "C-j"           #'+default/newline
 "C-s"           #'swiper
 ;; Buffer related bindings
 "C-x b"         #'persp-switch-to-buffer
 "C-x B"         #'switch-to-buffer
 "C-x k"         #'doom/kill-this-buffer-in-all-windows
 ;; Switching windows
 "C-x p"         #'+popup/other
 ;; Smart-forward
 "M-<up>"    #'smart-up
 "M-<down>"  #'smart-down
 "M-<left>"  #'smart-backward
 "M-<right>" #'smart-forward
 ;; Restore common editing keys in minibuffer
 (:map (minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map
        read-expression-map)
   "C-g" #'abort-recursive-edit
   "C-a" #'move-beginning-of-line)
 ;; Doom emacs bindings
 (:prefix "C-c d"
   "d" #'+doom-dashboard/open
   "f" #'recentf-open-files
   "o" #'+popup/other
   "t" #'+popup/toggle
   "c" #'+popup/close
   "C" #'+popup/close-all
   "r" #'+popup/raise
   "R" #'+popup/restore
   "s" #'doom/open-scratch-buffer
   "S" #'doom/switch-to-scratch-buffer
   "u" #'doom/sudo-this-file
   "e" #'+eshell/open-popup
   "E" #'+eshell/open
   :desc "Reload Private Config" "R" #'doom/reload)
 "C-`" #'+popup/toggle
 ;; Org related bindings
 (:prefix "C-c o"
   "s"     #'org-caldav-sync
   "a a"   #'org-agenda
   "a t"   #'org-todo-list
   "a m"   #'org-tags-view
   "a v"   #'org-search-view
   ;"c"     #'org-capture
   "c"     (λ! (require 'org-capture) (call-interactively #'org-capture-goto-target))
   "b"     #'org-iswitchb
   ;; "e l b" #'org-beamer-export-to-latex
   ;; "e l B" #'org-beamer-export-as-latex
   ;; "e l P" #'org-beamer-export-to-pdf
   "l"     #'org-store-link
   "b"     #'+boy/org-babel-hydra/body)
 ;; lsp / ccls related bindings
 (:prefix "C-c c"
   ;; lsp
   :desc "Rename symbol"           "n" #'lsp-rename
   :desc "Goto type definition"    "g" #'lsp-goto-type-definition
   :desc "Peek definitions"        "p" #'lsp-ui-peek-find-definitions
   :desc "Peek references"         "P" #'lsp-ui-peek-find-references
   :desc "Peek implementation"   "C-p" #'lsp-ui-peek-find-implementation
   :desc "Traverse references"     "r" #'+mr/lsp-traverse-hydra/body
   ;; $ccls/inheritance
   ;; https://github.com/MaskRay/ccls/blob/master/src/messages/ccls_inheritance.cc
   :desc "Base (1 level)"          "b" (λ! (ccls/base 1))
   :desc "Base (3 levels)"         "B" (λ! (ccls/base 3))
   :desc "Derived (1 level)"       "d" (λ! (ccls/derived 1))
   :desc "Derived (3 levels)"      "D" (λ! (ccls/derived 3))
   :desc "Base hierarchy"          "i" (λ! (ccls-inheritance-hierarchy nil))
   :desc "Derived hierarchy"       "I" (λ! (ccls-inheritance-hierarchy t))
   ;; $ccls/call
   ;; https://github.com/MaskRay/ccls/blob/master/src/messages/ccls_call.cc
   :desc "Caller"                  "c" #'ccls/caller
   :desc "Callee"                  "C" #'ccls/callee
   :desc "Caller hierarchy"        "e" (λ! (ccls-call-hierarchy nil))
   :desc "Callee hierarchy"        "E" (λ! (ccls-call-hierarchy t))
   ;; $ccls/member
   ;; 2 (Type) => nested classes/namespace members
   ;; 3 (Func) => member functions
   ;; other => member variables
   ;; https://github.com/MaskRay/ccls/blob/master/src/messages/ccls_member.cc
   :desc "Member types"            "t" (λ! (ccls/member 2))
   :desc "Member functions"        "f" (λ! (ccls/member 3))
   :desc "Member vars (/other)"    "m" (λ! (ccls/member 0))
   :desc "Member hierarchy"        "M" #'ccls-member-hierarchy
   ;; $ccls/vars
   ;; https://github.com/maskray/ccls/blob/master/src/messages/ccls_vars.cc
   :desc "Vars (field or local)"   "v" (λ! (ccls/vars 3))
   :desc "Vars (field)"            "V" (λ! (ccls/vars 1))
   :desc "Vars (any)"            "C-v" (λ! (ccls/vars 7))
   ;; code lens
   :desc "Toggle CCLS code lens"   "L" #'ccls-code-lens-mode)
 ;; Snippets
 (:prefix "C-c s"
   :desc "New snippet"           "n" #'yas-new-snippet
   :desc "Insert snippet"        "i" #'yas-insert-snippet
   :desc "Find snippet"          "s" #'+default/find-in-snippets
   :desc "Find snippet for mode" "S" #'+default/browse-snippets
   :desc "Find global snippet"   "/" #'yas-visit-snippet-file
   :desc "Reload snippets"       "r" #'yas-reload-all
   :desc "Create Temp Template"  "c" #'aya-create
   :desc "Use Temp Template"     "e" #'aya-expand)
 ;; Version control bindings
 (:prefix "C-c v"
   :desc "Browse issues tracker" "i" #'+vc/git-browse-issues
   :desc "Browse remote"         "o" #'+vc/git-browse
   :desc "Diff current file"     "d" #'magit-diff-buffer-file
   :desc "Git revert hunk"       "r" #'git-gutter:revert-hunk
   :desc "Git stage file"        "S" #'magit-stage-file
   :desc "Git stage hunk"        "s" #'git-gutter:stage-hunk
   :desc "Git time machine"      "t" #'git-timemachine-toggle
   :desc "Git unstage file"      "U" #'magit-unstage-file
   :desc "Initialize repo"       "I" #'magit-init
   :desc "List repositories"     "L" #'magit-list-repositories
   :desc "Magit blame"           "b" #'magit-blame
   :desc "Magit buffer log"      "l" #'magit-log-buffer-file
   :desc "Magit commit"          "c" #'magit-commit
   :desc "Magit status"          "g" #'magit-status
   :desc "Next hunk"             "]" #'git-gutter:next-hunk
   :desc "Previous hunk" "[" #'git-gutter:previous-hunk)
 ;; Working with windows, workgroups and stuff.
 (:prefix "C-c w"
   "d" #'+workspace/display
   "r" #'+workspace/rename
   "c" #'+workspace/new
   "k" #'+workspace/delete
   "s" #'+workspace/save-session
   "l" #'+workspace/load-session
   "L" #'+workspace/load-last-session
   "o" #'doom/kill-other-buffers
   "u" #'winner-undo
   "U" #'winner-redo
   "p" #'+workspace/switch-left
   "n" #'+workspace/switch-right
   "h" #'resize-window ; requires private package 'resize-window'
   "1" (λ! (+workspace/switch-to 0))
   "2" (λ! (+workspace/switch-to 1))
   "3" (λ! (+workspace/switch-to 2))
   "4" (λ! (+workspace/switch-to 3))
   "5" (λ! (+workspace/switch-to 4))
   "6" (λ! (+workspace/switch-to 5))
   "7" (λ! (+workspace/switch-to 6))
   "8" (λ! (+workspace/switch-to 7))
   "9" (λ! (+workspace/switch-to 8))
   "0" #'+workspace/switch-to-last)

 ;; Plugins

 ;; Misc plugins
 "C-="     #'er/expand-region
 "C-c ."   #'goto-last-change ; requires private package 'goto-last-change'
 "C-c p p" #'projectile-switch-project
 ;; Smartparens
 (:after smartparens
   (:map smartparens-mode-map
     "C-M-a"     #'sp-beginning-of-sexp
     "C-M-e"     #'sp-end-of-sexp
     "C-M-f"     #'sp-forward-sexp
     "C-M-b"     #'sp-backward-sexp
     "C-M-d"     #'sp-splice-sexp
     "C-M-k"     #'sp-kill-sexp
     "C-M-t"     #'sp-transpose-sexp
     ;; "C-<right>" #'sp-forward-slurp-sexp
     ;; "M-<right>" #'sp-forward-barf-sexp
     ;; "C-<left>"  #'sp-backward-slurp-sexp
     ;; "M-<left>"  #'sp-backward-barf-sexp
     ))
 ;; Company mode
 "C-;" #'+company/complete
 ;; Counsel
 (:when (featurep! :completion ivy)
   (:after counsel
     (:map counsel-ag-map
       [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
       "C-SPC"    #'ivy-call-and-recenter ; preview
       "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action))))
 "C-h b"   #'counsel-descbinds
 "C-M-y"   #'counsel-yank-pop
 "C-h F"   #'counsel-faces
 "C-h p"   #'counsel-package
 "C-h a"   #'counsel-apropos
 "C-h V"   #'counsel-set-variable
 "C-'"     #'counsel-imenu
 "C-c g"   #'counsel-git
 "C-c j"   #'counsel-git-grep
 "C-c k"   #'counsel-ag
 "C-x l"   #'counsel-locate
 ;; Repl Toggle
 "C-c C-z" #'+eval/open-repl
 ;; Company mode and the like
 (:after company
   (:map company-active-map
     "C-o"      #'company-search-kill-others
     "C-n"      #'company-select-next
     "C-p"      #'company-select-previous
     "C-h"      #'company-show-doc-buffer
     "C-s"      #'company-search-candidates
     "M-s"      #'company-filter-candidates
     "C-;"      #'company-complete-common-or-cycle
     "TAB"      #'company-complete-common-or-cycle
     [backtab]  #'company-select-previous
     "C-RET"    #'counsel-company)
   (:map company-search-map
     "C-n"        #'company-search-repeat-forward
     "C-p"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))))
 ;; Refactoring and compilation
 (:map prog-mode-map
   "M-RET" #'emr-show-refactor-menu)
 (:after cc-mode
   (:map c++-mode-map
     "M-RET" #'srefactor-refactor-at-point)
   (:map c-mode-map
     "M-RET" #'srefactor-refactor-at-point))
 (:after help-mode
   ;; (:map help-map
   ;;   "e" 'doom/popup-toggle-messages)
   (:map help-mode-map
     "o" #'ace-link-help
     ">" #'help-go-forward
     "<" #'help-go-back))
 (:after info
   (:map Info-mode-map
     "o" #'ace-link-info))
 ;; yasnippet
 (:after yasnippet
   ;; keymap while yasnippet is active
   (:map yas-minor-mode-map
     "<C-tab>" #'yas-insert-snippet)
   ;; keymap while editing an inserted snippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field))
 ;; flycheck
 (:after flycheck
   (:map flycheck-error-list-mode-map
     "C-n" #'flycheck-error-list-next-error
     "C-p" #'flycheck-error-list-previous-error
     "RET" #'flycheck-error-list-goto-error))
 ;; flyspell
 (:after flyspell
   (:map flyspell-mode-map
     "C-;" nil ; Do not override
     "C-M-i" #'flyspell-auto-correct-previous-word))
 ;; ivy
 (:after ivy
   (:map ivy-minibuffer-map
     "TAB" #'ivy-alt-done
     "C-g" #'keyboard-escape-quit))
 ;; magit
 (:after magit
   (:map magit-mode-map
     ;; Don't let Tab binding in my bindings conflict with Tab in magit
     "<tab>" #'magit-section-toggle))
 ;; undo-tree
 (:after undo-tree
   (:map undo-tree-map
     "C-x <backspace>" #'undo-tree-visualize
     "C-x u"           #'undo-tree-undo))
 )

(which-key-add-key-based-replacements "C-c d"   "doom")
(which-key-add-key-based-replacements "C-c e"   "perspective")
(which-key-add-key-based-replacements "C-c o a" "org agenda")
(which-key-add-key-based-replacements "C-c o e" "org export")
(which-key-add-key-based-replacements "C-c o"   "org")
(which-key-add-key-based-replacements "C-c p"   "projectile")
(which-key-add-key-based-replacements "C-c s"   "snippets")
(which-key-add-key-based-replacements "C-c v"   "versioning")
(which-key-add-key-based-replacements "C-c w"   "workspace")
