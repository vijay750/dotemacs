;;; .emacs -- initialization file for GNU Emacs 29, may work for older versions

;; top-level vars, and frame look-n-feel
(setq inhibit-startup-message t
      find-file-existing-other-name t
      delete-auto-save-files t
      default-fill-column 90
      dired-listing-switches "-alg"
      inhibit-read-only nil
      mouse-yank-at-point t
      password-cache-expiry nil
	  vr:venv-root nil
      )
;; vertical scroll bar
(scroll-bar-mode)
(menu-bar-mode)
(tool-bar-mode -1)
(setq default-frame-alist
      '((width . 100)
        (height . 57)
        ))

;; TODO - how to do spelling? Why to do it?
(autoload 'ispell-region "flyspell" "spell checking as you go" t)

;; configure package manager
;; TODO: how to keep it up to date?
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents t))
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; more vars and defuns from 1993+
;; disable the "normal" marking
(transient-mark-mode 0)
;; set this if there are custom elisp files
;;(setq load-path (cons (expand-file-name "~/elisp") load-path))

(setq text-mode-hook
      '((lambda () (auto-fill-mode 1))
        (lambda () (flyspell-mode 1))
        ))

(setq find-file-visit-truename nil
      next-line-add-newlines nil
      font-lock-maximum-decoration t)
(setq-default column-number-mode t)

;; keyboard macros and key symbol changes
;; make super key act as meta
(setq x-super-keysym 'meta)
(fset 'yes-or-no-p (symbol-function 'y-or-n-p))

;; Functions

;;; Look for matching parenthesis
;;; from:   michael@pbinfo.UUCP  or  michael@uni-paderborn.de
(defun vi-type-paren-match (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))
        (t (self-insert-command (or arg 1)))))
(define-key global-map "%" 'vi-type-paren-match)

;; start view mode if the file is read only
(defun my-readonly-file-hook ()
  "if file is readonly, turn on view less mode"
  (if buffer-read-only
	 (view-mode)))
(add-hook 'find-file-hooks 'my-readonly-file-hook)

(defun what-face (pos)
  "name of the face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun cursor-shape-hook ()
  (cond ((equal (thing-at-point 'line) "\n")
         (setq cursor-type '(bar . 5)))
        ((eolp) (setq cursor-type '(bar . 2)))
        (t (setq cursor-type 'box))))

(add-hook 'post-command-hook 'cursor-shape-hook)


;;; My keybindings - muscle memory
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-c h") 'help-command)
(define-key esc-map "@" 'set-mark-command)
(define-key Buffer-menu-mode-map " " 'Buffer-menu-select)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; M-C-s is also mapped to this. One needs to be mapped to not query?
(define-key esc-map "s" 'query-replace-regexp)
;; (define-key esc-map "z" 'my-compile-command)
;; taken over by X/gnome
;; (define-key esc-map " " 'delete-horizontal-space)
(define-key esc-map "$" 'ispell-word)
(define-key esc-map "#" 'ispell-region)
(define-key ctl-x-map "\\" 'just-one-space)
;; (define-key ctl-x-map "\n" 'goto-line)
(define-key global-map "\^y" 'yank)
(define-key global-map "\M-y" 'yank-pop)
(define-key global-map "\^l" 'recenter)
(define-key ctl-x-map "x" 'copy-to-register)
(define-key ctl-x-map "g" 'insert-register)
(define-key minibuffer-local-completion-map " " 'minibuffer-complete)

;; unset C-x f - set-fill-column - I HATE THIS key
(global-unset-key [(control x) f])
(define-key emacs-lisp-mode-map [(control E)] 'eval-defun)
(define-key emacs-lisp-mode-map [(control R)] 'eval-region)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-x SPC") 'gud-break)

(global-set-key [(control delete)] 'backward-char)
;; Make F5 be `undo'
(global-set-key '[f5] 'undo)
;; Make F6 be `what-line'
(global-set-key '[f6] 'what-line)
;; Make F7 be `font-lock-fontify-buffer'
(global-set-key '[f7] 'font-lock-fontify-buffer)
;; Make F9 be "revert-buffer"
(global-set-key '[f9] 'revert-buffer)
;; Make `C-x C-m' and `C-x RET' be different (since I tend to type
;; the latter by accident sometimes.)
(define-key global-map [(control x) return] nil)
;; scroll more on mouse
(setq mouse-wheel-scroll-amount-horizontal 4)

;;; packages
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(require 'which-func)
(use-package idomenu
  :config
  (global-set-key (kbd "C-c f") 'idomenu))

(require 'imenu)

;; use-package didn't work, had to install via packages-list from elpa. melpa is old?
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish
  :commands (projectile-project-root)
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c @ p" . projectile-command-map)
  :init
   (setq projectile-project-search-path (list "~/dev/leankloud" '("~/dev/mb" . 1)))
  ;;(setq projectile-switch-project-action #'projectile-dired)
  )

(use-package magit
  :config
  (define-key magit-status-mode-map (kbd "C-<tab>") nil)
  (define-key magit-diff-mode-map (kbd "C-<tab>") nil)
  (global-set-key (kbd "C-c @ c") 'magit-status)
  )

;;; rust
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c l l" . flycheck-list-errors)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l r" . lsp-rename)
              ("C-c l q" . lsp-workspace-restart)
              ("C-c l Q" . lsp-workspace-shutdown)
              ("C-c l s" . lsp-rust-analyzer-status)
              ("C-c l e" . lsp-rust-analyzer-expand-macro)
              ("C-c l d" . dap-hydra)
              ("C-c l h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;;; python
;; language-server
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . (lambda ()
					  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
					  (lsp-headerline-breadcrumb-mode)))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-jedi
  :config
  (add-to-list 'lsp-enabled-clients 'jedi))

(use-package lsp-ivy)

(use-package pyvenv
  :config
  (pyvenv-mode t)
  (pyvenv-tracking-mode 1)
  )


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.1))

(use-package company-box
  :hook (company-mode . company-box-mode))


;;; look-n-feel
(set-face-background 'default "WhiteSmoke")
(set-face-foreground 'default "black")
(set-face-foreground 'italic "red")
(set-face-foreground 'bold "black")
(set-face-foreground 'font-lock-function-name-face "royalblue")
(make-face-bold 'font-lock-function-name-face)
(set-face-foreground 'font-lock-comment-face "deeppink4")
(set-face-foreground 'font-lock-string-face "red")
(set-face-foreground 'font-lock-doc-face "darkgreen")
(make-face-bold 'font-lock-doc-face)
(set-face-foreground 'font-lock-keyword-face "navyblue")
(make-face-bold 'font-lock-keyword-face)
(set-face-foreground 'font-lock-preprocessor-face "black")
(make-face-bold 'font-lock-preprocessor-face)
(set-face-foreground 'font-lock-variable-name-face "steelblue")
(make-face-bold 'font-lock-variable-name-face)
;;(set-face-underline-p 'font-lock-variable-name-face t)
(set-face-underline 'font-lock-variable-name-face t)
(set-face-foreground 'font-lock-type-face "firebrick")
(make-face-bold 'font-lock-type-face)
(set-face-foreground 'font-lock-constant-face "gold")
(set-face-background 'font-lock-constant-face "grey40")
(make-face-bold 'font-lock-constant-face)
(set-face-underline 'font-lock-string-face nil)
(set-face-foreground 'highlight "gold")
(set-face-background 'highlight "royalblue")
(set-face-foreground 'font-lock-builtin-face "darkgreen")
(make-face-bold 'font-lock-builtin-face)


;;; customized via custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(gud-pdb-command-name "python3 -m pdb")
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 1048576)
 '(line-move-visual nil)
 '(package-selected-packages nil)
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((vr:venv-root . "venv") (vr:venv-root . "aws-metrics")
	 (vr:venv-root . "intel3.9")))
 '(tab-width 4)
 '(warning-suppress-types '(((undo discard-info)) (comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "cyan" :foreground "black"))))
 '(isearch-fail ((t (:background "dark orange")))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)


;; automatic indentation
(electric-indent-mode 1)
;; Ignoring electric indentation for python
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))

;; make python mode hook use local vars
;; https://stackoverflow.com/questions/19697453/
(defun my-python-mode-hook ()
  (set-newline-and-indent)
  (setq lsp-jedi-executable-command
		(concat (projectile-project-root)
				vr:venv-root "/bin/jedi-language-server"))
  (lsp-deferred))


(defun my-local-vars-hook ()
  (when (derived-mode-p 'python-mode) (my-python-mode-hook)))

(add-hook 'hack-local-variables-hook #'my-local-vars-hook)

(which-function-mode 1)
