;;; .emacs -- initialization file for GNU Emacs 24, may work for 22+.
;; make sure elisp installed via elpa
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  )

;; turn on menu bar and scroll bar!
(scroll-bar-mode)
(menu-bar-mode)
(setq default-frame-alist
      '(
        (width . 100)
        (height . 57)
        ))

(autoload 'ispell-region "flyspell" "spell checking as you go" t)

;;{{{ Set top vars

(setq inhibit-startup-message t
      find-file-existing-other-name t
      delete-auto-save-files t
      default-fill-column 80
      dired-listing-switches "-alg"
      inhibit-read-only nil
      mouse-yank-at-point t
      password-cache-expiry nil
      )
(transient-mark-mode 0)
;;(setq load-path (cons (expand-file-name "~/elisp") load-path))
;;(setq crite-string "/*\n * Copyleft. Some rights reserved.\n **/\n")

(setq text-mode-hook
      '((lambda () (auto-fill-mode 1))
        (lambda () (flyspell-mode 1))
        ))

(setq find-file-visit-truename nil
      ;; find-file-compare-truenames t
      ;; minibuffer-confirm-incomplete t
      ;; complex-buffers-menu-p t
      next-line-add-newlines nil
      mail-yank-prefix "> "
      font-lock-maximum-decoration t)

(setq-default column-number-mode t)

;; keyboard macros and key symbol changes
;; make super key act as meta
(setq x-super-keysym 'meta)
(fset 'yes-or-no-p (symbol-function 'y-or-n-p))

;; Functions

;;; Look for matching parenthesis
;;;
;;; from:   michael@pbinfo.UUCP  or  michael@uni-paderborn.de
(defun vi-type-paren-match (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))
        (t (self-insert-command (or arg 1)))))
(define-key global-map "%" 'vi-type-paren-match)

(require 'ido)
(ido-mode t)

;; my compilation thingy which is used to set the compile command "dynamagically"
(defun my-compile-command ()
  "Set the value of the variable `compile-command' to conduct the compilation on the
least loaded polar* machines. Expects that the shell command, ~/cmds/set_comp_mc,
echoes the correct value. Look at ~vijay/cmds/set_comp_mc on how to do this. "
  (interactive)
  (progn
    (make-local-variable 'compile-command)
    (cond ((or (file-exists-p "Makefile") (file-exists-p "makefile"))
           (setq compile-command "make"))
          ((file-exists-p "build.xml")
           (setq compile-command "ant"))
          ((file-exists-p "pom.xml")
           (setq compile-command "mvn compile"))
          ((file-exists-p "build.gradle")
           (setq compile-command "gradle build"))
          ((or (string= "cxx" (substring (buffer-name) -3 nil))
               (string= ".cc" (substring (buffer-name) -3 nil))
               (string= ".C" (substring (buffer-name) -2 nil)))
           (setq compile-command (concat "g++ -g " (buffer-name))))
          ((string= "java" (substring (buffer-name) -4 nil))
           (setq compile-command (concat "javac " (buffer-name))))
          (t (setq compile-command "make"))))
  (command-execute 'compile))
					
(global-set-key (kbd "C-c @ c") 'magit-status)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c f") 'idomenu)
(setq same-window-regexps (list "\\*rsh-[^-]*\\*\\(\\|<[0-9]*>\\)" "\\*telnet-.*\\*\\(\\|<[0-9]+>\\)" "^\\*rlogin-.*\\*\\(\\|<[0-9]+>\\)" "\\*info\\*\\(\\|<[0-9]+>\\)" "\\*gud-.*\\*\\(\\|<[0-9]+>\\)" "\\`\\*Customiz.*\\*\\'" "\\*Buffer List\\*" "\\*Help\\*"))
					
(define-key global-map "\^h" 'backward-delete-char-untabify)
(define-key global-map "\^_" 'help-command)
;; what the hell are these???
;; (define-key esc-map "n" 'down-comment-line)
;; (define-key esc-map "p" 'up-comment-line)
(define-key esc-map "@" 'set-mark-command)
(define-key Buffer-menu-mode-map " " 'Buffer-menu-select)

;;; other key bindings i like
;;(define-key global-map "\^w" 'backward-kill-word)
(define-key esc-map "h" 'backward-kill-word)
(define-key ctl-x-map "\^b" 'buffer-menu)
(define-key esc-map "=" 'compare-windows)
(define-key esc-map "s" 'query-replace-regexp)
(define-key esc-map "z" 'my-compile-command)
(define-key esc-map " " 'delete-horizontal-space)
(define-key esc-map "$" 'ispell-word)
(define-key esc-map "#" 'ispell-region)
(define-key ctl-x-map "\\" 'just-one-space)
;;(define-key ctl-x-map "r" 'vm)
;;(define-key ctl-x-map "n" 'gnus)
(define-key ctl-x-map "\n" 'goto-line)
;;(define-key ctl-x-map "m" 'vm-mail)
(define-key global-map "\^y" 'yank)
(define-key global-map "\M-y" 'yank-pop)
(define-key global-map "\^l" 'recenter)
(define-key ctl-x-map "x" 'copy-to-register)
(define-key ctl-x-map "g" 'insert-register)
(define-key minibuffer-local-completion-map " " 'minibuffer-complete)

;; unset C-x f - set-fill-column - I HATE THIS key
(global-unset-key [(control x) f])


(define-key emacs-lisp-mode-map [(control E)] 'eval-defun)
;; (global-set-key (kbd "C-c @ c") 'svn-examine)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-x SPC") 'gud-break)

;; Make backspace and delete be the same.  This doesn't work in all
;; cases; a better way would be to use xmodmap.
;; (global-set-key 'backspace [delete])
;; (global-set-key '(meta backspace) [(meta delete)])
;; (global-set-key '(control backspace) [(control delete)])
;; (global-set-key '(meta control backspace) [(meta control delete)])
;; set C-del to be backward-char
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

(setq auto-save-directory (expand-file-name "~/autosave/")
      auto-save-directory-fallback auto-save-directory
      auto-save-hash-p nil
      ;; efs-auto-save t
      ;; efs-auto-save-remotely nil
      ;; now that we have auto-save-timeout, let's crank this up
      ;; for better interactive response.
      auto-save-interval 2000
      )

;; (require 'mycc-mode)
;; (require 'php+-mode)
;; (php+-mode-setup)
(require 'which-func)

;; geben for debugging
;; (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;; start view mode if the file is read only
(defun my-readonly-file-hook ()
  "if file is readonly, turn on view less mode"
  (if buffer-read-only
	 (view-mode)))

(add-hook 'find-file-hooks 'my-readonly-file-hook)

;;(require 'psvn)
;;(define-key global-map [(control c) @ c] 'svn-examine)
;;(define-key svn-status-mode-map '[button2] 'svn-status-find-file-or-examine-directory)

(set-face-background 'default "WhiteSmoke")
;;(set-face-background 'default "antiquewhite")
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
(set-face-underline-p 'font-lock-variable-name-face t)
(set-face-foreground 'font-lock-type-face "firebrick")
(make-face-bold 'font-lock-type-face)
(set-face-foreground 'font-lock-constant-face "gold")
(set-face-background 'font-lock-constant-face "grey40")
(make-face-bold 'font-lock-constant-face)
(set-face-underline-p 'font-lock-string-face nil)
;;(set-face-background 'mode-line "#babdb6")
;;(set-face-foreground 'mode-line "black")
(set-face-foreground 'highlight "gold")
(set-face-background 'highlight "royalblue")
;;(set-face-foreground 'text-cursor "black")
;;(set-face-background 'text-cursor "firebrick")
;;(set-face-foreground 'modeline-mousable "red")
;;(set-face-foreground 'modeline-buffer-id "blue")
;; (set-face-foreground 'widget-documentation "red")
;; (set-face-foreground 'hyperlink "gold")
;; (set-face-foreground (gdb-arrow-face) "black")
;; (set-face-background (gdb-arrow-face) "paleturquoise")
;; (set-face-foreground 'isearch "black")
;; (set-face-background 'isearch "cyan")
(set-face-foreground 'font-lock-builtin-face "darkgreen")
(make-face-bold 'font-lock-builtin-face)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(eclim-eclipse-dirs '("~/eclipse"))
 '(eclim-executable "~/eclipse/eclim")
 '(gud-pdb-command-name "python3 -m pdb")
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 1048576)
 '(line-move-visual nil)
 '(package-selected-packages
   '(docker-compose-mode dockerfile-mode jedi-core jedi projectile find-file-in-project elpy flycheck-yamllint yaml-imenu yaml-mode thrift psvn pig-mode php+-mode mvn maven-test-mode magit-commit-training-wheels magit javadoc-lookup idomenu groovy-mode gradle-mode geben flymake-python-pyflakes expand-region ensime emacs-eclim company better-defaults))
 '(python-shell-interpreter "python3")
 '(tab-width 4)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "cyan" :foreground "black"))))
 '(isearch-fail ((t (:background "dark orange")))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; eclim for java/eclipse integration. The relevant eclim server must be running
;; (require 'eclim)
;; (global-eclim-mode)
;; (require 'eclimd)


(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'auto-complete-config)
(ac-config-default)

;; ;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

(require 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-c <tab>") 'company-complete)

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
(add-hook 'python-mode-hook 'set-newline-and-indent)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
;;(add-hook 'python-mode-hook 'my/python-mode-hook)

(which-function-mode 1)
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(package-initialize)
(elpy-enable)
