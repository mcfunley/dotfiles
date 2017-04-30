(eval-when-compile (require 'cl))

;;; -----------------------------------------------------------------------------
;;; paths

(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'exec-path "/usr/local/bin")

;;; -----------------------------------------------------------------------------
;;; packages

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fill-column-indicator
     aggressive-indent
     bm
     cider
     clojure-mode
     color-theme
     color-theme-molokai
     dockerfile-mode
     erc-hl-nicks
     fsharp-mode
     flycheck
     dash
     company-quickhelp
     company
     gist
     gh
     go-mode
     haskell-mode
     js2-mode
     json-mode
     json-reformat
     json-snatcher
     less-css-mode
     logito
     markdown-mode
     pcache
     php-mode
     pkg-info
     epl
     popup
     pos-tip
     queue
     rainbow-delimiters
     s
     scala-mode
     spinner
     swift3-mode
     web-mode
     yaml-mode))))

(when (seq-remove #'package-installed-p package-selected-packages)
  (package-install-selected-packages))


;;; -----------------------------------------------------------------------------
;;; windows, etc

(setq split-width-threshold nil)

;;; -----------------------------------------------------------------------------
;;; comint / shells / terminals

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;;; -----------------------------------------------------------------------------
;;; icomplete

(icomplete-mode t)

;; This fixes the default completion when using icomplete. For example,
;; if you type C-x k to kill the current buffer, type nothing, and then force
;; icomplete via C-j, the buffer killed will be whatever icomplete would have
;; completed to and not the default.
;; This seems like the kind of thing that will be fixed in future versions
;; of icomplete.
(defun icomplete-complete-or-default ()
  (interactive)
  (let* ((start (minibuffer-prompt-end))
         (end (point-max))
         (phrase (buffer-substring start end)))
    (if (zerop (length phrase))
        ; selects the minibuffer's default
        (minibuffer-complete-and-exit)
      ; selects icomplete's default completion
      (minibuffer-force-complete-and-exit))
  ))

;; key bindings similar to the old iswitchb keys
(setq icomplete-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\M-\t] 'minibuffer-force-complete)
    (define-key map [?\C-j]  'icomplete-complete-or-default)
    (define-key map [return]  'icomplete-complete-or-default)
    (define-key map [?\C-.]  'icomplete-forward-completions)
    (define-key map [?\C-s]  'icomplete-forward-completions)
    (define-key map [?\C-,]  'icomplete-backward-completions)
    (define-key map [?\C-r]  'icomplete-forward-completions)
    map))

(setq completion-ignore-case t)

;;; -----------------------------------------------------------------------------
;;; ibuffer

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("scala" (mode . scala-mode))
               ("erc" (mode . erc-mode))
               ("php" (mode . php-mode))
               ("py" (mode . python-mode))
               ("shell" (mode . shell-mode))
               ("elisp" (mode . emacs-lisp-mode))
               ("evernote" (or (mode . evernote-mode)
                               (name . "^\\*ENB.*")
                               (name . ".*[Ee]vernote.*")))
               ("js" (mode . js2-mode))
               ("sql" (mode . sql-mode))
               ("smarty" (name . ".*tpl$"))
               ("clojure" (mode . clojure-mode))
               ("nrepl" (mode . nrepl-messages-mode))
               ("emacs" (or (name . "^\\*scratch\\*$")
                            (mode . ielm)
                            (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;; -----------------------------------------------------------------------------
;;; keys

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command) ; originally compose-mail
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-x\C-z" 'undo)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "<C-tab>") 'indent-relative)
(global-set-key (kbd "s-w") 'fixup-whitespace)

;; apple command key is the meta key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;;; -----------------------------------------------------------------------------
;;; files

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; -----------------------------------------------------------------------------
;;; fci

(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-column 81)
(setq fci-rule-color "#1E2021")


;;; -----------------------------------------------------------------------------
;;; progmodes

;; languages
(require 'python)
(require 'haskell-mode)
(require 'scala-mode)
(require 'go-mode)

(setq indent-tabs-mode nil)

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun enable-tab-completion ()
  (local-set-key [tab] 'indent-or-expand))

(defun show-parens-in-buffer ()
  (make-local-variable 'show-paren-mode)
  (show-paren-mode t))

(defun progmode-defaults ()
  (interactive)
  (enable-tab-completion)
  (linum-mode t)
  (fci-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (show-parens-in-buffer))

(dolist (h '(python-mode-hook
             emacs-lisp-mode-hook
             lisp-mode-hook
             php-mode-hook
             c-mode-hook
             html-mode-hook
             haskell-mode-hook
             ruby-mode-hook
             scala-mode-hook
             go-mode-hook
             clojure-mode-hook
             web-mode-hook
             less-css-mode-hook
             java-mode-hook))
  (add-hook h 'progmode-defaults))


;;;; override tab width for java files
(add-hook 'java-mode-hook (lambda () (setq tab-width 2)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; -----------------------------------------------------------------------------
;;; web mode

(add-to-list 'auto-mode-alist (cons "\\.djhtml\\'" 'web-mode))
(add-to-list 'auto-mode-alist (cons "\\.tpl\\'" 'web-mode))
(add-to-list 'auto-mode-alist (cons "\\.jinja\\'" 'web-mode))
(add-to-list 'auto-mode-alist (cons "\\.soy\\'" 'web-mode))
(add-to-list 'auto-mode-alist (cons "\\.html\\'" 'web-mode))

(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq css-indent-offset 2)

;;; -----------------------------------------------------------------------------
;;; appearance / global interface
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq visible-bell nil
      ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq inhibit-startup-message t)

(when window-system
  (set-frame-size (selected-frame) 125 55)
  (set-frame-position (selected-frame) 300 30))

;;; -----------------------------------------------------------------------------
;;; server / daemon stuff

(server-start)

; If running as --daemon, prevent killing the process via muscle memory.
(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'delete-frame))


;;; -----------------------------------------------------------------------------
;;; bm.el

(when (fboundp 'define-fringe-bitmap)
  (require 'bm)
  (global-set-key (kbd "s-b") 'bm-toggle)
  (global-set-key (kbd "s-B") 'bm-show-all)
  (global-set-key (kbd "s-,") 'bm-previous)
  (global-set-key (kbd "s-.") 'bm-next))


;;; -----------------------------------------------------------------------------
;;; clojure

(require 'clojure-mode)
(setq cider-lein-command (executable-find "lein"))
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(put-clojure-indent 's/defn :defn)
(put-clojure-indent 's/fn :defn)
(put-clojure-indent 'defroutes :defn)
(put-clojure-indent 'routes :defn)
(put-clojure-indent 'GET 2)
(put-clojure-indent 'POST 2)
(put-clojure-indent 'PUT 2)
(put-clojure-indent 'DELETE 2)
(put-clojure-indent 'HEAD 2)
(put-clojure-indent 'ANY 2)
(put-clojure-indent 'context 2)
(put 's/defn 'clojure-doc-string-elt 2)
(put 's/defschema 'clojure-doc-string-elt 2)

;;; -----------------------------------------------------------------------------
;;; markdown

(autoload 'markdown-mode
  "markdown-mode.el"
  "Major mode for editing Markdown files"
  t)
(setq markdown-command (executable-find "markdown"))

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))


;;; -----------------------------------------------------------------------------
;;; aggressive-indent

(require 'aggressive-indent)
(global-aggressive-indent-mode)

(dolist (m '(html-mode
             sql-mode
             web-mode
             markdown-mode
             css-mode
             less-css-mode
             js2-mode))
  (add-to-list 'aggressive-indent-excluded-modes m))


;;; -----------------------------------------------------------------------------
;;; php
(c-add-style
 "my-php-style"
 '((c-offsets-alist . ((arglist-close . c-lineup-close-paren)))))

(defun php-formatting-defaults ()
  ; This crap fixes array indentation
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
  (c-set-style "my-php-style"))

(defun php-pman ()
  (interactive)
  (let ((man-term (read-from-minibuffer "Term: " (current-word))))
    (man (concat "-M /usr/share/pear/doc/pman " man-term))))
(add-hook 'php-mode-hook '(lambda () (local-set-key "\C-c\C-h" 'php-pman)))

(add-hook 'php-mode-hook 'php-formatting-defaults)
(add-hook 'java-mode-hook 'php-formatting-defaults)


;;; -----------------------------------------------------------------------------
;;; javascript and json

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; do not delete trailing whitespace all over the place on save and fuck up
;; my diffs
(setq js2-cleanup-whitespace nil)

(defun js-formatting-defaults ()
  (interactive)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3))

(add-hook 'js2-mode-hook 'progmode-defaults)
(add-hook 'js2-mode-hook 'js-formatting-defaults)

(when (load "ess-autoloads.el" t)
  (require 'ess-site))

(setq js-indent-level 2)
(add-hook 'json-mode-hook 'progmode-defaults)

;;; -----------------------------------------------------------------------------
;;; sql

(defun sql-mode-defaults ()
  (toggle-truncate-lines 1))

(add-hook 'sql-mode-hook 'sql-mode-defaults)
(add-hook 'sql-interactive-mode-hook 'sql-mode-defaults)


;;; -----------------------------------------------------------------------------
;;; color-theme

(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)

(message "done")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
