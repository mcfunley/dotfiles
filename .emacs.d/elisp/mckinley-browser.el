(eval-when-compile (require 'cl))

(require 'php-mode)


(defvar have-w3m 
  ;; Compile w3m --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
  (or 
   (load-path-safe "/Applications/Emacs.app/Contents/share/emacs/site-lisp/w3m/")
   (load-path-safe "~/lib/site-lisp/w3m")))


(defvar *url-handlers* nil)

(defvar *browse-url-program* (expand-file-name "~/bin/browse-url"))


(defun browse-url-x-host (url &optional new-window)
  (call-process *browse-url-program* nil nil nil url))

(defvar *default-url-handler* (if (file-exists-p *browse-url-program*)
                                  'browse-url-x-host
                                'browse-url-default-macosx-browser))


(defun* choose-browser (url &optional new-window)
  (dolist (u *url-handlers*)
    (when (string-match-p (car u) url)
      (apply (cdr u) url new-window)
      (return-from choose-browser)))
  (apply *default-url-handler* url new-window))


(setq browse-url-browser-function 'choose-browser)


; needs work
;(when have-w3m
;  (require 'mckinley-w3m))


(provide 'mckinley-browser)
