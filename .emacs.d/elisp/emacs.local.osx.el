(when (string= system-type "darwin")

  (defun mck-post-init () 
    (load-theme 'solarized-dark t)))

