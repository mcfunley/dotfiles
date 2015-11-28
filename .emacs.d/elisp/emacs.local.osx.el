(when (string= system-type "darwin")

  (defun mck-post-init ()
    nil))

(defun radicoweb ()
  (interactive)
  (let ((buf "*django*"))
    (shell buf)
    (with-current-buffer buf
      (let ((proc (get-buffer-process buf)))
        (dolist (cmd '("cd ~/radico/web" "workon web" "./manage.py runserver 8080"))
          (comint-simple-send proc cmd))))))
