;; -*- lexical-binding: t; -*-

(require 'super-hint)
(require 'rg)

(defun super-hint--shorten-string (str)
  (if (> (length str) super-hint-hint-width)
	  (let ((start (substring str 0 10))
            (end (substring str (- (length str) 39))))
        (concat start "…" end))
    str))


(defun super-hint--rg-buffer()
  (interactive)
  (goto-char (line-beginning-position))
  (let* ((get-msg-fn (lambda() (get-text-property (point) 'compilation-message)))
		 (msg (or
			   (funcall get-msg-fn)
			   (progn
				 (sit-for 0.0)
				 (funcall get-msg-fn))))
		 (function_name
		  (if msg
			  (let* ((loc (compilation--message->loc msg))
					 (file (caar (compilation--loc->file-struct loc)))
					 (line (compilation--loc->line loc))
					 (col (compilation--loc->col loc)))
				(super-hint-which-function file line col))
			nil
			;; (message "not get msg %s" (thing-at-point 'line))
			)))
	;; (message "msg %s, function_name got: %s"  msg function_name)
	(let* ((text (funcall super-hint-color-function function_name))
		   (ov (make-overlay (line-beginning-position)
							 (1+ (line-beginning-position))
							 nil
							 t)))
	  (overlay-put ov 'before-string text)
	  (overlay-put ov 'evaporate t))))



(defun super-hint--current-line-contains-rg-finished-p ()
  "Check if the current line contains 'rg finished'."
  (let ((line (thing-at-point 'line t)))
    (when line
	  (string-match-p "rg finished" line))))

(defun super-hint--rg-hint-all (&rest args)
  (interactive)

  (goto-char (point-min))



  (setq-local scroll-margin 1)
  (with-current-buffer (rg-buffer-name)
	(let ((start-time (current-time)))
	  (while (and (not (eobp)) (< (float-time (time-since start-time)) 30.0))
		(super-hint--rg-buffer)
		(forward-line))
	  ;; (message "hinting current line done: %s" (thing-at-point 'line) )
	  )
	(goto-char (point-min)))

  (unless exec/which-function-current-buffer-already-exist
	(if exec/which-function-last-buffer-name
		(kill-buffer (find-file-noselect exec/which-function-last-buffer-name))))
  (setq-local exec/which-function-current-buffer-already-exist nil)
  (setq-local exec/which-function-last-buffer-name ""))

(defun super-hint-setup(&rest args)
  (add-to-list 'compilation-finish-functions #'super-hint--rg-hint-all nil))


(defun super-hint-enable-rg()
  (interactive)
  (add-hook 'rg-mode-hook #'super-hint-setup))

(defun super-hint-disable-rg()
  (interactive)
  (remove-hook 'rg-mode-hook #'super-hint-setup)
  )

(provide 'super-hint-rg)
