;; -*- lexical-binding: t; -*-

(require 'xref)

(defun super-hint--xref-hint()
  (interactive)
  ;; get xref file  and line, set col as 0
  (let* ((entry (xref--item-at-point))
		 (function_name
		  (when entry
			(let ((location (xref-item-location entry)))
			  (when (xref-file-location-p location)
				(let ((file (xref-file-location-file location))
					  (line (xref-file-location-line location))
					  (col (xref-file-location-column location)))
				  (super-hint-which-function file line col)))))))

	;; got function_name, may be nil
	(let* ((text (funcall super-hint-color-function function_name))
		   (ov (make-overlay (line-beginning-position)
							 (1+ (line-beginning-position))
							 nil t)))
	  (overlay-put ov 'before-string text)
	  (overlay-put ov 'evaporate t))))


(defun super-hint--xref-hint-all()
  (interactive)
  (setq-local scroll-margin 1)
  (when (memq this-command
			  '(xref-find-references
				xref-find-definition-or-reference
				lsp-find-implementation
				xref-revert-buffer
				exec/lsp-toggle-filter-test))
	(with-current-buffer (current-buffer)
	  (goto-char (point-min))
	  (while (not (eobp))
		(super-hint--xref-hint)
		(forward-line))
	  (goto-char (point-min)))))

(defun super-hint--xref-hint-after-update()
  (super-hint--xref-hint-all))


;;;###autoload
(defun super-hint-enable-xref()
  (interactive)
  (add-hook 'xref-after-update-hook #'super-hint--xref-hint-after-update))

;;;###autoload
(defun super-hint-disable-xref()
  (interactive)
  (remove-hook 'xref-after-update-hook #'super-hint--xref-hint-after-update)
  )

(provide 'super-hint-xref)
