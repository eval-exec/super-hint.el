;; -*- lexical-binding: t; -*-

(defvar-local exec/which-function-last-buffer-name "")

(defvar-local exec/which-function-current-buffer-already-exist nil)

(defun super-hint-which-function (file line column)
  (unless (eq file exec/which-function-last-buffer-name)
	(unless exec/which-function-current-buffer-already-exist
	  (if exec/which-function-last-buffer-name
		  (bury-buffer (find-file-noselect exec/which-function-last-buffer-name))))
	(setq-local exec/which-function-current-buffer-already-exist (get-file-buffer file))
	(setq-local exec/which-function-last-buffer-name file))
  (let* ((buffer (find-file-noselect file)))
	(with-current-buffer buffer
	  (save-excursion
		(goto-line line)
		(move-to-column column)
		(which-function)))))


(provide 'super-hint)
