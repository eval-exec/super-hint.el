;; -*- lexical-binding: t; -*-

(defcustom super-hint-hint-width 50
  "The width of the hint window."
  :type 'integer
  :group 'super-hint)

(defcustom super-hint-color-function #'super-hint-colorful
  "The function to color the hint."
  :type 'function
  :group 'super-hint
  )


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

(defun super-hint-colorful(str)
  (let* ((str (or str ""))
		 (dot-pos (cl-search "." str)))

	;; apply color to hint like: "AfterContext.is_switch"
	(if dot-pos
		(progn
		  (put-text-property 0 dot-pos 'face 'font-lock-type-face str)
		  (put-text-property (1+ dot-pos) (length str) 'face 'font-lock-property-name-face str))
	  (put-text-property 0 (length str) 'face 'font-lock-type-face str))

	;; apply color to hint like:  "ParseResult<T>"
	(let ((start 0))
	  (while (string-match "[<>]" str start)
		(put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-punctuation-face str)
		(setq start (match-end 0))))

	;; apply color to hint like:  "Iterator for Walk"
	(let ((start 0))
	  (while (string-match "\\bfor\\b" str start)
		(put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-keyword-face str)
		(setq start (match-end 0))))

	(format (concat "%-" (number-to-string super-hint-hint-width) "s%s")
			(super-hint--shorten-string str)
			(propertize
			 "│"
			 'face 'font-lock-variable-use-face))))


(provide 'super-hint)
