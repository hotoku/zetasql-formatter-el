;;; zetasql-formatter-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Yasunori Horikoshi

;; Author: Yasunori Horikoshi <horikoshi.et.al@gmail.com>
;; Keywords: lisp
;; Version: 1.0.0


;;; Commentary:


;;; Code:

(define-minor-mode zetasql-formatter-mode
  "Format query by zetasql-formatter before saving the buffer.")

(defgroup zetasql-formatter nil
  "Format query by zetasql-formatter before saving the buffer."
  :group 'convenience
  :prefix "zsfm-")

(defcustom zsfm-do-format t
  "If this is nil, zetasql-formatter-mode does not format the buffer."
  :type 'boolean
  :group 'zetasql-formatter
  :local t
  :safe 'booleanp)

(defun zsfm-command (fpath)
  "Return zetasql-formatter command with argument FPATH."
  (let ((fname (file-name-nondirectory fpath))
        (dname (file-name-directory fpath)))
    (format "docker run --rm -v %s:/home:Z matts966/zetasql-formatter:latest %s"
            dname fname)))


;;;###autoload
(defun zsfm-format ()
  "Format sql in the buffer."
  (interactive)
  (when zsfm-do-format
    (let* ((curbuf (current-buffer))
           (outbuf-name "*zetasql*")
           (outbuf (get-buffer outbuf-name))
           (fpath (buffer-file-name))
           (command (zsfm-command fpath)))
      (message "command=%s" command)
      (when outbuf
        (switch-to-buffer outbuf)
        (erase-buffer)
        (switch-to-buffer curbuf))
      (call-process-shell-command command nil outbuf-name)
      (revert-buffer t t))))


;;;###autoload
(add-hook 'sql-mode-hook
          '(lambda ()
             (when (executable-find "docker")
               (add-hook 'after-save-hook'zsfm-format nil t))))

(provide 'zetasql-formatter-mode)
;;; zetasql-formatter-mode.el ends here
