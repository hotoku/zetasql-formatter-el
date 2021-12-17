;;; zetasql-formatter-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Yasunori Horikoshi

;; Author: Yasunori Horikoshi <horikoshi.et.al@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1


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


;;;###autoload
(defun zsfm-format ()
  "Format sql in the buffer."
  (interactive)
  (when zsfm-do-format
    (let* ((curbuf (current-buffer))
           (current (point))
           (outbuf-name "*zetasql*")
           (outbuf (get-buffer outbuf-name))
           (fpath (buffer-file-name))
           (command (zsfm-command fpath)))
      (when outbuf
        (save-excursion
          (switch-to-buffer outbuf)
          (erase-buffer)))
      (let ((ret (call-process-shell-command command nil outbuf-name)))
        (if (not (= ret 0))
            ())))))


;;;###autoload
(add-hook 'sql-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'zsfm-format nil t)))

(provide 'zetasql-formatter-mode)
;;; zetasql-formatter-mode.el ends here
