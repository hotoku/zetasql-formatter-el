;;; zetasql-formatter.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Yasunori Horikoshi

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Author: Yasunori Horikoshi <horikoshi.et.al@gmail.com>
;; Keywords: SQL
;; Version: 1.0.5


;;; Commentary:


;;; Code:

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
(when (executable-find "docker")
  (add-hook 'sql-mode-hook
            '(lambda ()
               (add-hook 'after-save-hook 'zsfm-format nil t))))

(provide 'zetasql-formatter)
;;; zetasql-formatter.el ends here
