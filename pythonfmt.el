;;; pythonfmt.el --- Format the Python file.
;;
;; Author: c
;; Keywords: convenience tools extensions python
;; Package-Version: 20190627.2003
;; URL: https://github.com/czqhurricnae/pythonfmt.el
;; Version: 0.1.0
;; Copyright © 2019, c, all rights reserved.
;; Created: 27 June 2019
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(defgroup pythonfmt nil
  "Emacs interface to pythonfmt."
  :prefix "pythonfmt-"
  :group 'tools)

(defcustom pythonfmt-command nil
  "The `pythonfmt' command."
  :type 'string
  :group 'pythonfmt)

(defcustom pythonfmt-command-args nil
  "The `pythonfmt' command arguments."
  :type 'string
  :group 'pythonfmt)

(defun pythonfmt ()
  "Format the current buffer according to the pythonfmt tool."
  (unless (executable-find pythonfmt-command)
    (error "Cant not find %s in the exe path" pythonfmt-command))
  (let (our-command-args)
    (setq our-command-args (mapconcat
                            (function (lambda (x) (format "%s" x)))
                            (append our-command-args
                                    (list pythonfmt-command-args (buffer-file-name)))
                            " "))
    (apply #'call-process-region (list (point-min) (point-max)
                                       "/bin/bash"
                                       nil nil nil
                                       "-c"
                                       (concat
                                        pythonfmt-command
                                        " "
                                        our-command-args)))))

;;;###autoload
(define-minor-mode pythonfmt-mode
  "Enable formate-on-save for python mode buffers via python format tools."
  :lighter " fmt"
  (if pythonfmt-mode
      (add-hook 'before-save-hook 'pythonfmt-before-save t t)
    (remove-hook 'before-save-hook 'pythonfmt-before-save t)))

(defun pythonfmt-before-save ()
  "Format buffer via pythonfmt if major mode is a python mode."
  (interactive)
  (pythonfmt))

(provide 'pythonfmt)
;;; pythonfmt.el ends here
