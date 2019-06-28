;;; pythonfmt.el --- Format the Python file.
;;
;; Author: c
;; Keywords: convenience tools extensions python
;; Package-Version: 20190628.1220
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
  (interactive)
  (let ((tmpfile (make-temp-file "pythonfmt" nil ".py"))
        (patchbuf (get-buffer-create "*pythonfmt patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-command-args)

    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patchbuf (erase-buffer))

          (write-region nil nil tmpfile)

          (setq our-command-args (mapconcat
                                  (function (lambda (x) (format "%s" x)))
                                  (append our-command-args
                                          (list pythonfmt-command-args tmpfile))
                                  " "))

          (message "Calling %s: %s %s"
                   pythonfmt-command pythonfmt-command our-command-args)

          (apply #'call-process-region (list (point-min) (point-max)
                                             "/bin/bash"
                                             nil nil nil
                                             "-c"
                                             (concat
                                              pythonfmt-command
                                              " "
                                              our-command-args)))

          (if (zerop (call-process-region (point-min) (point-max)
                                          "diff" nil patchbuf nil "-n" "-"
                                          tmpfile))
              (message "Buffer is already format.")
            (pythonfmt--apply-rcs-patch patchbuf)
            (with-current-buffer patchbuf
              (message "Buffer already fixed in: /n%s" (buffer-string))))))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defun pythonfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in pythonfmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (pythonfmt--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (pythonfmt--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in pythonfmt--apply-rcs-patch")))))))))

(defun pythonfmt--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun pythonfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

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
