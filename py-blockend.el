;;; py-blockend.el

;; ----------------------------------------------------------------------------

(defgroup py-blockend nil
  ""
  :group 'convenience)

(defcustom py-blockend-global t
  ""
  :type 'boolean)

(defcustom py-blockend-command "pyblockend"
  ""
  :type 'string)

(defcustom py-blockend-command-append "--append --def-return"
  ""
  :type 'string)

(defcustom py-blockend-command-remove "--remove"
  ""
  :type 'string)

(defcustom py-blockend-append-after-save t
  ""
  :type 'boolean)

;; ----------------------------------------------------------------------------

(defun py-blockend-toggle-global ()
  (interactive)
  (setq py-blockend-global (null py-blockend-global)))

(defun py-blockend-global-enable ()
  (interactive)
  (setq py-blockend-global t))

(defun py-blockend-global-disable ()
  (interactive)
  (setq py-blockend-global nil))

(defun py-blockend-toggle-append-after-save ()
  (interactive)
  (setq py-blockend-append-after-save (null py-blockend-append-after-save)))

;; ----------------------------------------------------------------------------

(defun py-blockend-append-region (beg end)
  (interactive "r")
  (py-blockend--command-region beg end py-blockend-command-append))

(defun py-blockend-remove-region (beg end)
  (interactive "r")
  (py-blockend--command-region beg end py-blockend-command-remove))

(defun py-blockend-append-buffer (&optional keep)
  (interactive)
  (let ((modified (buffer-modified-p)))
    (py-blockend-append-region (point-min) (point-max))
    (when keep (set-buffer-modified-p modified))
    nil))

(defun py-blockend-remove-buffer (&optional keep)
  (interactive)
  (let ((modified (buffer-modified-p)))
    (py-blockend-remove-region (point-min) (point-max))
    (when keep (set-buffer-modified-p modified))
    nil))

;; ----------------------------------------------------------------------------

(defun py-blockend--command-call (beg end command-option)
  (apply 'call-process-region beg end
         py-blockend-command t t nil
         (split-string command-option " ")))

(defun py-blockend--command-region (beg end command-option)
  (let ((modified (buffer-modified-p))
        (param (py-blockend--prepare beg end))
        rdata pdata rl s1 s2)
    (setq s1 (nth 0 param)
          rdata (nth 1 param)
          pdata (nth 2 param))
    (setq beg (nth 0 rdata)
          end (nth 1 rdata)
          bline (nth 2 rdata))
    (py-blockend--command-call beg end command-option)
    (setq s2 (buffer-substring beg (point)))
    (cond
     ((string= s1 s2)
      (set-buffer-modified-p modified))
     (t
      (setq bline (py-blockend--region-line s1 s2 bline))
      (setq pdata (py-blockend--update-point pdata beg bline))))
    (set-window-start (get-buffer-window) (nth 1 pdata))
    (goto-char (nth 0 pdata))
    nil))

(defun py-blockend--prepare (beg end)
  (let ((cntsl (count-screen-lines (window-start) (point)))
        (winpos (window-start)) (bufpos (point))
        column colpos bline)
    (when (> beg end)
      (cl-rotatef beg end))
    (beginning-of-line)
    (setq column (- bufpos (setq colpos (point))))
    (goto-char beg)
    (beginning-of-line)
    (setq bline (count-lines (point) colpos))
    (when (> beg bufpos)
      (setq bline (- bline)))
    (list (buffer-substring beg end)
          (list beg end bline)
          (list bufpos winpos column cntsl))))

(defun py-blockend--update-point (param beg bline)
  (let ((bufpos (nth 0 param))
        (winpos (nth 1 param))
        (column (nth 2 param))
        (cntsl (nth 3 param))
        bpos wpos)
    (goto-char beg)
    (beginning-of-line)
    (line-move bline t)
    (setq bpos (+ (point) column))
    (line-move-visual (- cntsl) t)
    (setq wpos (point))
    (list bpos wpos)))

(defun py-blockend--region-line (s1 s2 q)
  (let ((x (< (length s1) (length s2)))
        p1 p2 l1 l2 m n n1 n2)
    (if (< q 0) q
      (when x (cl-rotatef s1 s2))
      (setq s1 (split-string s1 "\n")
            s2 (split-string s2 "\n"))
      (setq n1 (length s1)
            n2 (length s2))
      (if (>= q (setq n (if x n2 n1)))
          (+ q (- (if x n1 n2) n))
        (progn
          (setq  p1 0  p2 0)
          (while (< p1 n1)
            (setq l1 (nth p1 s1)
                  l2 (nth p2 s2))
            (add-to-list 'm (cons p1 p2))
            (when (string= l1 l2)
              (setq p2 (1+ p2)))
            (setq p1 (1+ p1)))
          (if x
              (car (rassoc q m))
            (cdr (assoc q m))))))))

;; ----------------------------------------------------------------------------

(defvar py-blockend--hook-temporary nil)

(defun py-blockend--before-save-hook ()
  (let ((bpos (point)) (wpos (window-start)) pbuf nbuf)
    (setq pbuf (buffer-substring (point-min) (point-max)))
    (py-blockend--command-call (point-min) (point-max) py-blockend-command-remove)
    (setq nbuf (buffer-substring (point-min) (point-max)))
    (setq py-blockend--hook-temporary (list bpos wpos pbuf nbuf))))

(defun py-blockend--after-save-hook ()
  (let ((bpos (nth 0 py-blockend--hook-temporary))
        (wpos (nth 1 py-blockend--hook-temporary))
        (pbuf (nth 2 py-blockend--hook-temporary))
        (nbuf (nth 3 py-blockend--hook-temporary)))
    (when (string= nbuf (buffer-substring (point-min) (point-max)))
      (delete-region (point-min) (point-max)) (insert pbuf)
      (set-window-start (get-buffer-window) wpos)
      (goto-char bpos))
    (when py-blockend-append-after-save
      (py-blockend-append-buffer))
    (set-buffer-modified-p nil)
    (setq py-blockend--hook-temporary nil)))

(defun py-blockend--add-hooks ()
  (add-hook 'before-save-hook 'py-blockend--before-save-hook nil t)
  (add-hook 'after-save-hook 'py-blockend--after-save-hook nil t))

(defun py-blockend--remove-hooks ()
  (remove-hook 'before-save-hook 'py-blockend--before-save-hook t)
  (remove-hook 'after-save-hook 'py-blockend--after-save-hook t))

(define-minor-mode py-blockend-mode
  "py-blockend minor mode"
  :global nil
  :keymap nil
  :lighter ""

  (cond
   ((and py-blockend-global
         py-blockend-mode)
    (save-excursion
      (py-blockend--command-call (point-min) (point-max)
                                 py-blockend-command-append)
      (set-buffer-modified-p nil))
    (py-blockend--add-hooks))
   (t
    (py-blockend--remove-hooks))))

;; ----------------------------------------------------------------------------
(provide 'py-blockend)
;; ----------------------------------------------------------------------------
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
