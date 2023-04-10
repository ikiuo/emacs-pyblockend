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

(defcustom py-blockend-minor-mode-line " EOB"
  ""
  :type 'string)

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

(defun py-blockend-goto-line (LINE)
  (interactive "nGoto Line: ")
  (if py-blockend-mode
      (let ((cbuf (current-buffer))
            (tbuf (generate-new-buffer " *py-blockend-temporary*"))
            (cb (buffer-string)) tb alist)
        (apply 'call-process-region (point-min) (point-max)
               py-blockend-command nil tbuf nil
               (split-string py-blockend-command-remove " "))
        (set-buffer tbuf)
        (setq tb (buffer-string))
        (set-buffer cbuf)
        (kill-buffer tbuf)
        (setq alist (caar (cdr (py-blockend--updated-line-map cb tb))))
        (goto-line (car (rassoc LINE alist)))
        nil)
    (goto-line LINE)))

;; ----------------------------------------------------------------------------

(defun py-blockend--command-call (beg end command-option)
  (apply 'call-process-region beg end
         py-blockend-command t t nil
         (split-string command-option " ")))

(defun py-blockend--command-region (beg end command-option)
  (let* ((modified (buffer-modified-p))
         (pdata (py-blockend--prepare beg end))
         wdata rdata cbuf ubuf rbeg rend rlen)
    (cl-multiple-value-setq (wdata rdata cbuf) pdata)
    (cl-multiple-value-setq (rbeg rend rlen) rdata)
    (py-blockend--command-call rbeg rend command-option)
    (setq ubuf (buffer-substring rbeg (point)))
    (let (ulm grow udata wp wins)
      (if (string= cbuf ubuf)
          (set-buffer-modified-p modified)
        (setq ulm (py-blockend--updated-line-map cbuf ubuf))
        (cl-multiple-value-setq (grow udata) ulm))
      (py-blockend--set-window-start-and-point
       (dolist (wp wdata wins)
         (add-to-list 'wins (py-blockend--update-point
                             rbeg wp grow udata)))))))

(defun py-blockend--prepare (beg end)
  (let ((wins (py-blockend--window-start-and-point))
        (bol-point (lambda (p) (goto-char p) (beginning-of-line) (point)))
        (eol-point (lambda (p) (goto-char p) (end-of-line)
                     (if (< (point) (point-min)) (forward-char)) (point)))
        win wdata)
    (save-excursion
      (when (> beg end)
        (cl-rotatef beg end))
      ;; (setq beg (funcall bol-point beg))
      ;; (setq end (funcall eol-point end))
      (dolist (wp wins)
        (let ((win (nth 0 wp))
              (wpos (funcall bol-point (nth 1 wp)))
              (bpos (nth 2 wp))
              wline rline lpos cols)
          (setq wline (count-screen-lines wpos bpos nil win))
          (setq cols (- bpos (setq lpos (funcall bol-point bpos))))
          (setq rline (count-lines beg lpos))
          (when (> beg lpos)
            (setq rline (- rline)))
          (add-to-list 'wdata (list win wpos wline bpos rline cols) t nil))))
    (list wdata
          (list beg end (count-lines beg end))
          (buffer-substring beg end))))

(defun py-blockend--updated-line-map (b1 b2)
  (let* ((s1 (split-string b1 "\n")) (n1 (length s1)) (p1 0) l1
         (s2 (split-string b2 "\n")) (n2 (length s2)) (p2 0) l2
         (grow (< n1 n2)) m)
    (when grow
      (cl-rotatef s1 s2)
      (cl-rotatef n1 n2))
    (while (< p1 n1)
      (setq l1 (nth p1 s1)
            l2 (nth p2 s2))
      (setq m (append (list (cons p1 p2)) m))
      (when (string= l1 l2)
        (setq p2 (1+ p2)))
      (setq p1 (1+ p1)))
    (list grow (list m n1 n2 (- n1 n2)))))

(defun py-blockend--update-point (rpos wdata grow udata)
  (let (win wpos wline bpos rline cols)
    (cl-multiple-value-setq (win wpos wline bpos rline cols) wdata)
    (when (and udata (> rline 0))
      (let (m n1 n2 nd)
        (cl-multiple-value-setq (m n1 n2 nd) udata)
        (if (< rline (if grow n1 n2))
            (setq rline (if grow
                            (car (rassoc rline m))
                          (cdr (assoc rline m))))
          (setq rline (+ rline (if grow nd (- nd))))))
      (save-excursion
        (goto-char rpos)
        (line-move rline t)
        (setq bpos (+ (point) cols))
        (line-move-visual (- wline) t)
        (setq wpos (point))))
    (list win wpos bpos)))

(defun py-blockend--window-start-and-point ()
  (mapcar (lambda (win)
            (list win (window-start win) (window-point win)))
          (get-buffer-window-list (current-buffer) nil t)))

(defun py-blockend--set-window-start-and-point (wins)
  (let (wp win start point)
    (dolist (wp wins)
      (cl-multiple-value-setq (win start point) wp)
      (set-window-start win start)
      (set-window-point win point))))

;; ----------------------------------------------------------------------------

(defvar py-blockend--hook-temporary nil)

(defun py-blockend--before-save-hook ()
  (let ((win (py-blockend--window-start-and-point)) pbuf nbuf)
    (setq pbuf (buffer-string))
    (py-blockend-remove-buffer)
    (setq nbuf (buffer-string))
    (setq py-blockend--hook-temporary (list win pbuf nbuf))))

(defun py-blockend--after-save-hook ()
  (let (wins pbuf nbuf)
    (cl-multiple-value-setq (wins pbuf nbuf) py-blockend--hook-temporary)
    (when (string= nbuf (buffer-string))
      (erase-buffer) (insert pbuf)
      (py-blockend--set-window-start-and-point wins))
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
  :lighter py-blockend-minor-mode-line

  (make-variable-buffer-local 'py-blockend--mode-enable)

  (cond
   ((and py-blockend-global
         py-blockend-mode)
    (setq py-blockend--mode-enable t)
    (py-blockend-append-buffer t)
    (when (eq 0 (length (get-buffer-window-list (current-buffer))))
      (goto-char (point-min)))
    (py-blockend--add-hooks))
   (t
    (when py-blockend--mode-enable
      (setq py-blockend--mode-enable nil)
      (py-blockend-remove-buffer t))
    (py-blockend--remove-hooks))))

;; ----------------------------------------------------------------------------
(provide 'py-blockend)
;; ----------------------------------------------------------------------------
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
