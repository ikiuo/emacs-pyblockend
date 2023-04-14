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

(defvar py-blockend--temporary-buffer-name " *py-blockend-temporary*")

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
  (py-blockend--append-region beg end))

(defun py-blockend-append-buffer (&optional keep)
  (interactive)
  (let ((modified (buffer-modified-p)))
    (py-blockend-append-region (point-min) (point-max))
    (when keep (set-buffer-modified-p modified))
    nil))

;; ----------------------------------------------------------------------------

(defun py-blockend-remove-region (beg end)
  (interactive "r")
  (py-blockend--remove-region beg end))

(defun py-blockend-remove-buffer (&optional keep)
  (interactive)
  (let ((modified (buffer-modified-p)))
    (py-blockend-remove-region (point-min) (point-max))
    (when keep (set-buffer-modified-p modified))
    nil))

;; ----------------------------------------------------------------------------

(defun py-blockend-goto-line (LINE)
  (interactive "nGoto Line: ")
  (when py-blockend-mode
    (let (exit output)
      (cl-multiple-value-setq (exit output)
        (py-blockend--execute-command (py-blockend--remove-command-line)
                                      (point-min) (point-max)))
      (ignore-errors
        (setq LINE (car (rassoc LINE (py-blockend--compare-lines
                                      (buffer-string) output)))))))
  (goto-line LINE))

;; ----------------------------------------------------------------------------

(defun py-blockend--append-command-line ()
  (concat py-blockend-command " " py-blockend-command-append))

(defun py-blockend--append-region (&optional beg end)
  (py-blockend--update-region (py-blockend--append-command-line) beg end))

(defun py-blockend--append-buffer ()
  (py-blockend--append-region))

;; ----------------------------------------------------------------------------

(defun py-blockend--remove-command-line ()
  (concat py-blockend-command " " py-blockend-command-remove))

(defun py-blockend--remove-region (&optional beg end)
  (py-blockend--update-region (py-blockend--remove-command-line) beg end))

(defun py-blockend--remove-buffer ()
  (py-blockend--revmoe-region))

;; ----------------------------------------------------------------------------

(defun py-blockend--execute-command (command beg end)
  (let ((curr (current-buffer))
        (targ (generate-new-buffer py-blockend--temporary-buffer-name t))
        exit output)
    (setq exit (call-shell-region beg end command nil targ))
    (set-buffer targ) (setq output (buffer-string))
    (set-buffer curr) (kill-buffer targ)
    (unless (eq exit 0)
      (user-error "Error (exit code = %s): %s\nOutput:\n%s"
                  exit command output))
    (list exit output)))

(defun py-blockend--update-region (command &optional beg end)
  (cl-multiple-value-setq (beg end)
    (py-blockend--fix-region beg end))
  (let ((wdata (py-blockend--update-prepare beg end))
        exit output input)
    (cl-multiple-value-setq (exit output)
      (py-blockend--execute-command command beg end))
    (setq input (buffer-substring beg end))
    (unless (string= input output)
      (let ((lmap (py-blockend--compare-lines input output beg)))
        (py-blockend--update-window beg end wdata lmap)))))

;; ----------------------------------------------------------------------------

(defun py-blockend--point-beginnig-of-line (pos)
  (save-excursion (goto-char pos) (beginning-of-line) (point)))

(defun py-blockend--point-end-of-line (pos)
  (save-excursion (goto-char pos) (end-of-line) (point)))

(defun py-blockend--fix-region (beg end)
  (save-excursion
    (unless beg (setq beg (point-min)))
    (unless end (setq end (point-max)))
    (when (> beg end)
      (cl-rotatef beg end))
    (setq beg (py-blockend--point-beginnig-of-line beg))
    (goto-char end) (beginning-of-line)
    (unless (eq end (point))
      (end-of-line)
      (if (eq end (point-max))
          (insert "\n")
        (forward-char))
      (setq end (point))))
  (list beg end))

(defun py-blockend--window-start-and-point ()
  (mapcar (lambda (win)
            (list win (window-start win) (window-point win)))
          (get-buffer-window-list (current-buffer) nil t)))

(defun py-blockend--set-window-start-and-point (wins)
  (let (wp win wpos bpos)
    (dolist (wp wins)
      (cl-multiple-value-setq (win wpos bpos) wp)
      (set-window-start win wpos)
      (set-window-point win bpos))))

;; ----------------------------------------------------------------------------

(defun py-blockend--update-prepare (beg end)
  (let ((wins (py-blockend--window-start-and-point))
        wdata wp win wpos wline bpos rline cols lpos)
    (save-excursion
      (dolist (wp wins)
        (cl-multiple-value-setq (win wpos bpos) wp)
        (setq wline (count-screen-lines wpos bpos nil win))
        (setq lpos (py-blockend--point-beginnig-of-line bpos))
        (setq cols (- bpos lpos))
        (setq rline (count-lines beg lpos))
        (when (> beg lpos) (setq rline (- rline)))
        (add-to-list 'wdata (list win wpos wline bpos rline cols) t nil)))
    wdata))

(defun py-blockend--compare-lines (input output &optional update)
  (let* ((ilines (split-string input "\n")) (ilen (length ilines))
         (olines (split-string output "\n")) (olen (length olines))
         (diff (- ilen olen)) (ipos 0) (opos 0)
         iline oline lmap)
    (when update (goto-char update) (beginning-of-line))
    (while (or (< ipos ilen) (< opos olen))
      (setq lmap (append (list (cons ipos opos)) lmap))
      (setq iline (nth ipos ilines))
      (setq oline (nth opos olines))
      (cond
       ((string= iline oline)
        (when update (line-move 1 t) (beginning-of-line))
        (setq ipos (1+ ipos))
        (setq opos (1+ opos)))
       ((> diff 0)
        (when update (delete-line) (beginning-of-line))
        (setq ipos (1+ ipos)))
       ((< diff 0)
        (when update (insert oline "\n"))
        (setq opos (1+ opos)))))
    (append (list (cons ipos opos)) lmap)))

(defun py-blockend--update-window (beg end wdata lmap)
  (let* ((ilen (caar lmap)) (olen (cdar lmap)) (dlen (- ilen olen))
         wins wp win wpos wline bpos rline cols)
    (dolist (wp wdata)
      (cl-multiple-value-setq (win wpos wline bpos rline cols) wp)
      (cond
       ((< rline 0) nil)
       ((< rline ilen) (setq rline (cdr (assoc rline lmap))))
       (t (setq rline (- rline dlen))))
      (when (>= rline 0)
        (goto-char beg) (beginning-of-line)
        (line-move rline t) (beginning-of-line)
        (setq bpos (+ (point) cols))
        (line-move-visual (- wline) t) (beginning-of-visual-line)
        (setq wpos (point)))
      (add-to-list 'wins (list win wpos bpos)))
    (py-blockend--set-window-start-and-point wins)))

;; ----------------------------------------------------------------------------

(defvar py-blockend--hook-temporary nil)

(defun py-blockend--before-save-hook ()
  (let ((win (py-blockend--window-start-and-point)) pbuf nbuf)
    (setq pbuf (buffer-string))
    (py-blockend-remove-buffer t)
    (setq nbuf (buffer-string))
    (setq py-blockend--hook-temporary (list win pbuf nbuf))))

(defun py-blockend--after-save-hook ()
  (let (wins pbuf nbuf)
    (cl-multiple-value-setq (wins pbuf nbuf) py-blockend--hook-temporary)
    (when (string= nbuf (buffer-string))
      (erase-buffer) (insert pbuf)
      (py-blockend--set-window-start-and-point wins))
    (when py-blockend-append-after-save
      (py-blockend-append-buffer t))
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
    (setq py-blockend-mode nil)
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
