

; license: gnu. see end of file


;;; filter-lines

(defun filter-lines (&optional regexp)
  "Hide lines that don't match REGEXP. Call with no argument or nil to restore them."
  ; some logic from hide-lines
  (interactive "MShow only lines matching regexp: ")
  (if (or (null regexp) (string= "" regexp))
      (progn
        (remove-from-invisibility-spec 'filter-lines) ; show flagged lines
        (remove-overlays (point-min) (point-max) 'invisible 'filter-lines)) ; clear flags
    (save-excursion
      (goto-char (point-min))
      (let ((start (point-min))
             ; re-search sets point to end of match, returns point. t means return nil if fail
            (pos (re-search-forward regexp nil t))
            (flag-text (lambda (start end) (overlay-put (make-overlay start end) 'invisible 'filter-lines))))
        (while pos
          (beginning-of-line)
          (funcall flag-lines start (point))
          (forward-line 1) ; move to start of next line
          (setq start (point))
          (if (eq (point) (point-max))
              (setq pos nil)
            (setq pos (re-search-forward regexp nil t))))
        (funcall flag-text start (point-max))
        (add-to-invisibility-spec 'filter-lines) ; hide flagged lines
        ))))

; for testing
; (global-set-key (kbd "C-<f12>") 'filter-lines)
; (global-set-key (kbd "C-M-f") 'filter-lines)
; (global-set-key (kbd "M-4") 'filter-lines)
; (global-set-key (kbd "M-4") (lambda () (filter-lines-mode) (filter-lines)))


; (filter-lines "lines")
; (filter-lines "asdf")
; (filter-lines)

; (defun mark-invisible (start end) (overlay-put (make-overlay start end) 'invisible 'filter-lines))
; (defun hide-lines () (add-to-invisibility-spec 'filter-lines))
; (defun show-lines () (remove-from-invisibility-spec 'filter-lines))
; (defun mark-visible () (remove-overlays (point-min) (point-max) 'invisible 'filter-lines))



(defvar filter-lines/keymap (make-sparse-keymap) "Filter-lines mode keybindings")
; (suppress-keymap modern-modal/keymap) ; turn off all printing characters, eg 'p'

; m-q shadowed by modern mode. but m-4 isn't
; (define-key filter-lines/keymap "C-M-q" (lambda () (interactive) (filter-lines-mode 0)))


(define-minor-mode filter-lines-mode
  "A minor mode to selectively hide lines matching a regexp."
  ; :lighter " Filter"
  ; :lighter (:eval (propertize " Filter" 'face 'mode-line-emphasis))
  :lighter (:eval (propertize " {Filter}" 'face 'mode-line-emphasis))
  :keymap filter-lines/keymap
  :global t
  ; body is run every time mode is turned on or off
  (message (if filter-lines-mode "Filter on" "Filter off"))
  (if filter-lines-mode
    (progn
      ; (set-hl-line-face 'hl-line-modal)
      ; (setq modern-modal/save-cursor-color (face-attribute 'cursor :background)) ; save current color
      ; (set-cursor-color (face-attribute 'cursor-modal :background))
      ; (setq cua-normal-cursor-color (face-attribute 'cursor-modal :background)) ; in case using cua cursor colors
      ; (line-numbers-show)
      ;. remember lnm state
      ; (line-number-mode 1)
      (view-line-numbers 1)
      (call-interactively 'filter-lines)
      )
    (progn
      ; (set-hl-line-face 'hl-line)
      ; (line-number-mode 0)
      (view-line-numbers 0)
      (filter-lines) ; show lines
      ))
)

; (filter-lines-mode)
; (filter-lines-mode 1)
; (filter-lines-mode 0)

(global-set-key (kbd "M-4") 'filter-lines-mode)



(provide 'filter-lines)


;;;; hide-lines package
; (require 'hide-lines)
;. nowork interactively. fix. 
; (hide-lines "filter")
; (hide-lines "lines")
; (hide-lines)
; (hide-lines-not-matching "lines")
;; (defun filter-lines ...)
;. nowork
; (global-set-key (kbd "C-c /") 'hide-lines)

