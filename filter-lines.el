
; filter-lines
; filter lines interactively

;;; description
;
; buffer-local minor mode
;
; author: brian burns <brianburns@utexas.edu>
; url: github...
; history:
; 2014-01 started
;
; see also
; keep-lines, flush-lines
; hide-lines.el
;
; license: gnu. see end of file


;;; notes

; can insert lines in filter mode
; but m-g will carry invisible lines surrounding it also
; because of the way it's written - which is why it works with org subtrees also



;;; private

(defun flag-text (start end) 
  (overlay-put (make-overlay start end) 'invisible 'filter-lines))


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
            (pos (re-search-forward regexp nil t)))
        (while pos
          (beginning-of-line)
          ; (funcall flag-lines start (point))
          (flag-text start (point))
          (forward-line 1) ; move to start of next line
          (setq start (point))
          (if (eq (point) (point-max))
              (setq pos nil)
            (setq pos (re-search-forward regexp nil t))))
        ; (funcall flag-text start (point-max))
        (flag-text start (point-max))
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


;;; keymap

(defvar filter-lines/keymap (make-sparse-keymap) "Filter-lines mode keybindings")
; (suppress-keymap modern-modal/keymap) ; turn off all printing characters, eg 'p'

; m-q shadowed by modern mode. but m-4 isn't
; (define-key filter-lines/keymap "C-M-q" (lambda () (interactive) (filter-lines-mode 0)))

;;; mode

(define-minor-mode filter-lines-mode
  "A minor mode to selectively hide lines matching a regexp."
  ; :lighter " Filter"
  ; :lighter (:eval (propertize " Filter " 'face 'mode-line-emphasis))
  ; :lighter (:eval (concat " " (propertize " Filter " 'face 'mode-line-emphasis)))
  ; :lighter (:eval (concat " " (propertize "Filter" 'face 'mode-line-emphasis)#("Filter" 0 6 (face mode-line-emphasis))))
  ; :lighter (:eval (concat " " "hi"))
  ; :lighter (:eval (concat " " (concat "hi" "there")))
  :lighter #(" Filter" 0 3 (face mode-line-emphasis)) ; colors whole word
  ; :lighter #(" Filter" 1 6 (face mode-line-emphasis))
  :keymap filter-lines/keymap
  :global nil
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
      ; (view-line-numbers 1)
      (call-interactively 'filter-lines)
      )
    (progn
      ; (set-hl-line-face 'hl-line)
      ; (line-number-mode 0)
      ; (view-line-numbers 0)
      (filter-lines) ; show hidden lines
      ))
)

; (filter-lines-mode)
; (filter-lines-mode 1)
; (filter-lines-mode 0)


;.. m-f m-l
(global-set-key (kbd "M-4 f") 'filter-lines-mode)


;;; afterword

(provide 'filter-lines)


;;; hide-lines package
; (require 'hide-lines)
;. nowork interactively. fix. 
; (hide-lines "filter")
; (hide-lines "lines")
; (hide-lines)
; (hide-lines-not-matching "lines")
; (defun filter-lines ...)
;. nowork
; (global-set-key (kbd "C-c /") 'hide-lines)

;;; license
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; see the file COPYING.  If not, write to the
; Free Software Foundation, Inc., 51 Franklin Street, Fifth
; Floor, Boston, MA 02110-1301, USA.
