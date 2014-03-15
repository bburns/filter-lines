; filter-lines.el
; filter lines interactively
;
;;; Description
;
; Turn on mode, start typing and incrementally hide lines not
; containing the string.
; A buffer-local minor mode.
;
; See also: 
; keep-lines, flush-lines - emacs fns which delete lines from a buffer
; hide-lines.el - hides lines matching or not matching a regexp
;   this mode is an interactive version. 
;
; Author: Brian Burns <bburns.km@gmail.com>
; Site: https://github.com/bburns/filter-lines
; License: GNU - see end of file
; Date: 2014-01





;;; Keybindings

; m-f m-l, m-f m-f?
; (global-set-key (kbd "M-F") 'filter-lines-mode) ; shadowed by modern

(defun filter-lines-set-keys () (global-set-key (kbd "<f12>") 'filter-lines-mode))


; m-q shadowed by modern mode. but m-4 isn't
; (define-key filter-lines-keymap "C-M-q" (lambda () (interactive) (filter-lines-mode 0)))

; (lookup-key filter-lines-keymap (kbd "a"))
; (lookup-key filter-lines-keymap (kbd "DEL"))
; (lookup-key filter-lines-keymap (kbd "<backspace>"))


(defvar filter-lines-keymap
  (let ((map (make-keymap)) i)
    ; printing chars extend the search string
    (setq i ?\s) ; space
    (while (< i 256)
      (define-key map (vector i) 'filter-lines-add-char)
      (setq i (1+ i)))
    ; add some other events
    (define-key map (kbd "C-j") 'filter-lines-add-char)
    (define-key map (kbd "<tab>") 'filter-lines-add-char) ; \t
    (define-key map (kbd "<backspace>") 'filter-lines-remove-char)
    (define-key map (kbd "DEL") 'filter-lines-remove-char)
    (define-key map (kbd "C-g") 'filter-lines-abort)
    ; (define-key map (kbd "<f1>") 'filter-lines-help-map)
    map)
  "Filter-lines mode keybindings")


; (describe-keymap 'filter-lines-keymap)

; (string-to-char (kbd "DEL")) ; 127


;;; Private functions


(defvar filter-lines-prompt "Filter lines matching: ")
(defvar filter-lines-string nil)
; (defvar filter-lines-regexp nil)

; (substring "penguin" 0 -1)

(defun filter-lines-start ()
  (message filter-lines-prompt))


(defun filter-lines-add-char ()
  "Add last typed character to the search string and search."
  (interactive)
  (let ((char last-command-event))
    (unhighlight-regexp filter-lines-string)
    (setq filter-lines-string (concat filter-lines-string (char-to-string char)))
    (filter-lines-search)))

(defun filter-lines-remove-char ()
  "Remove last letter from search string and re-search."
  (interactive)
  (unhighlight-regexp filter-lines-string)
    (setq filter-lines-string (substring filter-lines-string 0 -1))
    (filter-lines-search))

(defun filter-lines-search ()
  "Search on filter-lines-string - filter lines and highlight occurrences."
  (message (concat filter-lines-prompt filter-lines-string))
  (filter-lines filter-lines-string)
  (highlight-regexp filter-lines-string))



; (defun filter-lines-search (s)
;     (filter-lines s)
;     (highlight-regexp s))
; (filter-lines-search "lines")
; (filter-lines-search "")
; (unhighlight-regexp "lines")
; (unhighlight-regexp "line")
; (unhighlight-regexp "lin")
; (unhighlight-regexp "li")
; (unhighlight-regexp "l")


(defun filter-lines-stop ()
  (unhighlight-regexp filter-lines-string)
  (filter-lines "")
  (setq filter-lines-string nil)
  (message "Filter lines mode off"))

(defun filter-lines-abort ()
  (filter-lines-stop))
  ; (jumpbacktostart))


(defun filter-lines-set-invisible (start end)
  "Mark text as invisible"
  (overlay-put (make-overlay start end) 'invisible 'filter-lines))

(defun filter-lines-set-visible (start end)
  "Mark text as visible"
  (remove-overlays start end 'invisible 'filter-lines)) ; clear flags


(defun filter-lines (&optional regexp)
  "Hide lines that don't match REGEXP. Call with no argument or nil to restore them."
  ; some logic from hide-lines
  ; (interactive "MShow only lines matching regexp: ")
  ; (interactive nil)
  (if (or (null regexp) (string= "" regexp))
      (progn
        (remove-from-invisibility-spec 'filter-lines) ; show flagged lines
        ; (remove-overlays (point-min) (point-max) 'invisible 'filter-lines)) ; clear flags
        (filter-lines-set-visible (point-min) (point-max)))
    (save-excursion
      (goto-char (point-min))
      (let ((start (point-min))
             ; re-search sets point to end of match, returns point. t means return nil if fail
            (pos (re-search-forward regexp nil t)))
        (while pos
          (beginning-of-line)
          ; (funcall flag-lines start (point))
          (filter-lines-flag-text start (point))
          (forward-line 1) ; move to start of next line
          (setq start (point))
          (if (eq (point) (point-max))
              (setq pos nil)
            (setq pos (re-search-forward regexp nil t))))
        ; (funcall filter-lines-flag-text start (point-max))
        (filter-lines-set-invisible start (point-max))
        (add-to-invisibility-spec 'filter-lines) ; hide flagged lines
        ))))

; for testing
(global-set-key (kbd "<C-f12>") 'filter-lines)
; (filter-lines "lines")
; (filter-lines "asdf")
; (filter-lines)

; (defun mark-invisible (start end) (overlay-put (make-overlay start end) 'invisible 'filter-lines))
; (defun hide-lines () (add-to-invisibility-spec 'filter-lines))
; (defun show-lines () (remove-from-invisibility-spec 'filter-lines))
; (defun mark-visible () (remove-overlays (point-min) (point-max) 'invisible 'filter-lines))


;;; Mode

(define-minor-mode filter-lines-mode
  "A minor mode to interactively hide lines not matching a regexp."
  ; :lighter " Filter"
  ; :lighter (:eval (propertize " Filter " 'face 'mode-line-emphasis))
  ; :lighter (:eval (concat " " (propertize " Filter " 'face 'mode-line-emphasis)))
  ; :lighter (:eval (concat " " (propertize "Filter" 'face 'mode-line-emphasis)#("Filter" 0 6 (face mode-line-emphasis))))
  ; :lighter (:eval (concat " " "hi"))
  ; :lighter (:eval (concat " " (concat "hi" "there")))
  :lighter #(" Filter" 0 3 (face mode-line-emphasis)) ; colors whole word
  ; :lighter #(" Filter" 1 6 (face mode-line-emphasis))
  :keymap filter-lines-keymap
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
      ; (call-interactively 'filter-lines) ; asks user for search string
      (filter-lines-start)
      )
    (progn
      ; (set-hl-line-face 'hl-line)
      ; (line-number-mode 0)
      ; (view-line-numbers 0)
      ; (filter-lines) ; show hidden lines
      (filter-lines-stop)
      )))

; (filter-lines-mode)
; (filter-lines-mode 1)
; (filter-lines-mode 0)



;;; Test

; (require 'hide-lines)
;. nowork interactively. fix. 
; (hide-lines "filter")
; (hide-lines "lines")
; (hide-lines)
; (hide-lines-not-matching "lines")
; (defun filter-lines ...)
;. nowork
; (global-set-key (kbd "C-c /") 'hide-lines)


;;; Provide

(provide 'filter-lines)


;;; License
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

; eof
