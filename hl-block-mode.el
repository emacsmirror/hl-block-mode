;;; hl-block-mode.el --- minor mode for highlighting blocks

;; Author: Campbell Barton <ideasman42@gmail.com>
;; Version: 0.1

;;; Commentary:

;; TODO.
;; - More control of alpha blending.
;; - Options to highlight bracket types besides '{}'.

;;; Code:

(defcustom hl-block-delay 0.3
  "Idle time before highlighting."
  :group 'hl-block-mode
  :type  'float)
(defcustom hl-block-color-tint "#040404"
  "Color to add/subtract from the background each scope step."
  :group 'hl-block-mode
  :type  'float)

(defun hl-block--syntax-prev-curly-brace (pt)
  (let
    ((start (ignore-errors (elt (syntax-ppss pt) 1)))
      )
    (when start
      (if (char-equal ?{ (char-after start))
        start
        (hl-block--syntax-prev-curly-brace (1- start))
        )
      )
    )
  )
(defun hl-block--find-all-ranges (pt)
  "Return a list of ranges starting from 'pt', outer-most to inner-most."
  (let*
    (
      (start
        ;; (ignore-errors (elt (syntax-ppss pt) 1)))  ;; works for lisp
        (hl-block--syntax-prev-curly-brace pt))
      (end
        (when start (or (ignore-errors (scan-sexps start 1)) pt)))
      (range_prev
        (when start (hl-block--find-all-ranges start)))
      )
    (when start
      (if range_prev
        (cons (list start end) range_prev)
        (list (list start end))
        )
      )
    )
  )
(defun hl-block--color-values-as-string (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format
    "#%02x%02x%02x"
    (ash r -8)
    (ash g -8)
    (ash b -8)))
(defun hl-block--overlay-clear ()
  (when (boundp 'hl-block-overlay)
    (mapc 'delete-overlay hl-block-overlay))
  (setq hl-block-overlay (list))
  )
(defun hl-block--overlay-refresh ()
  (hl-block--overlay-clear)
  (let
    ((block-list (save-excursion (hl-block--find-all-ranges (point)))))
    (when block-list
      (let*
        (
          ;; (start-prev (point-min))
          ;; (end-prev (point-max))
          (block-list
            (if (cdr block-list)
              (reverse block-list)
              (cons (list (point-min) (point-max)) block-list)
              )
            )
          (start-prev (nth 0 (nth 0 block-list)))
          (end-prev (nth 1 (nth 0 block-list)))
          (block-list-len (length block-list))
          (block-list-last (1- block-list-len))
          (bg-color (color-values (face-attribute 'default :background)))
          (bg-color-tint (color-values hl-block-color-tint))
          ;; Check dark background is light/dark.
          (do-highlight (> 98304 (apply '+ bg-color)))
          )
        (seq-map-indexed
          (lambda (elem_range i)
            (let*
              (
                (i-next (1+ i))
                (i-tint (- block-list-len i))
                (start (nth 0 elem_range))
                (end (nth 1 elem_range))
                (elem-overlay-start (make-overlay start start-prev))
                (elem-overlay-end (make-overlay end-prev end))
                (bg-color-blend
                  (apply 'hl-block--color-values-as-string
                    (if do-highlight
                      (cl-mapcar '(lambda (a b) (+ a (* i-tint b))) bg-color bg-color-tint)
                      (cl-mapcar '(lambda (a b) (- a (* i-tint b))) bg-color bg-color-tint)
                      )
                    )
                  )
                )
              (overlay-put elem-overlay-start 'face `(:background ,bg-color-blend))
              (overlay-put elem-overlay-end 'face `(:background ,bg-color-blend))
              (add-to-list 'hl-block-overlay elem-overlay-start)
              (add-to-list 'hl-block-overlay elem-overlay-end)
              (setq start-prev start)
              (setq end-prev end)
              )
            )
          (cdr block-list)
          )
        )
      )
    )
  )
;; Timer
(defvar hl-block--delay-timer nil)
(defun hl-block--overlay-delay ()
  (when (timerp hl-block--delay-timer)
    (cancel-timer hl-block--delay-timer))
  (setq hl-block--delay-timer
    (run-with-idle-timer hl-block-delay t
      'hl-block--overlay-refresh)
    )
  )
(defun hl-block-mode-enable ()
  (add-hook 'post-command-hook 'hl-block--overlay-delay)
  )
(defun hl-block-mode-disable ()
  (hl-block--overlay-clear)
  (when (timerp hl-block--delay-timer)
    (cancel-timer hl-block--delay-timer))
  (remove-hook 'post-command-hook 'hl-block--overlay-delay)
  )

;;;###autoload
(define-minor-mode hl-block-mode
  "Highlight block under the cursor."
  :lighter ""
  (if hl-block-mode
    (progn
      (jit-lock-unregister 'hl-block-mode-enable)
      (hl-block-mode-enable))
    (progn
      (jit-lock-unregister 'hl-block-mode-enable)
      (hl-block-mode-disable))))

(provide 'hl-block-mode)
;;; hl-block-mode.el ends here
