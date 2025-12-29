;;; hl-block-mode.el --- Highlighting nested blocks -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2019-2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>
;; URL: https://codeberg.org/ideasman42/emacs-hl-block-mode
;; Keywords: convenience, faces
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Highlight blocks surrounding the cursor.

;;; Usage:

;; (hl-block-mode)        ; Activate in the current buffer.
;; (hl-block-global-mode) ; Activate globally for all buffers.

;;; Code:

(eval-when-compile
  ;; For pcase macros.
  (require 'pcase))


;; ---------------------------------------------------------------------------
;; Compatibility

(eval-when-compile
  (when (version< emacs-version "31.1")
    (defmacro incf (place &optional delta)
      "Increment PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(+ ,getter ,(or delta 1)))))
    (defmacro decf (place &optional delta)
      "Decrement PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(- ,getter ,(or delta 1)))))))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup hl-block nil
  "Highlight nested blocks or brackets."
  :group 'convenience)

(defcustom hl-block-bracket "{"
  "Characters to use as starting brackets (set to nil to use all brackets)."
  :type '(choice (const nil) string))

(defcustom hl-block-delay 0.2
  "Idle time to wait before highlighting (in seconds)."
  :type 'number)

(defcustom hl-block-multi-line nil
  "When non-nil, skip highlighting nested blocks on the same line.

Useful for languages that use S-expressions to avoid overly nested highlighting."
  :type 'boolean)

(defcustom hl-block-single-level nil
  "When non-nil, highlight only a single level instead of all levels."
  :type 'boolean)

(defcustom hl-block-style 'color-tint
  "Style used for highlighting blocks."
  :type
  '(choice (const :tag "Tint the background at each level." color-tint)
           (const :tag "Highlight surrounding brackets." bracket)))

;; Used with the `color-tint' draw style.
(defcustom hl-block-color-tint "#040404"
  "Color to add/subtract from the background each scope step."
  :type 'color)

;; Used with the `bracket' draw style.
(defcustom hl-block-bracket-face '(:inverse-video t)
  "Face used when `hl-block-style' is set to `bracket'."
  :type '(choice face plist))

(defcustom hl-block-mode-lighter ""
  "Lighter for `hl-block-mode'."
  :type 'string)


;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar-local hl-block--overlays nil
  "List of overlays used for highlighting.")


;; ---------------------------------------------------------------------------
;; Internal Bracket Functions

(defun hl-block--syntax-prev-bracket (pt)
  "Find the previous bracket position matching `hl-block-bracket'.
PT is typically the result of calling `point'."
  (declare (important-return-value t))
  (when-let* ((beg
               (ignore-errors
                 (nth 1 (syntax-ppss pt)))))
    (cond
     ((memq (char-after beg) hl-block-bracket)
      beg)
     (t
      (hl-block--syntax-prev-bracket (1- beg))))))


(defun hl-block--find-range (pt)
  "Return range around PT or nil."
  (declare (important-return-value t))
  (let ((beg
         (cond
          (hl-block-bracket
           (hl-block--syntax-prev-bracket pt))
          (t
           (ignore-errors
             (nth 1 (syntax-ppss pt)))))))
    (when beg
      ;; Note that `end' may be nil for unmatched brackets.
      ;; The caller must handle this case.
      (let ((end
             (ignore-errors
               (scan-sexps beg 1))))
        (cons beg end)))))


(defun hl-block--find-all-ranges (pt)
  "Return ranges starting from PT, innermost to outermost."
  (declare (important-return-value t))
  (when-let* ((range (hl-block--find-range pt)))
    ;; When no further range is found, recursion terminates.
    (cons range (hl-block--find-all-ranges (car range)))))


(defun hl-block--find-single-range (pt)
  "Return a list with the innermost range around PT."
  (declare (important-return-value t))
  (when-let* ((range (hl-block--find-range pt)))
    (list range)))


(defun hl-block--syntax-skip-to-multi-line ()
  "Move point to the first multi-line block.

The point will only ever be moved backward."
  (declare (important-return-value nil))
  (let ((line-min (pos-bol))
        (line-max (pos-eol))
        (beg (point))
        (end (point)))
    (while (and beg (>= beg line-min) end (<= end line-max))
      (setq beg
            (ignore-errors
              (nth 1 (syntax-ppss beg))))
      (when beg
        (setq end
              (ignore-errors
                (scan-sexps beg 1)))))))


;; ---------------------------------------------------------------------------
;; Internal Color Tint (Draw Style)

(defun hl-block--color-values-as-string (color)
  "Convert COLOR vector to a hex color string.
COLOR is a 3-element vector of RGB values."
  (declare (important-return-value t))
  (format "#%02x%02x%02x" (ash (aref color 0) -8) (ash (aref color 1) -8) (ash (aref color 2) -8)))


(defun hl-block--color-tint-add (a b tint)
  "Add B scaled by TINT to color A, making it lighter."
  (declare (important-return-value t))
  (vector
   (+ (aref a 0) (* tint (aref b 0)))
   (+ (aref a 1) (* tint (aref b 1)))
   (+ (aref a 2) (* tint (aref b 2)))))


(defun hl-block--color-tint-sub (a b tint)
  "Subtract B scaled by TINT from color A, making it darker."
  (declare (important-return-value t))
  (vector
   (- (aref a 0) (* tint (aref b 0)))
   (- (aref a 1) (* tint (aref b 1)))
   (- (aref a 2) (* tint (aref b 2)))))


(defun hl-block--overlay-create-color-tint (block-list end-fallback)
  "Create overlays with tinted backgrounds for nested blocks.
Argument BLOCK-LIST represents start-end ranges of brackets.
Argument END-FALLBACK is the point used when no matching end bracket is found,
typically the result of calling `point'."
  (declare (important-return-value nil))
  (let* ((block-list-len (length block-list))
         (bg-color (apply #'vector (color-values (face-attribute 'default :background))))
         (bg-color-tint (apply #'vector (color-values hl-block-color-tint)))
         ;; Check if background is light or dark.
         ;; 98304 = 65535 * 1.5, i.e. 50% of max RGB sum (65535 * 3).
         (do-highlight (> 98304 (+ (aref bg-color 0) (aref bg-color 1) (aref bg-color 2))))
         ;; Tint level counter.
         (i 0))
    (pcase-let ((`(,beg-prev . ,end-prev) (pop block-list)))
      (unless end-prev ; May be `nil' for unmatched brackets.
        (setq end-prev end-fallback))
      (while block-list
        (pcase-let ((`(,beg . ,end) (pop block-list)))
          (unless end ; May be `nil' for unmatched brackets.
            (setq end end-fallback))
          (let ((elem-overlay-beg (make-overlay beg beg-prev))
                (elem-overlay-end (make-overlay end-prev end)))

            ;; Calculate the face with the tint color at this highlight level.
            (let ((hl-face
                   (list
                    :background
                    (hl-block--color-values-as-string
                     (let ((i-tint (- block-list-len i)))
                       (cond
                        (do-highlight
                         (hl-block--color-tint-add bg-color bg-color-tint i-tint))
                        (t
                         (hl-block--color-tint-sub bg-color bg-color-tint i-tint)))))
                    :extend t)))

              (overlay-put elem-overlay-beg 'face hl-face)
              (overlay-put elem-overlay-end 'face hl-face))

            (push elem-overlay-beg hl-block--overlays)
            (push elem-overlay-end hl-block--overlays)
            (setq beg-prev beg)
            (setq end-prev end))
          (incf i))))))


;; ---------------------------------------------------------------------------
;; Internal Bracket (Draw Style)

(defun hl-block--overlay-create-bracket (block-list)
  "Create overlays to highlight surrounding brackets.
Argument BLOCK-LIST represents start-end ranges of brackets."
  (declare (important-return-value nil))
  (pcase-dolist (`(,beg . ,end) block-list)
    (let ((elem-overlay-beg (make-overlay beg (1+ beg))))
      (overlay-put elem-overlay-beg 'face hl-block-bracket-face)
      (push elem-overlay-beg hl-block--overlays)
      (when end ; May be `nil' for unmatched brackets.
        (let ((elem-overlay-end (make-overlay (1- end) end)))
          (overlay-put elem-overlay-end 'face hl-block-bracket-face)
          (push elem-overlay-end hl-block--overlays))))))


;; ---------------------------------------------------------------------------
;; Internal Refresh Function

(defun hl-block--overlay-clear ()
  "Clear all overlays."
  (declare (important-return-value nil))
  (mapc #'delete-overlay hl-block--overlays)
  (setq hl-block--overlays nil))


(defun hl-block--overlay-refresh ()
  "Update the overlays based on the cursor location."
  (declare (important-return-value nil))
  (hl-block--overlay-clear)
  (let ((block-list
         (save-excursion
           (when hl-block-multi-line
             (hl-block--syntax-skip-to-multi-line))
           (cond
            (hl-block-single-level
             (hl-block--find-single-range (point)))
            (t
             (hl-block--find-all-ranges (point)))))))

    (when block-list
      (cond
       ((eq hl-block-style 'color-tint)
        ;; Ensure outer bounds (when only one pair exists).
        (setq block-list
              (cond
               ((cdr block-list)
                (nreverse block-list))
               (t
                (cons (cons (point-min) (point-max)) block-list))))
        (hl-block--overlay-create-color-tint block-list (point)))
       ((eq hl-block-style 'bracket)
        (hl-block--overlay-create-bracket block-list))
       (t
        (error "Unknown style %S" hl-block-style))))))


;; ---------------------------------------------------------------------------
;; Internal Timer Management
;;
;; This works as follows:
;;
;; - The timer is kept active as long as the local mode is enabled.
;; - Entering a buffer runs the buffer-local `window-state-change-hook'
;;   immediately which checks if the mode is enabled,
;;   sets up the global timer if it is.
;; - Switching any other buffer won't run this hook,
;;   we rely on the idle timer itself running, which detects the active mode,
;;   canceling itself if the mode isn't active.
;;
;; This is a reliable way of using a global,
;; repeating idle timer that is effectively buffer-local.
;;

(defvar hl-block--global-timer nil
  "Global idle timer for highlighting, active while buffer-local mode is enabled.")

(defvar hl-block--dirty-flush-all nil
  "When non-nil, the timer will update all dirty buffers in visible windows.")

(defvar-local hl-block--dirty nil
  "When non-nil, the buffer should be updated when inactive.")

(defun hl-block--time-callback-or-disable ()
  "Timer callback that refreshes highlighting."
  (declare (important-return-value nil))

  ;; Ensure all other buffers are highlighted on request.
  (let ((is-mode-active (bound-and-true-p hl-block-mode)))
    ;; When this buffer is not in the mode, flush all other buffers.
    (cond
     (is-mode-active
      ;; Don't update in the window loop to ensure we always
      ;; update the current buffer in the current context.
      (setq hl-block--dirty nil))
     (t
      ;; If the timer ran when in another buffer,
      ;; a previous buffer may need a final refresh, ensure this happens.
      (setq hl-block--dirty-flush-all t)))

    (when hl-block--dirty-flush-all
      ;; Refresh overlays for all dirty buffers in visible windows.
      (dolist (frame (frame-list))
        (when (frame-live-p frame)
          (dolist (win (window-list frame -1))
            (let ((buf (window-buffer win)))
              (when (and (buffer-local-value 'hl-block-mode buf)
                         (buffer-local-value 'hl-block--dirty buf))
                (with-selected-frame frame
                  (with-selected-window win
                    (with-current-buffer buf
                      (setq hl-block--dirty nil)
                      (hl-block--overlay-refresh))))))))))
    ;; Always keep the current buffer dirty
    ;; so navigating away from this buffer will refresh it.
    (when is-mode-active
      (setq hl-block--dirty t))

    (cond
     (is-mode-active
      (hl-block--overlay-refresh))
     (t ;; Cancel the timer until the current buffer uses this mode again.
      (hl-block--time-ensure nil)))))

(defun hl-block--time-ensure (state)
  "Ensure the timer is enabled when STATE is non-nil, otherwise disable."
  (declare (important-return-value nil))
  (cond
   (state
    (unless hl-block--global-timer
      (setq hl-block--global-timer
            (run-with-idle-timer hl-block-delay :repeat #'hl-block--time-callback-or-disable))))
   (t
    (when hl-block--global-timer
      (cancel-timer hl-block--global-timer)
      (setq hl-block--global-timer nil)))))

(defun hl-block--time-reset ()
  "Reset timer state when window state changes."
  (declare (important-return-value nil))
  ;; Ensure changing windows doesn't leave other buffers with stale highlighting.
  (cond
   ((bound-and-true-p hl-block-mode)
    (setq hl-block--dirty-flush-all t)
    (setq hl-block--dirty t)
    (hl-block--time-ensure t))
   (t
    (hl-block--time-ensure nil))))

(defun hl-block--time-buffer-local-enable ()
  "Ensure buffer-local state is enabled."
  (declare (important-return-value nil))
  ;; Needed in case focus changes before the idle timer runs.
  (setq hl-block--dirty-flush-all t)
  (setq hl-block--dirty t)
  (hl-block--time-ensure t)
  (add-hook 'window-state-change-hook #'hl-block--time-reset nil t))

(defun hl-block--time-buffer-local-disable ()
  "Ensure buffer-local state is disabled."
  (declare (important-return-value nil))
  (kill-local-variable 'hl-block--dirty)
  (hl-block--time-ensure nil)
  (remove-hook 'window-state-change-hook #'hl-block--time-reset t))

;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun hl-block--mode-enable ()
  "Turn on `hl-block-mode' for the current buffer."
  (declare (important-return-value nil))
  (hl-block--time-buffer-local-enable)

  ;; Set up brackets:
  ;; Keep as nil to match all brackets,
  ;; otherwise convert the string to a list of characters.
  (let ((bracket-orig (append hl-block-bracket nil)))
    ;; Make a local, sanitized version of this variable.
    (setq-local hl-block-bracket nil)
    (when bracket-orig
      ;; Filter for characters with open-bracket syntax.
      (while bracket-orig
        (let ((ch (pop bracket-orig)))
          (when (eq ?\( (char-syntax ch))
            (push ch hl-block-bracket)))))))

(defun hl-block--mode-disable ()
  "Turn off `hl-block-mode' for the current buffer."
  (declare (important-return-value nil))
  (hl-block--overlay-clear)
  (kill-local-variable 'hl-block--overlays)
  (kill-local-variable 'hl-block-bracket)
  (hl-block--time-buffer-local-disable))

(defun hl-block--mode-turn-on ()
  "Enable `hl-block-mode' in the current buffer."
  (declare (important-return-value nil))
  (when (and (null (minibufferp)) (not (bound-and-true-p hl-block-mode)))
    (hl-block-mode 1)))

;; ---------------------------------------------------------------------------
;; Public API

;;;###autoload
(define-minor-mode hl-block-mode
  "Highlight blocks surrounding the cursor."
  :global nil
  :lighter hl-block-mode-lighter

  (cond
   (hl-block-mode
    (hl-block--mode-enable))
   (t
    (hl-block--mode-disable))))

;;;###autoload
(define-globalized-minor-mode hl-block-global-mode
  hl-block-mode
  hl-block--mode-turn-on)

(define-obsolete-function-alias 'global-hl-block-mode #'hl-block-global-mode "0.2")

(provide 'hl-block-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; hl-block-mode.el ends here
