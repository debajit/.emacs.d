;;; -*- lexical-binding: t -*-
;;
;; ~/.emacs.d/selection.el
;;
;; Selection settings.
;;

;;----------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------

;; Cut: s-x   (default: C-w)
(global-set-key (kbd "s-x") 'kill-region)

;; Copy: s-c   (default: M-w)
(global-set-key (kbd "s-c") 'kill-ring-save)

;; Paste: s-v   (default: C-y)
(global-set-key (kbd "s-v") 'yank)

;; Select all: s-a   (default: C-x h)
(global-set-key (kbd "s-a") 'mark-whole-buffer)

;; Delimiter-aware selection and killing.

(defconst mark-inner--delimiter-pairs
  '((?\( . ?\))
    (?\{ . ?\})
    (?\[ . ?\])
    (?\' . ?\')
    (?\" . ?\")
    (?\` . ?\`))
  "Opening and closing delimiters recognized by `mark-inner'.")

(defvar-local mark-inner--delimiter-cache-key nil)
(defvar-local mark-inner--delimiter-cache nil)

(defun mark-inner--balanced-pairs ()
  "Return balanced delimiter positions in the accessible buffer.

Each result is an (OPEN . CLOSE) pair.  Results are cached until the
buffer changes or its accessible portion changes."
  (let ((cache-key (list (buffer-chars-modified-tick)
                         (point-min)
                         (point-max))))
    (unless (equal cache-key mark-inner--delimiter-cache-key)
      (setq mark-inner--delimiter-cache-key cache-key
            mark-inner--delimiter-cache
            (save-excursion
              (goto-char (point-min))
              (let ((backslashes 0)
                    stack
                    pairs)
                (while (< (point) (point-max))
                  (let ((character (char-after))
                        (position (point)))
                    (if (eq character ?\\)
                        (setq backslashes (1+ backslashes))
                      (when (zerop (% backslashes 2))
                        (let ((top (car stack)))
                          (cond
                           ((and top (= character (cdr top)))
                            (push (cons (car top) position) pairs)
                            (pop stack))
                           ((assq character mark-inner--delimiter-pairs)
                            (push (cons position
                                        (cdr (assq character
                                                   mark-inner--delimiter-pairs)))
                                  stack)))))
                      (setq backslashes 0)))
                  (forward-char 1))
                pairs))))
    mark-inner--delimiter-cache))

(defun mark-inner--current-bounds ()
  "Return the active region, or point as an empty region."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (point) (point))))

(defun mark-inner--smallest-enclosing-pair (&optional contents-only point-inside)
  "Return the smallest delimiter pair enclosing the current bounds.

When CONTENTS-ONLY is non-nil, require the current bounds to fit
strictly between the delimiters.  When POINT-INSIDE is non-nil and
there is no active region, do not match a pair immediately before
point."
  (let* ((bounds (mark-inner--current-bounds))
         (start (car bounds))
         (end (cdr bounds))
         best
         best-size)
    (dolist (pair (mark-inner--balanced-pairs) best)
      (let* ((open (car pair))
             (close (cdr pair))
             (candidate-start (if contents-only (1+ open) open))
             (candidate-end (if contents-only close (1+ close)))
             (candidate-size (- candidate-end candidate-start)))
        (when (and (<= candidate-start start)
                   (>= candidate-end end)
                   (or (not point-inside)
                       (use-region-p)
                       (<= start close))
                   (or (null best-size) (< candidate-size best-size)))
          (setq best pair
                best-size candidate-size))))))

(defun mark-inner--mark-delimited-contents ()
  "Mark the contents of the smallest enclosing delimiter pair."
  (let ((pair (mark-inner--smallest-enclosing-pair t)))
    (when pair
      (goto-char (1+ (car pair)))
      (set-mark (cdr pair)))))

(defun mark-inner--mark-delimited-expression ()
  "Mark the smallest enclosing delimiter pair and its contents."
  (let ((pair (mark-inner--smallest-enclosing-pair)))
    (when pair
      (goto-char (car pair))
      (set-mark (1+ (cdr pair))))))

(defun mark-inner (arg)
  "Expand the region through nested delimiter boundaries.

This extends `er/expand-region' with mode-independent support for
quotes, backticks, parentheses, braces, and brackets.  With prefix
ARG, expand that many times."
  (interactive "p")
  (require 'expand-region)
  (let ((er/try-expand-list
         (append '(mark-inner--mark-delimited-contents
                   mark-inner--mark-delimited-expression)
                 er/try-expand-list))
        ;; Keep repeated M-i presses routed through this wrapper so
        ;; the custom expansion functions remain available.
        (expand-region-fast-keys-enabled nil)
        (last-command (if (eq last-command 'mark-inner)
                          'er/expand-region
                        last-command)))
    (er/expand-region arg)))

(defun kill-inner ()
  "Kill the contents of the smallest enclosing delimiter pair.

The delimiters remain in the buffer and the killed text is placed in
the kill ring."
  (interactive)
  (let ((pair (mark-inner--smallest-enclosing-pair nil t)))
    (unless pair
      (user-error "Point or region is not inside balanced delimiters"))
    (let ((start (1+ (car pair)))
          (end (cdr pair)))
      (if (= start end)
          (message "Delimiter pair is already empty")
        (kill-region start end))
      (goto-char start)
      (deactivate-mark))))

(global-set-key (kbd "M-i") 'mark-inner)
(global-set-key (kbd "M-D") 'kill-inner)

;;----------------------------------------------------------------------
;; Narrowing and widening
;;----------------------------------------------------------------------

; Enable narrowing (disabled by default)
(put 'narrow-to-region 'disabled nil)

;; ;; TODO: If something is selected it should narrow to selection instead
;; (global-set-key (kbd "s-n") 'narrow-to-defun)
;; (global-set-key (kbd "s-N") 'widen)
