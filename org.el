;; Allow bold, italic, code etc. markup to be inserted easily via
;; electric pairs.
;;
;; Taken from https://emacs.stackexchange.com/a/10034/12922

(defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=)
                             (?\_ . ?\_) (?~ . ?~) (?+ . ?+)) "Electric pairs for org-mode.")

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)
