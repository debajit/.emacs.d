(add-to-list 'load-path "~/.emacs.d/mu4e/")

(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-maildir (expand-file-name "~/.mail"))
(setq mu4e-drafts-folder "/gmail/[Gmail].Drafts")
(setq mu4e-sent-folder   "/gmail/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/gmail/[Gmail].Trash")

;;command used to get mail
;; use this for testing
;; (setq mu4e-get-mail-command "true")
;; use this to sync with mbsync
(setq mu4e-get-mail-command "mbsync -a")

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX"             . ?i)
        ("/gmail/[Gmail].Sent Mail" . ?s)
        ("/gmail/[Gmail].Trash"     . ?t)))

;; Function by Marc Bowes
(defun marc/mu4e-update-mail-and-index ()
  "This is the same as `mu4e-hide-index-messages' but with some
better support for running it in a timer, e.g. it is quieter and
handles slow updates better (doesn't snowball)."
  (when mu4e-get-mail-command
    ;; don't update when reading mail - it's annoying
    (unless (and (buffer-live-p mu4e~update-buffer)
             (process-live-p (get-buffer-process mu4e~update-buffer)))
      (run-hooks 'mu4e-update-pre-hook)
      (mu4e~update-mail-and-index-real t))))

;; (marc/mu4e-update-mail-and-index)

;; (run-with-timer 10 300 #'marc/mu4e-update-mail-and-index)
