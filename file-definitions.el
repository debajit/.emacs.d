;;
;; The Tasks directory is ~/Documents/Tasks.
;;

(setq tasks-directory "~/Projects/Tasks")

(let ((default-directory tasks-directory))
  (setq my-diary-file (expand-file-name "diary")
        inbox-tasks-file (expand-file-name "inbox.org")
        events-file (expand-file-name "events.org")
        home-tasks-file (expand-file-name "home.org")
        work-tasks-file (expand-file-name "work.org")
        finances-tasks-file (expand-file-name "finances.org")
        projects-tasks-file (expand-file-name "projects.org")
        people-tasks-file (expand-file-name "people.org")
        home-journal-file (expand-file-name "home-log.org")
        work-journal-file (expand-file-name "work-log.org")
        work-retrospective-file (expand-file-name "work-retrospective.org")
        ))
