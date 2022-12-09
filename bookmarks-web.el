;;
;; ~/.emacs.d/bookmarks-web.el
;;
;; Web bookmarks that we want to quickly jump to.
;;

(eval-after-load "webjump"
  '(setq webjump-sites
         (append '(
                   ("Debajit’s Movie Ratings and Reviews" . "http://debajit.com/movies")
                   ("Gmail" . "https://gmail.com/")
                   ("GitHub — Debajit — .emacs.d" . "https://github.com/debajit/.emacs.d")
                   ("Urban Dictionary" . [simple-query "www.urbandictionary.com" "http://www.urbandictionary.com/define.php?term=" ""])
                   ("AWS WorkDocs" . "https://amazon.awsapps.com/workdocs/index.html#/mydocs")
                   )
                 webjump-sites
                 webjump-sample-sites)))
