(use-package doct
  :commands (doct))

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)


(with-eval-after-load 'org-capture
  (setq org-capture-templates
          (doct `((,(format "%s\tPersonal todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "t"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/todo.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
		   :todo-state "TODO"
		   :template ("* %{todo-state} %^{Description}\n SCHEDULED: %^{Scheduled to begin}t DEADLINE: %^{Deadline}T"
			      ":PROPERTIES:"
			      ":Created: %U"
			      ":END:"
			      "%?")
                   )
		  (,(format "%s\tPayments" (all-the-icons-faicon "money" :face 'all-the-icons-red :v-adjust 0.01))
                   :keys "p"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/money.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
		   :todo-state "TODO"
                   :template ("* %{todo-state} [payment] %^{what payment?}\nDEADLINE: %^{Deadline}T")
		   
                   )
		  (,(format "%s\tSomedays" (all-the-icons-faicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
                   :keys "s"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/someday.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
		   :todo-state "TODO"
                   :template ("* %{todo-state} [someday] %^{what topics?}\nDEADLINE: %^{Deadline}T")
		   
                   )		  
		  (,(format "%s\tNLP_class" (all-the-icons-faicon "graduation-cap" :face 'all-the-icons-purple :v-adjust 0.01))
                   :keys "n"
                   :file  "/Users/holy/Dropbox/org/RoamNotes/Gtd/ai.org"
                   :headline "NLP_Class"
                   :prepend t
                   :type entry
                   :children ((,(format "%s\tTest" (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust 0.01))
                               :keys "t"
                               :template ("* TODO [#C] %? :uni:tests:"
                                          "SCHEDULEDD: %^{Test date:}T"
                                          "%i %a"))
                              (,(format "%s\tAssignment" (all-the-icons-material "library_books" :face 'all-the-icons-orange :v-adjust 0.01))
                               :keys "a"
                               :template ("* TODO [#B] %? :uni:assignments:"
                                          "DEADLINE: %^{Due date:}T"
                                          "%i %a"))
                              (,(format "%s\tSummary" (all-the-icons-faicon "bookmark" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "a"
                               :template ("* TODO [#B] %? :uni:assignments:"
                                          "DEADLINE: %^{Due date:}T"
                                          "%i %a"))			      
                              (,(format "%s\tMiscellaneous task" (all-the-icons-faicon "list" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "u"
                               :template ("* TODO [#C] %? :uni:"
                                          "%i %a"))))
		  (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
                   :keys "e"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/email.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %? :email:"
                              "%i %a"))
		  
		  (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
                   :keys "i"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/interesting.org"
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children ((,(format "%s\tWebpage" (all-the-icons-faicon "globe" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "w"
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              (,(format "%s\tArticle" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "a"
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              (,(format "%s\tInformation" (all-the-icons-faicon "info-circle" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "i"
                               :desc ""
                               :i-type "read:info"
                               )
                              (,(format "%s\tIdea" (all-the-icons-material "bubble_chart" :face 'all-the-icons-silver :v-adjust 0.01))
                               :keys "I"
                               :desc ""
                               :i-type "idea"
                               )))
		  (,(format "%s\tRoutines" (all-the-icons-faicon "hourglass" :face 'all-the-icons-red :v-adjust 0.01))
                   :keys "r"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/routines.org"
                   :prepend t
		   :todo-state "TODO"		   
                   :headline "Inbox"
                   :type entry
		   :template ("* %{todo-state} %^{Description}\n SCHEDULED: %^{Scheduled to begin}t DEADLINE: %^{Deadline}T"
			      ":PROPERTIES:"
			      ":Created: %U"
			      ":END:"
			      "%?")		   
                   )
                  (,(format "%s\tPersonal note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "n"
                   :file "/Users/holy/Dropbox/org/RoamNotes/Gtd/email.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a")
                   )
		  ))))
