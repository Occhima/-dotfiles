;;; config.el -*- lexical-binding: t; -*
;;;

;;; Code:

(setq user-full-name "Marco Occhialini"
      base-dir "~/OneDrive"
      projects-dir (concat (file-name-as-directory base-dir) "projects")
      user-mail-address "marcoocchialini@usp.br"
      command-line-default-directory "~/" ; set default directory to home
      org-directory (concat (file-name-as-directory projects-dir) "org")
      +doom-dashboard-pwd-policy projects-dir
      default-directory "~/"
      ns-use-proxy-icon nil             ; empty title
      frame-title-format '"\n" ; use a new-line to make sure rezising info is on the next line
      undo-limit 80000000      ; Raise undo-limit to 80Mb
      evil-want-fine-undo t ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t   ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…" ; Unicode ellispis are nicer than "...", and also save /precious/ space
      display-line-numbers-type 'relative
      which-key-idle-delay 0.3          ; Show key binding help quicker
      which-key-idle-secondary-delay 0)               ; Compile the vterm-module when needed without asking


(after! projectile
  (setq projectile-project-root-files-bottom-up '("package.json" ".projectile" ".project" ".git")
        projectile-ignored-projects '("~/.emacs.d/")
        projectile-project-search-path '(projects-dir))
  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))


(defun +fl/counsel-recentf-candidates (candidates)
  (seq-filter (lambda (f) (not
                      (or (string-match "^/private" f)
                          (string-match "^~/.emacs.d" f)
                          (string-match "^/Applications" f)))
                ) candidates))

(advice-add 'counsel-recentf-candidates :filter-return #'+fl/counsel-recentf-candidates)

(after! popup
  (set-popup-rule! "^\\*Flycheck errors\\*$" :side 'bottom :size 0.2 :select t))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change)))

(after! (flycheck lsp-mode)
  (add-hook 'lsp-after-initialize-hook (lambda()
                                        (flycheck-add-next-checker 'lsp '(warning . javascript-eslint)))))
(after! lsp-mode
  (setq lsp-lua-diagnostics-globals ["hs" "spoon"]))

(setq swiper-use-visual-line nil
      swiper-use-visual-line-p (lambda (a) nil))

;; There's a weird bug where fringe-modes < 8 dont show the fringes
(after! git-gutter
  (fringe-mode 8)
  (after! git-gutter-fringe
    (fringe-mode 8))
  (setq +vc-gutter-diff-unsaved-buffer t))

(after! ibuffer
  (set-popup-rule! "^\\*Ibuffer\\*$" :side 'bottom :size 0.4 :select t :ignore nil))

(setq +ivy-buffer-preview t)
(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center))

(setq doom-themes-treemacs-theme "doom-colors")
(after! treemacs
  (setq +treemacs-git-mode 'extended)
  (treemacs-follow-mode t))
(after! rainbow-mode
  (setq rainbow-html-colors-major-mode-list '(html-mode css-mode php-mode nxml-mode xml-mode typescript-mode javascript-mode)))

(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 22 :weight 'Bold)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20))


(setq doom-theme 'doom-dracula)

(custom-set-faces!
  '(font-lock-comment-face :slant italic))


; (defvar +fl/splashcii-query ""
;   "The query to search on asciiur.com")

; (defun +fl/splashcii-banner ()
;   (mapc (lambda (line)
;           (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
;                               'face 'doom-dashboard-banner) " ")
;           (insert "\n"))
;         (split-string (with-output-to-string
;                         (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
;                       "\n" t)))

; (setq +doom-dashboard-ascii-banner-fn #'+fl/splashcii-banner)

; (setq +fl/splashcii-query "christmas")

(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project)

  (+popup-window-p) ; needed to prevent recursive auto-loading of popup

  ;; Automatically turn off tabs in popups
  (defun +fl/hide-tabs-in-popup ()
    (if (+popup-window-p)
        (centaur-tabs-local-mode)
      (centaur-tabs-local-mode 0)))
  (add-hook! 'buffer-list-update-hook '+fl/hide-tabs-in-popup))

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have

(after! doom-modeline
  (setq doom-modeline-github t
        doom-modeline-github-interval (* 10 60)
        doom-modeline-major-mode-icon t)
  (add-hook 'doom-modeline-before-github-fetch-notification-hook #'auth-source-pass-enable)
  (doom-modeline--github-fetch-notifications))
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq writeroom-fullscreen-effect t)
(after! writeroom-mode
  (setq writeroom-fullscreen-effect t))

(setq +workspaces-on-switch-project-behavior t)
;; prefer right and bottom split
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; show buffer popup when splitting
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (projectile-find-file))

(set-docsets! 'python-mode "Python 3")
(set-docsets! 'lua-mode "Lua")
(set-docsets! 'emacs-lisp-mode "Emacs Lisp")
(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

;; change default notmuch func to open primary inbox
(defun +notmuch ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (unless (featurep! :ui workspaces)
    (user-error ":ui workspaces is required, but disabled"))
  (condition-case-unless-debug e
      (progn
        (+workspace-switch "*email*" t)
        (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                                   (doom-visible-windows))))
            (select-window (get-buffer-window buf))
          (notmuch-search "(tag:inbox (tag:personal or tag:flagged) not tag:trash"))
        (+workspace/display))
    ('error
     (+notmuch/quit)
     (signal (car e) (cdr e)))))

(map! :leader :desc "Open Notmuch" "o m" '+notmuch)

(after! notmuch
  ;; Popup rules
  (set-popup-rule! "^\\*notmuch.*search.*" :ignore t)
  (set-popup-rule! "^ \\*notmuch update.*" :select nil :quit t)
  (set-popup-rule! "^\\*notmuch-thread.*" :side 'bottom :size 0.6 :select t)

  ;; Show Images
  (setq notmuch-show-text/html-blocked-images nil)

  ;; dont use buffernames with thread subjects
  (defun notmuch-show--proper-buffer-name (args)
    (when (= (length args) 5)
      (setq args (butlast args)))
    args)
  (advice-add 'notmuch-show :filter-args 'notmuch-show--proper-buffer-name)

  ;; prefer html over text
  (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))

  (setq notmuch-saved-searches
        '((:name "  Inbox"      :query "(tag:inbox (tag:personal or tag:flagged) not tag:trash" :key "i")
          (:name "  Social"     :query "tag:social"              :key "cs")
          (:name "  Updates"    :query "tag:updates"             :key "cu")
          (:name "  Promotions" :query "tag:promotions"          :key "cp")
          (:name "  All Mail"   :query ""                        :key "a")
          (:name "  Starred"    :query "tag:flagged"             :key "*")
          (:name "  Sent"       :query "tag:sent"                :key "s")
          (:name "  Drafts"     :query "tag:draft"               :key "d"))))

;; (use-package elfeed
;;   :config
;;   (setq elfeed-search-feed-face ":foreground #fff :weight bold"
;;         elfeed-feeds (quote
;;                        (("https://www.reddit.com/r/linux.rss" reddit linux)
;;                         ("https://www.reddit.com/r/commandline.rss" reddit commandline)
;;                         ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
;;                         ("https://www.reddit.com/r/emacs.rss" reddit emacs)
;;                         ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
;;                         ("https://hackaday.com/blog/feed/" hackaday linux)
;;                         ("https://opensource.com/feed" opensource linux)
;;                         ("https://linux.softpedia.com/backend.xml" softpedia linux)
;;                         ("https://itsfoss.com/feed/" itsfoss linux)
;;                         ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
;;                         ("https://www.phoronix.com/rss.php" phoronix linux)
;;                         ("http://feeds.feedburner.com/d0od" omgubuntu linux)
;;                         ("https://www.computerworld.com/index.rss" computerworld linux)
;;                         ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
;;                         ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
;;                         ("https://betanews.com/feed" betanews linux)
;;                         ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
;;                         ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))

;; (use-package elfeed-goodies
;;   :init
;;   (elfeed-goodies/setup)
;;   :config
;;   (setq elfeed-goodies/entry-pane-size 0.5))

;; (add-hook 'elfeed-show-mode-hook 'visual-line-mode)
;; (evil-define-key 'normal elfeed-show-mode-map
;;   (kbd "J") 'elfeed-goodies/split-show-next
;;   (kbd "K") 'elfeed-goodies/split-show-prev)
;; (evil-define-key 'normal elfeed-search-mode-map
;;   (kbd "J") 'elfeed-goodies/split-show-next
;;   (kbd "K") 'elfeed-goodies/split-show-prev)


;; (after! elfeed
;;   (set-popup-rule! "^\\*elfeed-entry\\*" :side 'bottom :size 0.6 :select t :slot -1 :vslot -10)
;;   (add-hook! 'elfeed-search-mode-hook 'elfeed-update)) ; Update Elfeed when launched


(defvar +fl--browse-url-xwidget-last-session-buffer nil)

(defun +fl/browse-url-xwidget (url &optional new-session)
  (let ((orig-last-session-buffer
         (if (boundp 'xwidget-webkit-last-session-buffer)
             xwidget-webkit-last-session-buffer
           nil)))
    (setq xwidget-webkit-last-session-buffer +fl--browse-url-xwidget-last-session-buffer)
    (save-window-excursion
      (xwidget-webkit-browse-url url new-session))
    (with-popup-rules! '(("^\\*xwidget" :vslot -10 :size 0.6 :select t :slot -1))
      (pop-to-buffer xwidget-webkit-last-session-buffer))
    (setq +fl--browse-url-xwidget-last-session-buffer xwidget-webkit-last-session-buffer
          xwidget-webkit-last-session-buffer orig-last-session-buffer)))

(setq browse-url-browser-function '+fl/browse-url-xwidget)

;; (use-package! wakatime-mode
;;   :hook (after-init . global-wakatime-mode)
;;   :config
;;   (setq wakatime-python-bin "/usr/local/bin/python3"
;;         wakatime-api-key (auth-source-pass-get 'secret "wakatime/api_key")))


;; (use-package! wakatime-mode
;;   :hook (after-init . global-wakatime-mode))
;; (custom-set-variables '(wakatime-api-key (auth-source-pass-)))
;; :config
;; (setq  wakatime-python-bin "/usr/local/bin/python3"
;; wakatime-cli-path "~/.local/bin/wakatime"))

(setq
      org-ellipsis "  "                ; nerd fonts chevron character
      org-journal-file-type 'weekly
      org-use-property-inheritance t
      org-log-done 'time
      org-hide-emphasis-markers t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-into-drawer t
      org-log-state-notes-into-drawer t
      org-log-repeat 'time
      org-todo-repeat-to-state "TODO"
      +org-capture-notes-file "inbox.org"
      deft-directory "~/"
      deft-recursive t)

(after! org
  (setq org-tags-column -80
        org-agenda-sticky nil))

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(advice-add 'org-gcal-fetch :after 'org-save-all-org-buffers)

;; (after! org-gcal
;;   (setq org-gcal-client-id (auth-source-pass-get 'secret "org/gcal/client_id")
;;         org-gcal-client-secret (auth-source-pass-get 'secret "org/gcal/client_secret")
;;         org-gcal-fetch-file-alist '(("marcoocchialini@usp.br" .  "~/OneDrive/projects/org/gcal/personal.org")
;;                                     )))

(after! org
  (with-no-warnings
    (custom-declare-face '+org-todo-soon  '((t (:inherit (bold org-drawer org-todo)))) "")
    (custom-declare-face '+org-todo-next  '((t (:inherit (bold font-lock-keyword-face org-todo)))) "")
    (custom-declare-face '+org-todo-done  '((t (:inherit (bold org-headline-done org-done)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence
           "NEXT(n)"     ; A task that is in progress
           "WEEK(w)"     ; A task for this week
           "MONTH(m)"    ; A task for this month
           "TODO(t)"     ; A task that needs doing & is ready to do
           "HOLD(h@/!)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d!)"    ; Task successfully completed
           "KILL(k@!)")) ; Task was cancelled, aborted or is no longer applicable
        org-todo-keyword-faces
        '(("NEXT" . +org-todo-next)
          ("WEEK" . +org-todo-soon)
          ("MONTH" . +org-todo-soon)
          ("HOLD" . +org-todo-onhold)
          ("DONE" . +org-todo-done)
          ("KILL" . +org-todo-done))))



(after! org-roam
  (setq org-roam-directory (concat (file-name-as-directory org-directory) "roam")
        org-roam-tag-sources '(prop all-directories)
        +org-roam-open-buffer-on-find-file t
        ;; Create new roam notes under ~/org/notes
        org-roam-capture-ref-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t
           :immediate-finish t))))

(use-package! websocket
    :after org-roam)

(after! org
  (setq org-tags-column -80)
            `(:checkbox      ""
                             (appendq! +ligatures-extra-symbols
              :doing         ""
              :checkedbox    ""
              :list_property "∷"))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :doing         "[-]"
    :checkedbox    "[X]"
    :list_property "::"))

(setq org-agenda-category-icon-alist
      `(("inbox" ,(list (all-the-icons-faicon "inbox" :face 'all-the-icons-blue :v-adjust -0.1)) nil nil :ascent center)
        ("dev" ,(list (all-the-icons-faicon "code" :face 'all-the-icons-blue :height 0.8 :v-adjust 0)) nil nil :ascent center)
        ("splora" ,(list (all-the-icons-material "terrain" :face 'all-the-icons-blue :height 0.8)) nil nil :ascent center)
        ("home" ,(list (all-the-icons-faicon "home" :face 'all-the-icons-blue)) nil nil :ascent center)
        ("habits" ,(list (all-the-icons-faicon "undo" :face 'all-the-icons-pink)) nil nil :ascent center)
        ("life" ,(list (all-the-icons-faicon "asterisk" :face 'all-the-icons-blue)) nil nil :ascent center)
        ("birthdays" ,(list (all-the-icons-faicon "birthday-cake" :face 'all-the-icons-red)) nil nil :ascent center)
        ("calendar" ,(list (all-the-icons-faicon "google" :face 'all-the-icons-blue)) nil nil :ascent center)
        ("holidays" ,(list (all-the-icons-faicon "calendar-check-o" :face 'all-the-icons-green)) nil nil :ascent center)))

(after! org-agenda
  (set-popup-rule! "^\\*Org Agenda\\*$" :side 'bottom :size 0.4 :select t :quit t)
  (setq org-agenda-prefix-format
        '((agenda . "\t\t\t%-2i %-12t % s")
          (todo . "\t%-2i %-30b ")
          (tags . " %i %-12:c")
          (search         . " %i %-12:c"))
        org-agenda-block-separator nil
        org-agenda-span 7
        org-agenda-start-on-weekday 1
        org-agenda-start-day nil
        org-agenda-breadcrumbs-separator "  "
        org-agenda-files (directory-files-recursively org-directory "\.org$")
        org-agenda-time-grid (quote ((today daily require-timed remove-match) (0900 2100) " ╴╴╴╴╴" "──────────────────────"))
        org-agenda-current-time-string " now ────────────────")
  (set-face-attribute 'org-agenda-structure nil
                      :height 1.2
                      :foreground (face-attribute 'org-level-1 :foreground nil t)))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo   todo-state-up priority-down category-keep)
        (tags   todo-state-up priority-down category-keep)
        (search todo-state-up priority-down category-keep)))

(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(defun my-org-agenda-format-date-aligned (date)
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date nil nil))
         (day (cadr date))
         (month (car date))
         (monthname (calendar-month-name month nil))
         (year (nth 2 date)))
    (format "    %-10s %2d %s %4d"
            dayname day monthname year)))

(use-package! org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-unmatched-name "⚡ Backlog"
        org-super-agenda-unmatched-order 50))

(after! org-super-agenda
  (setq org-super-agenda-unmatched-name "⚡ Backlog"
        org-super-agenda-unmatched-order 50)
  (org-super-agenda-mode))

;; Super Agenda seems to jump to the last line, let's fix this!
(defun +fl/agenda-jump-to-start ()
  (goto-char (point-min)))
(add-hook 'org-agenda-finalize-hook '+fl/agenda-jump-to-start 90)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((todo "NEXT|WEEK"
                ((org-agenda-overriding-header "\n ⚡ Today")
                 (org-agenda-remove-tags t)))
          (agenda ""
                  ((org-agenda-skip-scheduled-if-done t)
                   (org-agenda-start-day "0d")
                   (org-agenda-span 3)
                   (org-agenda-skip-timestamp-if-done t)
                   (org-habit-show-all-today t)
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-overriding-header "\n ⚡ Agenda")
                   (org-agenda-repeating-timestamp-show-all nil)
                   (org-agenda-remove-tags t)
                   (org-agenda-time)))
          (todo "TODO|WAIT|HOLD"
                ((org-agenda-overriding-header "")
                 (org-agenda-remove-tags t)
                 (org-super-agenda-groups
                  '((:category "habits" :name "⚡ Habits" :order 60)
                    (:name "⚡ Inbox"
                     :category "inbox")
                    (:name "⚡ Next"
                     :todo "NEXT")
                    (:name "⚡ Week"
                     :todo "WEEK")
                    (:name "⚡ Month"
                     :todo "MONTH")
                    (:todo ("WAIT" "HOLD") :name "⚡ On Hold" :order 11)))))))))

(after! org-agenda
  (setq org-habit-show-all-today nil
        org-habit-today-glyph ?⚡
        org-habit-completed-glyph ?+ ))

; (use-package org-brain :ensure t
;  :init
;  (setq org-brain-path (concat (file-name-as-directory org-directory) "brain"))
;   (with-eval-after-load 'evil
;    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;   :config
;   (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
;   (setq org-id-track-globally t)
;   (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
;   (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;   ; (push '("b" "Brain" plain (function org-brain-goto-end)
;   ;         "* %i%?" :empty-lines 1)
;   ;       org-capture-templates)
;   (setq org-brain-visualize-default-choices 'all)
;   (setq org-brain-title-max-length 12)
;   (setq org-brain-include-file-entries nil
;         org-brain-file-entries-use-title nil))

; ;; Allows you to edit entries directly from org-brain-visualize
; (use-package polymode
;   :config
;   (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

; (use-package! org-fragtog
;   :after org
;   ; :hook (org-mode . org-fragtog) ; this auto-enables it when you enter an org-buffer, remove if you do not want this
;   )

;; Zooming in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq gc-cons-threshold (* 2 1000 1000))


(setq org-re-reveal-klipsify-src t)

(after! org-kanban
  :config
(defun org-kanban//link-for-heading (heading file description)
  "Create a link for a HEADING optionally USE-FILE a FILE and DESCRIPTION."
  (if heading
      (format "[[*%s][%s]]" heading description)
    (error "Illegal state")))
  )

(require 'org-habit)

(use-package! org-ref)
(after! org-ref
  (setq org-ref-default-bibliography (concat (file-name-as-directory org-directory) "bib")
          org-ref-pdf-directory (concat (file-name-as-directory org-directory) "books")
          org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          org-ref-notes-directory (concat (file-name-as-directory org-directory) "notes")
          org-ref-notes-function 'orb-edit-notes)
    )

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  ;; (setq org-roam-server-host "172.16.3.168")
  (setq orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${=key=}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS: article
- tags ::
- keywords :: ${keywords}
* ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :URL: ${url}
  :AUTHOR: ${author-or-editor}
  :NOTER_DOCUMENT: %(file-relative-name (orb-process-file-field \"${=key=}\") (print org-directory))
  :NOTER_PAGE:
  :END:
** CATALOG
*** Motivation :springGreen:
*** Model :lightSkyblue:
*** Remarks
*** Applications
*** Expressions
*** References :violet:
** NOTES
"
           :unnarrowed t))))

; (org-roam-bibtex-mode)
; (use-package! org-roam-server)
(use-package! org-journal
  ;; :bind
  ;; ("C-c n j" . org-journal-new-entry)
  ;; ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-file-format "private-%Y-%m-%d.org"
        org-journal-dir (concat (file-name-as-directory org-directory) "journal")
        org-journal-carryover-items nil
        org-journal-date-format "%Y-%m-%d")
  ;; do not create title for dailies
  (set-file-template! "/private-.*\\.org$"    :trigger ""    :mode 'org-mode)
  (print +file-templates-alist)
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package! org-noter

  :config
  (setq
   org-noter-pdftools-markup-pointer-color "yellow"
   org-noter-notes-search-path '()
   ;; org-noter-insert-note-no-questions t
   org-noter-doc-split-fraction '(0.7 . 03)
   org-noter-always-create-frame nil
   org-noter-hide-other nil
   org-noter-pdftools-free-pointer-icon "Note"
   org-noter-pdftools-free-pointer-color "red"
   org-noter-kill-frame-at-session-end nil
   )
  (map! :map (pdf-view-mode)
        :leader
        (:prefix-map ("n" . "notes")
          :desc "Write notes"                    "w" #'org-noter)
        )
  )

(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))
(use-package! org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
