(bind-keys :map global-map
           ("M-o" . anup/other-window)
           ("C-x o" . anup/other-window))

(use-package exwm
  :if (display-graphic-p)
  :vc (:url "https://github.com/emacs-exwm/exwm" :rev :newest)
  :init
  ;; moved to using polybar
  ;; ;;system tray, to show dropbox etc, must be configured before (exwm-wm-mode +1). aka: dock, bottom system panel
  ;;(require 'exwm-systemtray)
  ;;(exwm-systemtray-mode +1)
  
  (require 'exwm)
  (setq exwm-layout-show-all-buffers t)
  ;;(require 'exwm-config)
  (setq xcb:connection-timeout 3)
  (defun farl-exwm/on-startup ()
    "Start EXWM and related processes."
    (setenv "XDG_CURRENT_DESKTOP" "emacs")
    ;;(setenv "GTK2_RC_FILES" (expand-file-name "~/.config/gtk-2.0/gtkrc"))
    (setenv "QT_QPA_PLATFORMTHEME" "gtk2")
    (setenv "_JAVA_AWT_WM_NONREPARENTING" "1")
    (start-process "Disable Blanking" nil "xset" "s" "off" "-dpms")
    (start-process "Set keyboard repeat rate" nil "xset" "r" "rate" "200" "60")
    (set-process-query-on-exit-flag
     (start-process "Hide mouse pointer" nil "unclutter")
     nil)
    (start-process "Fallback Cursor" nil "xsetroot" "-cursor_name" "left_ptr")
    (efs/start-panel))
  (defun farl-exwm/on-logout ()
    "Run this when logging out as part of `kill-emacs-hook'."
    ;;(start-process "Root window" nil "hsetroot" "-solid" "'#000000'")
    (start-process "Erase Qutebrowser session on emacs close" nil "rm" "/home/anup/.local/share/qutebrowser/sessions/_autosave.yml"))
  (push ?\M-g exwm-input-prefix-keys)
  (push ?\C-c exwm-input-prefix-keys)
  :hook
  ;;Without below renaming, you may see the X buffer names being generically named '*EXWM*' which is confusing
  ;;Below will add the app name to X buffer names
  
  ;;An explicit " -- exwm-class-name" is required because obscure X apps such as,-
  ;;-such as qBittorrent sometimes dont add their appname to all their windows
  ;;whereas Google Chrome and Qutebrowser, by default, already include their classname in their title.
  (exwm-update-title . (lambda ()
                         (if (or (string= exwm-class-name "qutebrowser")
                                 (string= exwm-class-name "Google-chrome"))
                             (exwm-workspace-rename-buffer exwm-title)
                           (exwm-workspace-rename-buffer (concat exwm-title " -- " exwm-class-name)))))

  ;;per application settings
  :hook
  (exwm-manage-finish . (lambda ()
                          ;; (when (and exwm-class-name
                          ;;            (string= exwm-class-name "Google-chrome"))
                          ;;   (exwm-input-set-local-simulation-keys
                          ;;    (cl-concatenate 'list
                          ;;                 exwm-input-simulation-keys
			  ;;       	  `((,(kbd "M-b") . ,(kbd "<XF86Back>"))
			  ;;       	    (,(kbd "M-f") . ,(kbd "<XF86Forward>"))
			  ;;       	    (,(kbd "C-g") . ,(kbd "ESC"))))))
                          
			  ;;disable all simulation keys for some app:-
                          (when (and exwm-class-name
                                     (or
                                      (string= exwm-class-name "Nyxt")))
                            (exwm-input-set-local-simulation-keys nil))))
  :hook
  ((exwm-manage-finish . (lambda ()
                           (when (and exwm-class-name
                                      (string= exwm-class-name "qutebrowser"))
                             ;;Qutebrowser wants to redefine C-n, C-p etc by itself in its own config.py
                             ;;ie it does not want to inherit baggage from exwm-input-simulation-keys
                             ;;Hence, earlier I was resetting exwm-input-set-local-simulation-keys to nil
                             ;;(exwm-input-set-local-simulation-keys nil)
                             ;;But, make one exception
                             ;;This translates M-i into M-i. This gimmick is needed to make avy global keybindings work
                             ;;except, if qute, M-i will be sent as M-i to config.py
                             ;;over there it is bound to 'hint all'
                             (exwm-input-set-local-simulation-keys '(([?\M-i] . [?\M-i]))))
                           (when (and exwm-class-name
                                      (not (string= exwm-class-name "qutebrowser")))
                             ;;Set avy global M-i keybinding
                             (keymap-local-set "M-i" 'avy-goto-char-timer))))
   (exwm-update-title . anup/activate-qutebrowser-window)
   (after-init . farl-exwm/on-startup)
   (kill-emacs . farl-exwm/on-logout))
  :bind
  (;;("C-c p" . helm-run-external-command)
   ("C-c p" . anoop/counsel-linux-app)
   ;;("C-c p" . app-launcher-run-app)
   ("M-y" . browse-kill-ring)
   ("<XF86Back>" . previous-buffer)
   ("<XF86Forward>" . next-buffer)
   ("M-!" . shell-command)
   :map exwm-mode-map
   ("C-c C-f" . nil)
   ("C-c C-t C-f" . nil)
   ("C-c C-t C-v" . nil)
   ("C-c C-t C-m" . nil)
   ("C-q" . exwm-input-send-next-key)
   ("C-c C-q" . nil)
   ("C-c C-l" . farl-exwm/C-k)
   ("C-x C-s" . farl-exwm/C-s)
   ("C-x h" . farl-exwm/C-a)
   ("C-o" . farl-exwm/C-o)
   ("M-<" . anup/M-<)
   ("M->" . anup/M->))
  :config
  (defun anup/activate-qutebrowser-window ()
    (if (string-match-p "Google Search⁞⁞" exwm-title)
        (progn
          ;;(message "Debug0:- Qutebrowser loading just completed \n and current buffer is %s and \n buffer name is %s and \n exwm-title is %s \n --------------------------------------------- \n\n" (current-buffer) (buffer-name) exwm-title)
          ;;< <Quit the minibuffer> >
          (if (and
               ;; Not (visible and focussed):-
               (not (eq (current-buffer) (window-buffer (selected-window))))
               ;; Not (visible and unfocussed):-
               (not (get-buffer-window (current-buffer))))
              (switch-to-buffer (current-buffer)))
          )))
;;; Some programs such as 'edwin' are better off being started in char-mode.
  (defun ambrevar/exwm-start-in-char-mode ()
    (when (or (string-prefix-p "edwin" exwm-instance-name) (string-prefix-p "code" exwm-instance-name))
      (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
  (add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)
  (setq exwm-manage-force-tiling t)
  (fringe-mode '(8 . 8))                ;make both left and right fringe 8 px wide
  ;;(scroll-bar-mode 0)
  (menu-bar-mode 0)

  (setq exwm-input-simulation-keys
        `(;; movement
          (,(kbd "M-b") . C-left)
          (,(kbd "M-f") . C-right)
          (,(kbd "M-v") . prior)
          (,(kbd "C-v") . next)
          (,(kbd "C-d") . delete)
          (,(kbd "M-d") . backspace)

          ;;unable to bind '<' or '>' in exwm due to a known exwm issue
          ;;(,(kbd "M-<") . home) 
          ;;(,(kbd "M->") . end)
          ;;([?\M-<] . [C-home])
          ;;([?\M->] . [C-end])

          ;; Go to begin and end of window
          ([?\C-a] . [home])
          ([?\C-e] . [end])

          ([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])

          ;;Navigate by paragraphs
          ([?\M-b] . [C-left])
          ([?\M-f] . [C-right])
          ([?\M-n] . [C-down])
          ([?\M-p] . [C-up])

          ;; disabled because I prefer caret-mode for selecting text in qutebrowser
          ;;however someday, this might become useful if I start using some X window program other than qutebrowser extensively
          ;; Selecting via navigation
          ;; (,(kbd "C-S-b") . [S-left])
          ;; (,(kbd "C-S-f") . [S-right])
          ;; (,(kbd "C-S-n") . [S-down])
          ;; (,(kbd "C-S-p") . [S-up])

          ;;how the heck does below not conflict with keybindings defined in config.py for qutebrowser?
          ;;DOUBT:-
          ;;does keybinding pass through EXWM to qutebrowser or
          ;;does it pass directly to qutebrowser
          ;;does it differ from line-mode to char-mode of EXWM
          ;; Copy/Paste
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])

          ;;disbled because I have bound this instead in qutebrowser's config.py
          ;;however someday, this might become useful if I start using some X window program other than qutebrowser extensively
          ;;Search
          ;;([?\C-s] . [?\C-f])

          ;;undo
          ([?\C-\/] . [?\C-z])

          ;; Other
          ([?\C-d] . [delete])
          ([?\M-d] . [C-delete])
          ([?\C-k] . [S-end delete])

          ;;disbled because I have bound this instead in qutebrowser's config.py
          ;;([?\C-g] . [escape])
          ))
  
  (setq exwm-input-global-keys
        `(
          ;; Enter line mode and redirect input to emacs
          ([?\s-n] . (lambda () (interactive)
                       (exwm-reset)
                       (setq exwm-input-line-mode-passthrough t)))

          ;; Only enter line mode
          ([?\s-N] . (lambda () (interactive)
                       (exwm-reset)
                       (setq exwm-input-line-mode-passthrough nil)))

          ;; enter char-mode again
          ([?\s-i] . exwm-input-release-keyboard)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ([?\M-o] . anup/other-window)

          ;;(,(kbd "M-y") . helm-show-kill-ring)
          ;;switched away from helm kill ring browser^ to this:-
          (,(kbd "M-y") . browse-kill-ring)
          
          (,(kbd "M-!") . shell-command)
          (,(kbd "<XF86Back>") . previous-buffer)
          (,(kbd "<XF86Forward>") . next-buffer)

          ;;;; Below causes problems with char-mode for Edwin editor
          ;;;;([key-4660] . ignore) ;; emods workaround, causes problems to Edwin
          ))

  ;;;;Make chrome buffer name equal to its title value
  ;;guide:- https://teddit.net/r/emacs/comments/mb8u1m/weekly_tipstricketc_thread/gs55kqw/

  ;;needs this chrome extension for to even include 'url' in chrome tab title
  ;;this extension is open source
  ;;https://chrome.google.com/webstore/detail/url-in-title/ignpacbgnbnkaiooknalneoeladjnfgb/related?hl=en

  ;;needs this chrome extension to by default open links in new window instead of new tab
  ;;https://chrome.google.com/webstore/detail/new-tab-new-window/dndlcbaomdoggooaficldplkcmkfpgff


  ;;custom keybindings for Google Chrome
  ;;source:- https://github.com/ch11ng/exwm/wiki#how-to-send-c-c-to-term
  ;;below has a bug, instead of setting =exwm-input-set-local-simulation-keys=,
  ;;-I should have added to existing set of =exwm-input-set-local-simulation-keys=
  ;;I have tried to solve this bug using cons, see if this bugfix works on restart
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
                         (string= exwm-class-name "Google-chrome"))
                (exwm-input-set-local-simulation-keys (cons '([?\C-r] . ?\C-r) exwm-input-simulation-keys))
                (exwm-input-set-local-simulation-keys (cons '([f5] . ?\C-r) exwm-input-simulation-keys))
                ;;M-w should copy
                (exwm-input-set-local-simulation-keys (cons '([?\M-w] . ?\C-c) exwm-input-simulation-keys))))))

(defun my/exwm-update-global-keys ()
  (interactive)
  (setq exwm-input--global-keys nil)
  (dolist (i exwm-input-global-keys)
    (exwm-input--set-key (car i) (cdr i)))
  (when exwm--connection
    (exwm-input--update-global-prefix-keys)))

(use-package exwm-mff
  :if (display-graphic-p)
  :config
  (exwm-mff-mode))

(use-package exwm-mff
  :vc (:url "https://codeberg.org/emacs-weirdware/exwm-mff.git")
  :if (display-graphic-p)
  :config
  (exwm-mff-mode))

(if (eq system-type 'gnu/linux)
    (run-with-timer 5
                    nil
                    (lambda () (call-process-shell-command "/usr/bin/dropbox start &" nil 0))))

(defun pen-tablet-reload ()
  (interactive)
  ;;if already running, kill it
  (call-process-shell-command "echo jjf | sudo -S pkill PenTablet" nil 0)
  ;;wait for 2 seconds for the kill to complete, before running next command
  (run-with-timer 2
                  nil
                  (lambda () (call-process-shell-command "echo jjf | sudo -S /usr/lib/pentablet/PenTablet.sh &" nil 0)))
  ;;bury the PenTablet settings window after it starts. I have a doubt on what to pass as the buffer name
  (run-with-timer 7
                  nil
                  (lambda ()
                    (switch-to-buffer
                     ;;The space is needed if the buffer is on another frame
                     (or (get-buffer "Pentablet -- PenTablet") (get-buffer " Pentablet -- PenTablet")))
                    ;;(bury-buffer)                         ;did not work
                    ;;Lets indirectly bury this buffer by switching to the scratch buffer as the active buffer
                    (switch-to-buffer "*scratch*"))))

(defalias 'pentablet-reload 'pen-tablet-reload)
(defalias 'drawing-tablet-reload 'pen-tablet-reload)

(if (and (equal system-name "imac0") (display-graphic-p))
    (progn
      ;;Below is the equivalent of unplugging and replugging the usb cable connecting the tablet
      (shell-command "usbreset 28bd:0905")

      ;;wait for 5 seconds for reset to complete
      (run-with-timer 5
                      nil
                      'pen-tablet-reload)))

(if (eq system-type 'gnu/linux)
    (run-with-timer 7
                    nil
                    (lambda () (call-process-shell-command "nm-applet &" nil 0))))

(use-package winner
  :ensure nil ;;builtin
  )
(defun anup/other-window ()
  (interactive)
  (other-window (+ 1 (cond ((= (length (cl-remove-if 'window-no-other-p (window-list (selected-frame)))) 1) (length (cl-remove-if 'window-no-other-p (window-list (exwm-workspace--workspace-from-frame-or-index (% (+ exwm-workspace-current-index 2) 3))))))
                           ((and (> (length (cl-remove-if 'window-no-other-p (window-list (selected-frame)))) 1)
                                 (not (eq (selected-window) (-last-item (winner-sorted-window-list))))) 0)
                           ((and (> (length (cl-remove-if 'window-no-other-p (window-list (selected-frame)))) 1)
                                 (eq (selected-window) (-last-item (winner-sorted-window-list)))) (length (cl-remove-if 'window-no-other-p (window-list (exwm-workspace--workspace-from-frame-or-index (% (+ exwm-workspace-current-index 2) 3)))))))) 'visible))

(setq helm-boring-buffer-regexp-list
      '(;;"\\` "
	"\\` \\*"
	"\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"))

(setq ivy-ignore-buffers '(;;"\\` "
                           "\\`\\*tramp/"))

(setq helm-boring-buffer-regexp-list (cons "*quelpa-build-checkout*" helm-boring-buffer-regexp-list))
(setq helm-boring-buffer-regexp-list (cons "tq-temp-epdfinfo" helm-boring-buffer-regexp-list))
(setq helm-boring-buffer-regexp-list (cons "*pdf-scroll-log*" helm-boring-buffer-regexp-list))

(setq helm-boring-buffer-regexp-list (cons "\\magit-process:" helm-boring-buffer-regexp-list))

;;(setq helm-boring-buffer-regexp-list (cons "\\No file - mpv -- mpv" helm-boring-buffer-regexp-list))
(setq helm-boring-buffer-regexp-list (cons "\\tq-temp" helm-boring-buffer-regexp-list))

(setq helm-boring-buffer-regexp-list (cons "*straight-process*" helm-boring-buffer-regexp-list))
(setq helm-boring-buffer-regexp-list (cons "*Compile-Log*" helm-boring-buffer-regexp-list))
(setq helm-boring-buffer-regexp-list (cons "*Async-native-compile-log*" helm-boring-buffer-regexp-list))
(setq helm-boring-buffer-regexp-list (cons "*XELB-DEBUG*" helm-boring-buffer-regexp-list))

(defun shutdown-computer ()
  "Shut down the computer."
  (interactive)
  (let ((shut-down (lambda ()
                     (shell-command "systemctl poweroff"))))
    (add-hook 'kill-emacs-hook shut-down)
    (save-buffers-kill-emacs)
    (remove-hook 'kill-emacs-hook shut-down)))

(defun reboot-computer ()
  "Reboot the computer."
  (interactive)
  (let ((reboot (lambda ()
                  (shell-command "reboot"))))
    (add-hook 'kill-emacs-hook reboot)
    (save-buffers-kill-emacs)
    (remove-hook 'kill-emacs-hook reboot)))
(defalias 'restart-computer 'reboot-computer)

(defun suspend-computer ()
  "Put computer to sleep"
  (interactive)
  ;;(start-process "suspend" nil "systemctl" "suspend" "-i")
  ;;(shell-command "systemctl suspend")
  ;;Does using sudo to sleep help fix bugs?
  (shell-command "echo jjf | systemctl suspend &"))

(defalias 'sleep-computer 'suspend-computer)

(defun kill-startx ()
  "Kill 9 the X server"
  (interactive)
  (shell-command "kill -9 $(pgrep Xorg)")
  ;;(start-process "killstartx" nil "kill" "-9" "$(pgrep startx)")
  )

(defun farl-exwm/C-s ()
  "Pass C-s to the EXWM window."
  (interactive)
  (execute-kbd-macro (kbd "C-q C-s")))

(defun farl-exwm/C-k ()
  "Pass C-k to the EXWM window."
  (interactive)
  (execute-kbd-macro (kbd "C-q C-k")))

(defun farl-exwm/C-a ()
  "Helper to select all text when in EXWM buffers."
  (interactive)
  (execute-kbd-macro (kbd "C-S-<f3>")))

(defun farl-exwm/C-o ()
  "Pass the equivalent of C-o to the EXWM window."
  (interactive)
  (execute-kbd-macro (kbd "<S-return> C-b")))

(defun anup/M-< ()
  "Move to end of qutebrowser window"
  (interactive)
  (execute-kbd-macro (kbd "C-S-<f1>")))
(defun anup/M-> ()
  "Move to beginning of qutebrowser window"
  (interactive)
  (execute-kbd-macro (kbd "C-S-<f2>")))

(defun single-monitor-setup ()
  "Only the iMac0 screen is active. All external monitors are off."
  (interactive)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "C-x o") 'other-window)
  ;; Since on iMac internal screen is active, let me adjust the font size too
  ;; The default was (:background "black" :foreground "gray60" :inverse-video nil :box (:line-width 1 :color "#1A2F54"))
  ;;(set-face-attribute 'mode-line nil :family "Iosevka Slab" :height 250)

  ;;(set-frame-font "Iosevka Slab 25" nil nil)

  ;;default wasy ((width . 80) (height . 2))
  ;;tried, but didnt have any visible effect:-
  ;;(setq minibuffer-frame-alist '((width . 80) (height . 4)))

  ;;(set-face-attribute 'minibuffer-prompt nil :family "Iosevka Slab" :height 250)
  )

(defun multi-monitor-setup ()
  "Setup keybindings for one iMac0 screen and two external monitors."
  (interactive)
  (bind-keys :map global-map
             ("M-o" . anup/other-window)
             ("C-x o" . anup/other-window)))

(defun dual-monitor-setup ()
  "Setup keybindings for one iMac0 screen and one external monitor."
  (interactive)
  (bind-keys :map global-map
             ("M-o" . (lambda ()
                        (interactive)
                        (other-window 1 t)))
             ("C-x o" . (lambda ()
                          (interactive)
                          (other-window 1 t)))))

(defun exwm-randr-refresh2 ()
  "randr refresh that works with anups triple monitor setup"
  (interactive)
  (exwm-randr-refresh)
  (message "Opening arandr, this forces exwm to refresh its layout")
  (call-process-shell-command "arandr &" nil 0)
  ;;for good measure, call the builtin once again:
  (exwm-randr-refresh)
  ;;wait for 3 seconds, and then close the gnome-calendar:
  (run-with-timer 2
                  nil
                  'anoop/kill-this-buffer))
