(use-package browse-kill-ring
  :bind
  (("M-y" . browse-kill-ring)
   (:map browse-kill-ring-mode-map
         ("C-n" . browse-kill-ring-forward)
         ("C-p" . browse-kill-ring-previous)))
  :custom
  (browse-kill-ring-separator (if window-system
                                  "──────────────────────────────────────────────────────────────────────────────────"
                                "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (browse-kill-ring-separator-face '((nil (:foreground "gray"))))
  :config
  (setq browse-kill-ring-show-preview nil)
  (setq browse-kill-ring-highlight-current-entry t)
  (define-advice browse-kill-ring-insert-and-highlight (:around (old-function str) exwm-paste)
    "Paste the selection appropriately in exwm mode buffers"
    (if (derived-mode-p 'exwm-mode)
        (progn
          (kill-new str)
          (exwm-input--fake-key ?\C-v)
          (exwm-input--fake-key ?\C-e))
      (funcall old-function str))))

(use-package clipmon
  :config
  ;;   ;;Clipmon auto adds any system clipboard entry into emacs kill ring every 1 second
  ;;   (setq clipmon-timer-interval 1) 
  ;;   ;;anything thats inside system clipboard is now always available to me in kill ring
  ;;   (clipmon-mode)
  ;;   ;;Trial and error below 3 lines, t-t-nil seems to work for now
  ;;   (setq select-enable-clipboard t)
  ;;   (setq select-enable-primary nil)
  ;;   (setq save-interprogram-paste-before-kill nil)
  ;;   ;;trying out suggestions from lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00922.html
  ;;   ;;(setq save-interprogram-paste-before-kill t)

  (setq kill-ring-max 1000)
  (setq clipmon-timer-interval 1)
  ;; monitor the system clipboard and add any changes to the kill ring
  (add-to-list 'after-init-hook 'clipmon-mode-start)
  (setq clipmon-autoinsert-color nil))
