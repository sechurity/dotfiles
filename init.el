;; customizations to be more minimalist
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; create some left and right margins
(set-fringe-mode 10) 

;; visual bell instead of audio bell
(setq visible-bell t)

;; use buffer name as title
(setq frame-title-format "%b")



;; enable elpa and melpa
(setq package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))

;; install packages:
;; speed-type
(package-refresh-contents)
(setq package-selected-packages '((speed-type) (solorized-theme))
(package-install-selected-packages)

;; apropos sorting results by relevancy
(setq apropos-sort-by-scores t)

;; shortcut for C-x o  
(global-set-key (kbd "M-o") 'other-window)

;; swapping meta and super (mac)
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  )
