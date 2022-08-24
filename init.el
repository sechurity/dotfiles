;; Automatically install Emacs packages by specifying a list of package names:;
;; Source: https://stackoverflow.com/a/10093312				       
; list the packages you want
(setq package-list '(org-roam org org-download ox-hugo simple-httpd htmlize impatient-mode))

; Adding melpa to the default list of GnuELPA
; https://www.emacswiki.org/emacs/InstallingPackages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'autoinstall-packages)

;; Adjust whole interface size, ideal size for mobile: 250 
(set-face-attribute 'default (selected-frame) :height 100)
;; Listing buffers with ibuffer instead
(global-set-key [remap list-buffers] 'ibuffer)

;; Hiding menubar, toolbar, and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable IDO mode
;; (ido-mode 'both nil (ido))
;; Enable IDO flex matching
;; (ido-enable-flex-matching t)

;; Enable FIDO mode
;; (fido-mode t)

;; Enable tab bar history mode
;; (tab-bar-history-mode t)

;; Keybindings to go back and forth tarbar history
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

;; Rebinding C-x o to M-o to switch windows
(global-set-key (kbd "M-o") 'other-window)
;; Use windmove default keybindings to move around (shift + arrow keys)
(windmove-default-keybindings)


;; Enabling pasting images to org files by org-download
;; https://github.com/abo-abo/org-download
(require 'org-download)
;; Create a keystroke for pasting image from clipboard
(define-key org-mode-map (kbd "C-S-v") 'org-download-clipboard)

;; Making org-mode inline picture display responsive
;; https://emacs.stackexchange.com/questions/64604/full-screen-hook
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Sizes.html
;; https://stackoverflow.com/questions/36465878/how-to-make-inline-images-responsive-in-org-mode/
(defun org-image-resize (frame)
  (when (derived-mode-p 'org-mode)
      (if (< (window-total-width) 80)
	  (setq org-image-actual-width (window-pixel-width))
	(setq org-image-actual-width (* 80 (window-font-width))))
      (org-redisplay-inline-images)))

(add-hook 'window-size-change-functions 'org-image-resize)

;; Org-roam
;; Setup: create the cache directory for all notes
(unless (file-directory-p "~/org-roam")
((make-directory "~/org-roam")
 (setq org-roam-directory (file-truename "~/org-roam"))))

;; Run functions on file changes to maintain cache consistency
(org-roam-db-autosync-mode)
;; build cache manually: M-x org-roam-db-sync
;; Custom configs to find and insert org-roam nodes
;; All 3 of them creates node if it doesn't exist
(define-key org-mode-map (kbd "C-x n f") 'org-roam-node-find)
(define-key org-mode-map (kbd "C-x n i") 'org-roam-node-insert)
(define-key org-mode-map (kbd "C-x n c") 'org-roam-capture)

(require 'org-roam-export)

;; Org babel
;; Prevent auto indent when editing python code
(setq org-src-preserve-indentation t)

;; Org babel for plantUML
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (shell . t)
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/Downloads/plantuml-1.2022.6.jar"))

;; Specify modes to disable line numbers
(add-hook 'image-mode (lambda () (display-line-numbers-mode -1)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(wombat))
 '(delete-selection-mode t)
 '(fido-mode t)
 '(global-display-line-numbers-mode t)
 '(global-superword-mode t)
 '(global-visual-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode 'both nil (ido))
 '(line-move-visual nil)
 '(package-selected-packages '(impatient-mode htmlize simple-httpd sokoban nov))
 '(tab-bar-history-mode t)
 '(tab-line-tabs-function 'tab-line-tabs-mode-buffers))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
