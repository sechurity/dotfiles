;; Automatically install Emacs packages by specifying a list of package names:;
;; Source: https://stackoverflow.com/a/10093312				       
; list the packages you want
(setq package-list '())

; Adding melpa to the default list of GnuELPA
; https://www.emacswiki.org/emacs/InstallingPackages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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
 '(package-selected-packages '(sokoban nov))
 '(tab-bar-history-mode t)
 '(tab-line-tabs-function 'tab-line-tabs-mode-buffers))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
