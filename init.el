;; after git tutorial
;; binding x to be easier to hit
(define-key key-translation-map [?\C-x] [?\C-t])
(define-key key-translation-map [?\C-t] [?\C-x])
(define-key key-translation-map [?\M-x] [?\M-t])
(define-key key-translation-map [?\M-t] [?\M-x])

;; enable elpa and melpa
(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")))

;; apropos sorting results by relevancy
(setq apropos-sort-by-scores t)

;;(global-set-key (kbd "M-o") 'other-window)

;; swapping meta and super
(setq x-super-keysym 'meta)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#4F4F4F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#6F6F6F"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#93E0E3")
 '(cua-normal-cursor-color "#DCDCCC")
 '(cua-overwrite-cursor-color "#F0DFAF")
 '(cua-read-only-cursor-color "#7F9F7F")
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" default))
 '(fci-rule-color "#4F4F4F")
 '(fido-mode t)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode t)
 '(global-visual-line-mode t)
 '(highlight-changes-colors '("#DC8CC3" "#bbb0cb"))
 '(highlight-symbol-colors
   '("#681063eb5999" "#54db645164d1" "#6098535f5323" "#5c2959a95fa1" "#4edf55f24ea4" "#64de597a525e" "#530160d26158"))
 '(highlight-symbol-foreground-color "#FFFFEF")
 '(highlight-tail-colors
   '(("#4F4F4F" . 0)
     ("#488249" . 20)
     ("#5dacaf" . 30)
     ("#57a2a4" . 50)
     ("#b6a576" . 60)
     ("#ac7b5a" . 70)
     ("#aa5790" . 85)
     ("#4F4F4F" . 100)))
 '(hl-bg-colors
   '("#b6a576" "#ac7b5a" "#9f5c5c" "#aa5790" "#85749c" "#57a2a4" "#5dacaf" "#488249"))
 '(hl-fg-colors
   '("#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F"))
 '(hl-paren-colors '("#93E0E3" "#F0DFAF" "#8CD0D3" "#bbb0cb" "#7F9F7F"))
 '(ido-enable-flex-matching t)
 '(ido-mode 'both nil (ido))
 '(line-move-visual t)
 '(line-number-mode t)
 '(package-selected-packages '(vlf))
 '(tool-bar-max-label-size 304)
 '(tool-bar-position 'top)
 '(tool-bar-style 'image))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray10" :underline nil)))))
