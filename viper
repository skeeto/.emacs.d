;; This file acts as a "light" configuration based on Viper

(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

;; Set up a few other basic things
(blink-cursor-mode -1)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(load-theme 'tango-dark)
