;; -*- mode: eask; lexical-binding: t -*-

(setq load-prefer-newer t)

;; Never load a `.eln' in place of a `.elc': an interactive Emacs using
;; this checkout native-compiles it into the eln-cache, and native code
;; ignores buttercup spies on primitives (e.g. `completing-read'), so
;; tests would hit real prompts and block reading stdin.
(setq load-no-native t)

(package "eca"
         "0.0.1"
         "AI pair programming via ECA (Editor Code Assistant)")

(website-url "https://github.com/editor-code-assistant/eca-emacs")
(keywords "tools")

(package-file "eca.el")

(files "*.el")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "28.1")
(depends-on "dash")
(depends-on "s")
(depends-on "f")
(depends-on "markdown-mode")
(depends-on "compat")
