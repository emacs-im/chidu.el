;; -*- mode: eask; lexical-binding: t -*-

(package "chidu"
         "0.1.0"
         "JMAP-first mail client")

(website-url "https://github.com/emacs-im/chidu.el")

(keywords "mail" "comm")

(package-file "chidu.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "29.1")
(depends-on "appkit" :vc "https://github.com/emacs-im/appkit.el" "v0.2.18")
(depends-on "plz" "0.9.1")
(depends-on "transient" "0.7.0")

(development
 (depends-on "evil"))

(script "test" "eask recompile --quiet && eask test ert --quiet test/*.el")
