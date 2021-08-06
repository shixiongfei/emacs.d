;;; init.el --- ShiXiongfei's Emacs configuration
;;
;; Copyright (c) 2021 Xiongfei Shi
;;
;; Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
;; License: Apache-2.0
;;
;; https://github.com/shixiongfei/emacs.d
;;
;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Xiongfei Shi"
      user-mail-address "xiongfei.shi@icloud.com")

;;; init.el ends here
