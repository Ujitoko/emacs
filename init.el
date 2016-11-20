;;load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;;load-pathに追加するフォルダ
(add-to-load-path "elisp" "elpa")

(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; 初期化
(package-initialize)

;;パッケージのインストールを自動化
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;;環境を日本語にする
;;(require 'mozc)
;;(set-language-environment "Japanese")
;;(setq default-input-method "japanese-mozc")
;;(prefer-coding-system 'utf-8)
		      
;;C-hでBackspaceに
(keyboard-translate ?\C-h ?\C-?)

;;スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;;バックアップファイルを作成させない
(setq make-backup-files nil)

;;列数を表示する
(column-number-mode t)

;; ;;行数を表示する
;;(global-linum-mode t)
;; ;;(setq linum-format "%4dl")

;; ;;対応する()を光らせる
(show-paren-mode 1)

;; ;;スペースやタブなどを可視化する
;;(global-whitespace-mode 1)

;;(tool-bar-mode-1)

;;yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ;;フレームの透明度 意味ない？
(set-frame-parameter (selected-frame) 'alpha '(1.0))

(setq x-select-enable-clipboard t)

;; unvisualize menubar, toolbar
(menu-bar-mode 0)
(tool-bar-mode 0)

;;文字自体をコメントアウト
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))

(global-set-key (kbd "M-;") 'one-line-comment)


;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(custom-enabled-themes nil)
;; '(custom-safe-themes
;;   (quote
;;    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default))))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )


(load-theme 'spolsky t)


(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "gfm-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; "C-t"でウィンドウ切り替える。初期値はtranspose-chars
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)


;;companyの設定
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る


;;ironyの設定
;;melpaの設定を行って、ironyをインストール
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
