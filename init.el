;;load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;;load-pathに追加するフォルダ
;;2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "conf" "public_repos")
;;(add-to-list 'load-path "elisp")

;;パッケージのインストールを自動化
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;;環境を日本語にする
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)
		      
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
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;;カラーテーマ
;;(load-theme 'adwaita')

;;(cond (window-system
;;       (setq x-select-enable-clipboard t)
;;       ))

(setq x-select-enable-clipboard t)

;;文字自体をコメントアウト
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))

(global-set-key (kbd "M-;") 'one-line-comment)


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

;; "C-t"でウィンドウ切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
