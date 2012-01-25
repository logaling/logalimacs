# logalimacsのインストール方法,設定
## 1. logaling-commandをインストール(Ruby 1.9専用)
Ruby1.9を既にインストールしている場合は、次の項に飛ばして下さい。

システムからruby1.9を使っている場合:

    % sudo gem install logaling-command

rvmを使っている場合:
(ruby1.9をインストールしていなかったら -> % rvm install 1.9.2)

    % gem install logaling-command

rvmをEmacsで使う場合の設定
[rvm.el](https://github.com/senny/rvm.el)が必要です。
端末で先に % rvm use 1.9.2 --defaultでデフォルトを1.9系にして、
以下のコードを.emacs等に書きます。

---

    (require 'rvm)
    (rvm-use-default)
    
---

## 2. Emacs24でのインストール方法
Emacs24でない方は、次の項に飛ばして下さい。

Emacs24では標準でパッケージインストーラ(package.el)が入っているので、
以下の様にする事で、インストール可能です。  
marmaladeの設定をしていない場合(Emacs24用):
以下の設定を.emacs等の設定ファイルに書く事でmarmaladeからElispをインストールできるようになります。既に設定している方は飛ばして"1. M-x list~"の所を見て下さい。

    (require 'package)
    (add-to-list 'package-archives
        '("marmalade" . "http://marmalade-repo.org/packages/"))
    ;;パッケージインストールするディレクトリを指定(任意)
    ;;(setq package-user-dir "~/.emacs.d/elpa")
    ;;インストールしたパッケージにロードパスを通してロードする
    (package-initialize)


1. "M-x list-packages"とタイプする。
2. インストール可能なパッケージが表示されるので、
その中から、logalimacsを探します。
3. logalimacsのパッケージ名の先頭行で"i"をタイプするとマークできます。
他にインストールするものがなければ、"x"でインストールを開始します。
4. 必要な設定は、自動で設定されますが、自分でカスタマイズしたい場合、  
.emacsへの設定の項を変更する事で自分好みにカスタマイズする時の参考になると思います。

## 3. それ以外の場合のインストール方法

Emacs24以外の方はgitが使用可能であれば、下記のコマンドでダウンロード可能です。
(Emacs23でも、package.elを入れればinstall可能と思いましたが、
私が試した所、パッケージインストール中にエラーが出た為、こちらの方法をお勧めします。)


    % cd YOUR-CLONE-DIRECTORY
    % git clone https://yuutayamada@github.com/logaling/logalimacs.git


ダウンロードしたら、./logalimacs/の中のlogalimacs.elを、
あなたのパッケージを管理している所へ写すか、インストールした場所へパスを通してください(わからなければ雑多な設定の項へ)。その後に.emacsへの設定の項へ進んで下さい。

logalimacsはpopwinを利用するとより便利になります。  
もし興味があれば、popwin.el用の便利な設定を試して見て下さい。

## .emacsへの設定
あなたの設定用の.emacsへ(~/.emacs.d/init.elでもいいですし、
他にload関数で読み込んだ所でもいいです)以下のように書込みます。
これで、logalimacsを利用できるようになります。

注意1:もしエラーが出るのであれば、閉じ括弧後ろでC-x C-e(又は、M-x eval last sexp)をタイプする事で、その行を評価でき、行単位でのチェックできます。  
注意2:キーバインド(kbd "ここの部分")は、あなたが使いやすい所に設定して下さい。


---

    ;;; loga-fly-mode用の秒単位の設定
    ;; "0.5"なども設定可能です。
    (setq loga-fly-mode-interval "1")

---

    ;;; keybinds
    ;; Emacs起動時から動作させる場合
    (when (require 'logalimacs nil t)
      (global-set-key (kbd "M-g M-i") 'loga-interactive-command)
      (global-set-key (kbd "M-g M-l") 'loga-lookup-in-hand-or-region)
      (global-set-key (kbd "M-g M-a") 'loga-add-word))

    ;;又は

    ;; コマンド実行時に読み込み
    (autoload 'loga-interactive-command "logalimacs")
    (autoload 'loga-lookup-in-hand-or-region "logalimacs")
    (autoload 'loga-add-word "logalimacs")
    (global-set-key (kbd "M-g M-i") 'loga-interactive-command)
    (global-set-key (kbd "M-g M-l") 'loga-lookup-in-hand-or-region)
    (global-set-key (kbd "M-g M-a") 'loga-loga-add-word)

    
---

## popwin.el用の便利な設定

注意:この設定を利用する為には、
[_popwin.el_](http://www.emacswiki.org/emacs/PopWin)が必要です。
Emacs24経由でlogalimacsをインストールした場合は、自動で設定されます。

    (require 'popwin)
    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:special-display-config
          (append '(
                ("*logalimacs*" :position bottom :height 10 :noselect t :stick t)
                ;;if need to other configuration, add for like below:
                ;("*Backtrace*")
                )
              popwin:special-display-config))

## 雑多な設定
logalimacsを設定する上でのEmacs初心者が躓きそうな所を、フォローするような事を書こうと思います。  
(初心者に毛が生えた程度の拙い説明かもしれませんが、ご容赦願います。)  

---
.emacs:  
emacs用の設定ファイルで、通常は、~/.emacs.d/init.el又は、  
~/.emacs(昔はこれでしたが今は.emacs.d/init.elに書くのがナウイようです)になります
もし、設定ファイルを分割したいと思ったら、init.elに下の様に書きます。

    (load "ディレクトリを含めた設定したいファイルパス")
    ;;~/.emacs.d/以下に設定ファイルを追加したいなら、
    (load "~/.emacs.d/logalimacs_config")

この二つ目の例は、~/.emacs.d/以下のlogalimacs_config.el又は、logalimacs.elc
を読むようにしています。

---
ロードパスを追加するには:  
add-to-list関数を使います。
以下をあなたの.emacsに設定します。

    ;;"~/.emacs.d/package/hoge/以下にパスを通す場合
    (add-to-list 'load-path "~/.emacs.d/package/hoge")
