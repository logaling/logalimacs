# logalimacsのインストール方法
## Emacs24でのインストール方法

Emacs24では標準でパッケージインストーラ(package.el)が入っているので、
以下の様にする事で、インストール可能です。

1. "M-x list-packages"とタイプする。
2. インストール可能なパッケージが表示されるので、
その中から、logalimacsを探します。
3. logalimacsのパッケージ名の上で(パッケージ名はハイライトされています)、
RETしインストール用のバッファが開くので、そこのinstallという色の違うボタンに
カーソルを合わせ、RETします。
4. 自動でインストールされるので、.emacsへの設定の項へ進んで下さい。


## それ以外の場合のインストール方法

Emacs24以外の方はgitが使用可能であれば、下記のコマンドでダウンロード可能です。
(Emacs23でも、package.elを入れればinstall可能と思いましたが、
私が試した所、パッケージインストール中にエラーが出た為、こちらの方法をお勧めします。)


    % cd YOUR-CLONE-DIRECTORY
    % git clone https://yuutayamada@github.com/logaling/logalimacs.git


ダウンロードしたら、./logalimacs/の中のlogalimacs.elを、
あなたのパッケージを管理している所へ写すか、インストールした場所へパスを通してください(わからなければ雑多な設定の項へ)。その後に.emacsへの設定の項へ進んで下さい。

## .emacsへの設定
あなたの設定用の.emacsへ(~/.emacs.d/init.elでもいいし、他にload関数で読み込んだ所でもいい)以下のように書込みます。
これで、logalimacsを利用できるようになりましたが、popwinを利用するとより便利になります。もし興味があれば、popwin.el用の便利な設定を試して見て下さい。

注意1:もしエラーが出るのであれば、閉じ括弧後ろでC-x C-e(又は、M-x eval last sexp)をタイプする事で、その行を評価でき、行単位でのチェックできます。  
注意2:キーバインド(kbd "ここの部分")は、あなたが使いやすい所に設定して下さい。

---

    ;;the second section example
    (when (require 'logalimacs nil t)
      (global-set-key (kbd "M-g M-i") 'loga-interactive-command)
      (global-set-key (kbd "M-g M-l") 'loga-lookup-in-hand-or-region)
      (global-set-key (kbd "M-g M-a") 'loga-add-word))

    ;;or, use only interactive-command
    (autoload 'loga-interactive-command "logalimacs"
      "front-end for logaling-command")
    (global-set-key (kbd "M-g M-i") 'loga-interactive-command)

---

## popwin.el用の便利な設定



注意:この設定を利用する為には、[_popwin.el_](http://www.emacswiki.org/emacs/PopWin)が必要です。

    (require 'popwin)
    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:special-display-config
          (append '(
                ("*logalimacs*" :position bottom :height 10 :noselect t :stick t)
                ;;if need to other configuration, add for like below:
                ;("*Backtrace*")
                )
              popwin:special-display-config))

    (setq popwin:popup-window-height 15   ;default 15. if left or right, ignored
          popwin:popup-window-width 30    ;default 30. if top or bottom, ignored
          )

## 雑多な設定
logalimacsを設定する上でのEmacs初心者が躓きそうな所を、フォローするような事を書こうと思います。  
(初心者に毛が生えた程度の拙い説明かもしれませんが、ご容赦願います。)  

* .emacsって:  
emacs用の設定ファイルで、通常は、~/.emacs.d/init.el又は、  
~/.emacs(昔はこれでしたが今は.emacs.d/init.elに書くのがナウイようです)になります
もし、設定ファイルを分割したいと思ったら、init.elに下の様に書きます。

    (load "ディレクトリを含めた設定したいファイルパス")
    ;;~/.emacs.d/以下に設定ファイルを追加したいなら、
    (load "~/.emacs.d/logalimacs_config")

この二つ目の例は、~/.emacs.d/以下のlogalimacs_config.el又は、logalimacs.elc
を読むようにしています。

* ロードパスを追加するには:  
add-to-list関数を使います。
以下をあなたの.emacsに設定します。
(add-to-list 'load-path pkg-dir)
